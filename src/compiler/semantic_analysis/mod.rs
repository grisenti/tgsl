use core::panic;
use std::{
  collections::{hash_map::Entry, HashMap},
  hash::Hash,
};

use super::{ast::*, error_from_source_info};
use crate::errors::{SourceError, SourceInfo};

#[derive(Clone)]
struct Function {
  paramenter_ids: Vec<Identifier>,
  fn_types: Vec<Type>,
  body: Vec<StmtHandle>,
}

#[derive(Clone)]
struct Struct {
  member_names: Vec<StrHandle>,
  member_types: Vec<Type>,
}

#[derive(Hash, PartialEq, Eq)]
struct InstancedFunction {
  id: Identifier,
  parameter_types: Vec<Type>,
}

type TypeMap = HashMap<Identifier, Type>;
type StructMap = HashMap<Identifier, Struct>;
type FunctionMap = HashMap<Identifier, Function>;
// maps instantiated functions to return types
type InstantiatedFunctions = HashMap<InstancedFunction, Type>;

pub struct SemanticAnalizer {
  loop_depth: u32,
  function_depth: u32,
  structs: StructMap,
  functions: FunctionMap,
  instantiated_functions: InstantiatedFunctions,
  type_map: TypeMap,
  errors: Vec<SourceError>,
}

impl SemanticAnalizer {
  fn emit_error(&mut self, err: SourceError) {
    self.errors.push(err);
  }

  fn get_struct_member(
    &mut self,
    struct_id: Identifier,
    name: StrHandle,
    ast: &AST,
  ) -> Option<Type> {
    todo!()
  }

  pub fn set_type(&mut self, id: Identifier, value: Type) {
    self.type_map.insert(id, value);
  }

  pub fn set_type_or_err(&mut self, id: Identifier, value_type: Type, name_info: SourceInfo) {
    match self.type_map.entry(id) {
      Entry::Occupied(mut e) => match e.get() {
        Type::Any => {
          e.insert(value_type);
        }
        Type::Error => {}
        other => {
          self.errors.push(error_from_source_info(
            &name_info,
            format!("cannot assign value of type {other:?} to identifier of type {value_type:?}"),
          ));
        }
      },
      Entry::Vacant(v) => {
        v.insert(value_type);
      }
    }
  }

  pub fn get_type(&self, id: Identifier) -> Type {
    self.type_map.get(&id).unwrap().clone()
  }

  fn check_self_assignment(
    &mut self,
    ast: &AST,
    identifier: Identifier,
    expression: Option<ExprHandle>,
  ) {
    if let Some(Expr::Variable { id, id_info }) = expression.map(|handle| handle.get(ast)) {
      if id == identifier {
        self.emit_error(error_from_source_info(
          &id_info.get(ast),
          "cannot initialize identifier with itself".to_string(),
        ));
      }
    }
  }

  fn check_valid_condition_type(&mut self, info: SourceInfo, condition_type: Type) {
    if condition_type != Type::Bool {
      self.emit_error(error_from_source_info(
        &info,
        format!("cannot use value of type {condition_type:?} in a condition"),
      ))
    }
  }

  fn call_function(
    &mut self,
    ast: &AST,
    id: Identifier,
    args: Vec<Type>,
    call_info: SourceInfoHandle,
  ) -> Type {
    if let Some(ret_val) = self.instantiated_functions.get(&InstancedFunction {
      id,
      parameter_types: args.clone(),
    }) {
      return ret_val.clone();
    }
    println!("instancing function {id:?}");
    let Function {
      paramenter_ids,
      fn_types,
      body,
    } = self.functions.get(&id).cloned().unwrap();
    if args.len() != paramenter_ids.len() {
      self.emit_error(error_from_source_info(
        &call_info.get(ast),
        format!(
          "incorrect number of arguments for function call (required {}, provided {})",
          paramenter_ids.len(),
          args.len()
        ),
      ));
      return Type::Error;
    }
    for (arg_num, ((param_type, param_id), arg)) in fn_types
      .into_iter()
      .zip(paramenter_ids)
      .zip(args.clone())
      .enumerate()
    {
      if arg != param_type && param_type != Type::Any {
        self.emit_error(error_from_source_info(
          &call_info.get(ast),
          format!(
            "mismatched types. Argument {} should be of type {param_type:?}",
            arg_num + 1
          ),
        ));
        return Type::Error;
      } else {
        self.set_type(param_id, arg)
      }
    }
    let mut return_type = Type::Undefined;
    for stmt in body {
      if let Some(ret) = self.analyze_stmt(ast, stmt.clone()) {
        if return_type == Type::Undefined || return_type == ret {
          return_type = ret;
        } else {
          self.emit_error(error_from_source_info(
            &call_info.get(ast),
            "inconsistent return types".to_string(),
          ))
        }
      }
    }
    self.instantiated_functions.insert(
      InstancedFunction {
        id,
        parameter_types: args,
      },
      return_type.clone(),
    );
    return_type
  }

  fn check_var_decl(
    &mut self,
    ast: &AST,
    identifier: Identifier,
    id_info: SourceInfoHandle,
    expression: Option<ExprHandle>,
    declared_type: Type,
  ) {
    self.check_self_assignment(ast, identifier, expression.clone());
    let var_type = if let Some(expr) = expression {
      let rhs = self.analyze_expr(ast, expr);
      if declared_type != rhs && declared_type != Type::Any {
        self.emit_error(error_from_source_info(
          &id_info.get(ast),
          format!("cannot assign expression of type {rhs:?} to type {declared_type:?}"),
        ));
      }
      rhs
    } else {
      declared_type
    };
    self.set_type_or_err(identifier, var_type, id_info.get(ast));
  }

  pub fn analyze_stmt(&mut self, ast: &AST, stmt: StmtHandle) -> Option<Type> {
    match stmt.clone().get(ast) {
      Stmt::VarDecl {
        identifier,
        id_info,
        var_type,
        expression,
      } => self.check_var_decl(ast, identifier, id_info, expression, var_type),
      Stmt::While {
        info,
        condition,
        loop_body,
      } => {
        let condition_type = self.analyze_expr(ast, condition);
        self.check_valid_condition_type(info.get(ast), condition_type);
        self.loop_depth += 1;
        self.analyze_stmt(ast, loop_body)?;
        self.loop_depth -= 1;
      }
      Stmt::Struct {
        name,
        name_info,
        member_names,
        member_types,
      } => {
        self.structs.insert(
          name,
          Struct {
            member_names: member_names.clone(),
            member_types: member_types.clone(),
          },
        );
        self.instantiated_functions.insert(
          InstancedFunction {
            id: name,
            parameter_types: member_types,
          },
          Type::Struct(name),
        );
        self.set_type(name, Type::Function(name))
      }
      Stmt::IfBranch {
        if_info,
        condition,
        true_branch,
        else_branch,
      } => {
        let condition_type = self.analyze_expr(ast, condition);
        self.check_valid_condition_type(if_info.get(ast), condition_type);
        self.analyze_stmt(ast, true_branch);
        if let Some(stmt) = else_branch {
          self.analyze_stmt(ast, stmt)?;
        }
      }
      Stmt::Block(stmts) => {
        for s in stmts {
          self.analyze_stmt(ast, s)?;
        }
      }
      Stmt::Function {
        id,
        name_info,
        parameters,
        fn_type,
        body,
      } => {
        self.functions.insert(
          id,
          Function {
            paramenter_ids: parameters,
            fn_types: fn_type,
            body,
          },
        );
        self.set_type(id, Type::Function(id));
      }
      Stmt::Break(info) => {
        if self.loop_depth == 0 {
          self.emit_error(error_from_source_info(
            &info.get(ast),
            "cannot have break outside of loop body".to_string(),
          ));
        }
      }
      Stmt::Return { expr, src_info } => {
        if self.function_depth == 0 {
          self.emit_error(error_from_source_info(
            &src_info.get(ast),
            "cannot have return outside of function body".to_string(),
          ))
        }
        return Some(self.analyze_expr(ast, expr));
      }
      Stmt::Expr(expr) => {
        self.analyze_expr(ast, expr);
      }
      _ => {}
    }
    None
  }

  pub fn analyze_expr(&mut self, ast: &AST, expr: ExprHandle) -> Type {
    match expr.clone().get(ast) {
      Expr::Closure { .. } => Type::AnonymusFunction(expr),
      Expr::Assignment { id, id_info, value } => {
        let value_type = self.analyze_expr(ast, value);
        self.set_type_or_err(id, value_type.clone(), id_info.get(ast));
        value_type
      }
      Expr::Variable { id, .. } => self.get_type(id),
      Expr::Literal { literal, .. } => Type::from_literal(literal),
      Expr::FnCall {
        func,
        call_info,
        arguments,
      } => {
        self.function_depth += 1;
        let func = self.analyze_expr(ast, func);
        let args = arguments
          .iter()
          .map(|arg| self.analyze_expr(ast, arg.clone()))
          .collect();
        let ret = if let Type::Function(id) = func {
          self.call_function(ast, id, args, call_info)
        } else {
          self.emit_error(error_from_source_info(
            &call_info.get(ast),
            format!("cannot call type {func:?}"),
          ));
          Type::Error
        };
        self.function_depth -= 1;
        ret
      }
      Expr::Binary {
        left,
        operator,
        right,
      } => self.analyze_expr(ast, left),
      Expr::Unary { operator, right } => self.analyze_expr(ast, right),
      Expr::Dot {
        lhs,
        name,
        identifier,
        name_info,
      } => {
        let left = self.analyze_expr(ast, lhs);
        if let Type::Struct(id) = left {
          if let Some(member_type) = self.get_struct_member(id, name, ast) {
            member_type
          } else {
            panic!()
          }
        } else {
          panic!();
        }
      }
      _ => Type::Undefined,
    }
  }

  pub fn analyze(ast: &AST) -> Result<(), SourceError> {
    let mut analizer = Self {
      loop_depth: 0,
      function_depth: 0,
      structs: HashMap::new(),
      functions: HashMap::new(),
      instantiated_functions: HashMap::new(),
      errors: Vec::new(),
      type_map: HashMap::new(),
    };
    for stmt in ast.get_program() {
      analizer.analyze_stmt(ast, stmt.clone());
    }
    if analizer.errors.is_empty() {
      Ok(())
    } else {
      Err(SourceError::from_err_vec(analizer.errors))
    }
  }
}