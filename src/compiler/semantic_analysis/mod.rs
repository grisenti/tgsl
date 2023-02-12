use std::{
  any,
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

const ARITHMETIC_OPERATORS: [Operator; 4] = [
  Operator::Basic('+'),
  Operator::Basic('-'),
  Operator::Basic('*'),
  Operator::Basic('/'),
];
const COMP_OPERATORS: [Operator; 5] = [
  Operator::Basic('<'),
  Operator::Basic('>'),
  Operator::Leq,
  Operator::Geq,
  Operator::Same,
];
const LOGICAL_OPERATORS: [Operator; 2] = [Operator::Or, Operator::And];

enum AnalyzerState {
  Function(SourceInfoHandle),
  Loop,
}

pub struct SemanticAnalizer {
  state: Vec<AnalyzerState>,
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

  fn equal_types(&self, value: &Type, specifier: &Type) -> bool {
    match (value, specifier) {
      (Type::Function(id), Type::FunctionType(specifier_types)) => {
        let Function {
          fn_types: value_types,
          ..
        } = self.functions.get(&id).unwrap();
        dbg!(value_types);
        if value_types.len() != specifier_types.len() {
          false
        } else {
          for (a, b) in value_types.iter().zip(specifier_types) {
            if !self.equal_types(a, b) || *b == Type::Any {
              return false;
            }
          }
          true
        }
      }
      (_, Type::Any) => true,
      //this is for lambda parameters. Should maybe make a distinction between value types and parameter types
      (Type::Any, _) => true,
      (Type::FunctionType(_), _) => panic!(),
      (a, b) => a == b,
    }
  }

  fn get_struct_member(
    &mut self,
    struct_id: Identifier,
    name: StrHandle,
    ast: &AST,
  ) -> Option<Type> {
    let s = self.structs[&struct_id].clone();
    let name = name.get(ast);
    if let Some((_, member_type)) = s
      .member_names
      .iter()
      .zip(s.member_types)
      .find(|(member_name, _)| member_name.get(ast) == name)
    {
      Some(member_type)
    } else {
      None
    }
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
        other if *other != value_type => {
          self.errors.push(error_from_source_info(
            &name_info,
            format!("cannot assign value of type {other:?} to identifier of type {value_type:?}"),
          ));
        }
        _ => {}
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
    lhs_id: Identifier,
    expression: Option<ExprHandle>,
  ) {
    if let Some(Expr::Variable {
      id: rhs_id,
      id_info,
    }) = expression.map(|handle| handle.get(ast))
    {
      if rhs_id == lhs_id {
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
    // TODO: think of something smarter
    if self.function_depth > 100 {
      self.emit_error(SourceError::from_source_info(
        &SourceInfo::temporary(),
        "function instantiation limit reached".to_string(),
        crate::errors::SourceErrorType::Compilation,
      ));
      return Type::Error;
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
      if !self.equal_types(&arg, &param_type) {
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
      if let Some(ret) = self.analyze_stmt(ast, stmt) {
        if return_type == Type::Undefined {
          return_type = ret;
        } else if return_type != ret {
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
    self.check_self_assignment(ast, identifier, expression);
    let var_type = if let Some(expr) = expression {
      let rhs = self.analyze_expr(ast, expr);
      if !self.equal_types(&rhs, &declared_type) {
        self.emit_error(error_from_source_info(
          &id_info.get(ast),
          format!("cannot assign expression of type {rhs:?} to type {declared_type:?}"),
        ));
      }
      rhs
    } else {
      declared_type
    };
    self.set_type(identifier, var_type);
  }

  pub fn check_return_types(&mut self, ast: &AST, returns: Vec<Option<Type>>) -> Option<Type> {
    let mut ret_types = returns.iter().filter_map(|v| v.as_ref());
    if let Some(first) = ret_types.next() {
      if ret_types.all(|ret| self.equal_types(ret, first)) {
        Some(first.clone())
      } else if let Some(AnalyzerState::Function(info)) = self.state.last() {
        self.emit_error(error_from_source_info(
          &info.get(ast),
          "inconsistent return types in function".to_string(),
        ));
        Some(Type::Error)
      } else {
        panic!();
      }
    } else {
      None
    }
  }

  pub fn analyze_stmt(&mut self, ast: &AST, stmt: StmtHandle) -> Option<Type> {
    match stmt.clone().get(ast) {
      Stmt::VarDecl {
        identifier,
        id_info,
        var_type,
        expression,
      } => {
        self.check_var_decl(ast, identifier, id_info, expression, var_type);
        None
      }
      Stmt::While {
        info,
        condition,
        loop_body,
      } => {
        let condition_type = self.analyze_expr(ast, condition);
        self.check_valid_condition_type(info.get(ast), condition_type);
        self.state.push(AnalyzerState::Loop);
        let ret = self.analyze_stmt(ast, loop_body);
        self.state.pop();
        ret
      }
      Stmt::Struct {
        name,
        name_info: _,
        member_names,
        member_types,
      } => {
        self.structs.insert(
          name,
          Struct {
            member_names,
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
        self.set_type(name, Type::Function(name));
        None
      }
      Stmt::IfBranch {
        if_info,
        condition,
        true_branch,
        else_branch,
      } => {
        let condition_type = self.analyze_expr(ast, condition);
        self.check_valid_condition_type(if_info.get(ast), condition_type);
        let ret_true = self.analyze_stmt(ast, true_branch);
        let ret_false = if let Some(stmt) = else_branch {
          self.analyze_stmt(ast, stmt)
        } else {
          None
        };
        self.check_return_types(ast, vec![ret_true, ret_false])
      }
      Stmt::Block(stmts) => {
        let mut return_types = Vec::new();
        for s in stmts {
          return_types.push(self.analyze_stmt(ast, s))
        }
        self.check_return_types(ast, return_types)
      }
      Stmt::Function {
        id,
        name_info: _,
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
        None
      }
      Stmt::Break(info) => {
        if !matches!(self.state.last(), Some(AnalyzerState::Loop)) {
          self.emit_error(error_from_source_info(
            &info.get(ast),
            "cannot have break outside of loop body".to_string(),
          ));
        }
        None
      }
      Stmt::Return { expr, src_info } => {
        if let Some(AnalyzerState::Function(_)) = self.state.last() {
          Some(self.analyze_expr(ast, expr))
        } else {
          self.emit_error(error_from_source_info(
            &src_info.get(ast),
            "cannot have return outside of function body".to_string(),
          ));
          None
        }
      }
      Stmt::Print(expr) | Stmt::Expr(expr) => {
        self.analyze_expr(ast, expr);
        None
      }
    }
  }

  fn dot_call(
    &mut self,
    ast: &AST,
    id: Identifier,
    lhs: Type,
    name_info: SourceInfoHandle,
    name: StrHandle,
  ) -> Type {
    if self.functions.contains_key(&id) {
      Type::PartialCall {
        func_id: id,
        partial_arguments: vec![lhs],
      }
    } else {
      self.emit_error(error_from_source_info(
        &name_info.get(ast),
        format!(
          "identifier {} is neither a struct member nor a function",
          name.get(ast)
        ),
      ));
      Type::Error
    }
  }

  fn binary_operation(
    &mut self,
    ast: &AST,
    lhs: ExprHandle,
    op: OperatorPair,
    rhs: ExprHandle,
  ) -> Type {
    let lhs = self.analyze_expr(ast, lhs);
    let rhs = self.analyze_expr(ast, rhs);
    let OperatorPair { op, src_info } = op;
    match (lhs, op, rhs) {
      (Type::Num, bin_op, Type::Num) if ARITHMETIC_OPERATORS.contains(&bin_op) => Type::Num,
      (Type::Str, Operator::Basic('+'), Type::Str) => Type::Str,
      (Type::Num, comp_op, Type::Num) if COMP_OPERATORS.contains(&comp_op) => Type::Bool,
      (Type::Str, comp_op, Type::Str) if COMP_OPERATORS.contains(&comp_op) => Type::Bool,
      (Type::Bool, logical_op, Type::Bool) if LOGICAL_OPERATORS.contains(&logical_op) => Type::Bool,
      (lhs, op, rhs) => {
        self.emit_error(error_from_source_info(
          &src_info.get(ast),
          format!("cannot apply operator {op:?} to operands {lhs:?} and {rhs:?}"),
        ));
        Type::Error
      }
    }
  }

  fn unary_operation(&mut self, ast: &AST, op: OperatorPair, rhs: ExprHandle) -> Type {
    let rhs = self.analyze_expr(ast, rhs);
    let OperatorPair { op, src_info } = op;
    match (op, rhs) {
      (Operator::Basic('-'), Type::Num) => Type::Num,
      (Operator::Basic('!'), Type::Bool) => Type::Bool,
      (op, rhs) => {
        self.emit_error(error_from_source_info(
          &src_info.get(ast),
          format!("cannot apply operator {op:?} to operand {rhs:?}"),
        ));
        Type::Error
      }
    }
  }

  pub fn analyze_expr(&mut self, ast: &AST, expr: ExprHandle) -> Type {
    match expr.clone().get(ast) {
      Expr::Closure {
        id,
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
        Type::Function(id)
      }
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
        self.state.push(AnalyzerState::Function(call_info));
        self.function_depth += 1;
        let func = self.analyze_expr(ast, func);
        let ret = match func {
          Type::Function(id) => {
            let args = arguments
              .iter()
              .map(|arg| self.analyze_expr(ast, *arg))
              .collect();
            self.call_function(ast, id, args, call_info)
          }
          Type::PartialCall {
            func_id,
            mut partial_arguments,
          } => {
            partial_arguments.extend(arguments.iter().map(|arg| self.analyze_expr(ast, *arg)));
            self.call_function(ast, func_id, partial_arguments, call_info)
          }
          _ => {
            self.emit_error(error_from_source_info(
              &call_info.get(ast),
              format!("cannot call type {func:?}"),
            ));
            Type::Error
          }
        };
        self.function_depth -= 1;
        self.state.pop();
        ret
      }
      Expr::Binary {
        left,
        operator,
        right,
      } => self.binary_operation(ast, left, operator, right),
      Expr::Unary { operator, right } => self.unary_operation(ast, operator, right),
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
            self.dot_call(ast, identifier, left, name_info, name)
          }
        } else {
          self.dot_call(ast, identifier, left, name_info, name)
        }
      }
      Expr::Set {
        object,
        name,
        name_info,
        value,
      } => {
        let lhs = self.analyze_expr(ast, object);
        let rhs = self.analyze_expr(ast, value);
        if let Type::Struct(id) = lhs {
          if let Some(member_type) = self.get_struct_member(id, name, ast) {
            if self.equal_types(&rhs, &member_type) {
              member_type
            } else {
              self.emit_error(error_from_source_info(
                &name_info.get(ast),
                format!(
                  "member '{}' is of type {member_type:?}, cannot assing value of type {rhs:?}",
                  name.get(ast)
                ),
              ));
              Type::Error
            }
          } else {
            self.emit_error(error_from_source_info(
              &name_info.get(ast),
              format!("{} is not a member of type {lhs:?}", name.get(ast)),
            ));
            Type::Error
          }
        } else {
          self.emit_error(error_from_source_info(
            &name_info.get(ast),
            format!("cannot set propriety for type {lhs:?}"),
          ));
          Type::Error
        }
      }
    }
  }

  pub fn analyze(ast: &AST) -> Result<(), SourceError> {
    let mut analizer = Self {
      state: Vec::new(),
      function_depth: 0,
      structs: HashMap::new(),
      functions: HashMap::new(),
      instantiated_functions: HashMap::new(),
      errors: Vec::new(),
      type_map: HashMap::new(),
    };
    for stmt in ast.get_program() {
      analizer.analyze_stmt(ast, *stmt);
    }
    if analizer.errors.is_empty() {
      Ok(())
    } else {
      Err(SourceError::from_err_vec(analizer.errors))
    }
  }
}
