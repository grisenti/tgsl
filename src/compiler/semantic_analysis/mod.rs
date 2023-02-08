use std::collections::{hash_map::Entry, HashMap};

use super::{ast::*, error_from_source_info};
use crate::errors::{SourceError, SourceInfo};

pub type StmtAnalisysRes = Result<Option<Type>, SourceError>;
type ExprAnalisysRes = Result<Type, SourceError>;

#[derive(Clone)]

struct Function {
  fn_type: Vec<Type>,
  body: ExprHandle,
}

type TypeMap = HashMap<Identifier, Type>;

pub struct SemanticAnalizer {
  loop_depth: u32,
  function_depth: u32,
  structs: HashMap<StructId, StmtHandle>,
  type_map: TypeMap,
  errors: Vec<SourceError>,
}

impl SemanticAnalizer {
  fn emit_error(&mut self, err: SourceError) {
    self.errors.push(err);
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
    if let Some(Expr::Variable { id, id_info }) =
      expression.map(|handle| ast.get_expression(handle))
    {
      if id == identifier {
        self.emit_error(error_from_source_info(
          &ast.get_source_info(id_info),
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
    fn_type: Vec<Type>,
    body: Vec<StmtHandle>,
    args: Vec<Type>,
    parameters: Vec<Identifier>,
    call_info: SourceInfoHandle,
  ) -> Type {
    if args.len() != parameters.len() {
      self.emit_error(error_from_source_info(
        &ast.get_source_info(call_info),
        format!(
          "incorrect number of arguments for function call (required {}, provided {})",
          parameters.len(),
          args.len()
        ),
      ))
    }
    for (arg_num, ((param_type, param_id), arg)) in
      fn_type.into_iter().zip(parameters).zip(args).enumerate()
    {
      if arg != param_type && param_type != Type::Any {
        self.emit_error(error_from_source_info(
          &ast.get_source_info(call_info),
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
        if return_type == Type::Undefined || return_type == ret {
          return_type = ret;
        } else {
          self.emit_error(error_from_source_info(
            &ast.get_source_info(call_info),
            "inconsistent return types".to_string(),
          ))
        }
      }
    }
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
          &ast.get_source_info(id_info),
          format!("cannot assign expression of type {rhs:?} to type {declared_type:?}"),
        ));
      }
      rhs
    } else {
      declared_type
    };
    self.set_type_or_err(identifier, var_type, ast.get_source_info(id_info));
  }

  pub fn analyze_stmt(&mut self, ast: &AST, stmt: StmtHandle) -> Option<Type> {
    match ast.get_statement(stmt.clone()) {
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
        self.check_valid_condition_type(ast.get_source_info(info), condition_type);
        self.loop_depth += 1;
        self.analyze_stmt(ast, loop_body)?;
        self.loop_depth -= 1;
      }
      Stmt::IfBranch {
        if_info,
        condition,
        true_branch,
        else_branch,
      } => {
        let condition_type = self.analyze_expr(ast, condition);
        self.check_valid_condition_type(ast.get_source_info(if_info), condition_type);
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
      Stmt::Function { id, .. } => {
        self.set_type(id, Type::NamedFunction(stmt));
      }
      Stmt::Break(info) => {
        if self.loop_depth == 0 {
          self.emit_error(error_from_source_info(
            &ast.get_source_info(info),
            "cannot have break outside of loop body".to_string(),
          ));
        }
      }
      Stmt::Return { expr, src_info } => {
        if self.function_depth == 0 {
          self.emit_error(error_from_source_info(
            &ast.get_source_info(src_info),
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
    match ast.get_expression(expr.clone()) {
      Expr::Closure { .. } => Type::AnonymusFunction(expr),
      Expr::Assignment { id, id_info, value } => {
        let value_type = self.analyze_expr(ast, value);
        self.set_type_or_err(id, value_type.clone(), ast.get_source_info(id_info));
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
        let ret = match func {
          Type::NamedFunction(stmt) => {
            if let Stmt::Function {
              fn_type,
              body,
              parameters,
              ..
            } = ast.get_statement(stmt)
            {
              self.call_function(ast, fn_type, body, args, parameters, call_info)
            } else {
              panic!();
            }
          }
          Type::AnonymusFunction(expr) => {
            if let Expr::Closure {
              body,
              fn_type,
              parameters,
            } = ast.get_expression(expr)
            {
              self.call_function(ast, fn_type, body, args, parameters, call_info)
            } else {
              panic!();
            }
          }
          _ => {
            self.emit_error(error_from_source_info(
              &ast.get_source_info(call_info),
              format!("cannot call type {func:?}"),
            ));
            Type::Undefined
          }
        };
        self.function_depth -= 1;
        ret
      }
      _ => Type::Undefined,
    }
  }

  pub fn analyze(ast: &AST) -> Result<(), SourceError> {
    let mut analizer = Self {
      loop_depth: 0,
      function_depth: 0,
      structs: HashMap::new(),
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
