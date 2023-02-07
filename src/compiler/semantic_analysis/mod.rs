use super::{ast::*, error_from_source_info};
use crate::errors::{SourceError, SourceInfo};

pub type StmtAnalisysRes = Result<(), SourceError>;
type ExprAnalisysRes = Result<Type, SourceError>;

mod type_checker;

use type_checker::TypeChecker;

pub struct SemanticAnalizer {
  loop_depth: u32,
  function_depth: u32,
  type_check: TypeChecker,
}

impl SemanticAnalizer {
  fn check_self_assignment(
    &mut self,
    ast: &AST,
    identifier: Identifier,
    expression: Option<ExprHandle>,
  ) -> StmtAnalisysRes {
    if let Some(Expr::Variable { id, id_info }) =
      expression.map(|handle| ast.get_expression(handle))
    {
      if id == identifier {
        return Err(error_from_source_info(
          &ast.get_source_info(id_info),
          "cannot initialize identifier with itself".to_string(),
        ));
      }
    }
    Ok(())
  }

  fn check_valid_condition_type(info: SourceInfo, condition_type: Type) -> StmtAnalisysRes {
    if condition_type != Type::Bool {
      Err(error_from_source_info(
        &info,
        format!("cannot use value of type {condition_type:?} in a condition"),
      ))
    } else {
      Ok(())
    }
  }

  fn check_var_decl(
    &mut self,
    ast: &AST,
    identifier: Identifier,
    id_info: SourceInfoHandle,
    expression: Option<ExprHandle>,
    declared_type: Type,
  ) -> StmtAnalisysRes {
    self.check_self_assignment(ast, identifier, expression.clone())?;
    let var_type = if let Some(expr) = expression {
      let rhs = self.analyze_expr(ast, expr)?;
      if declared_type != rhs && declared_type != Type::Any {
        return Err(error_from_source_info(
          &ast.get_source_info(id_info),
          format!("cannot assign expression of type {rhs:?} to type {declared_type:?}"),
        ));
      }
      rhs
    } else {
      declared_type
    };
    self
      .type_check
      .set_type_or_err(identifier, var_type, ast.get_source_info(id_info))?;
    Ok(())
  }

  pub fn analyze_stmt(&mut self, ast: &AST, stmt: StmtHandle) -> StmtAnalisysRes {
    match ast.get_statement(stmt) {
      Stmt::VarDecl {
        identifier,
        id_info,
        var_type,
        expression,
      } => self.check_var_decl(ast, identifier, id_info, expression, var_type)?,
      Stmt::While {
        info,
        condition,
        loop_body,
      } => {
        Self::check_valid_condition_type(
          ast.get_source_info(info),
          self.analyze_expr(ast, condition)?,
        )?;
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
        Self::check_valid_condition_type(
          ast.get_source_info(if_info),
          self.analyze_expr(ast, condition)?,
        )?;
        self.analyze_stmt(ast, true_branch)?;
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
        id: _,
        name_info: _,
        parameters: _,
        body,
        ..
      } => {
        self.function_depth += 1;
        for s in body {
          self.analyze_stmt(ast, s)?;
        }
        self.function_depth -= 1;
      }
      Stmt::Break(info) => {
        if self.loop_depth == 0 {
          return Err(error_from_source_info(
            &ast.get_source_info(info),
            "cannot have break outside of loop body".to_string(),
          ));
        }
      }
      Stmt::Return { expr: _, src_info } => {
        if self.function_depth == 0 {
          return Err(error_from_source_info(
            &ast.get_source_info(src_info),
            "cannot have return outside of function body".to_string(),
          ));
        }
      }
      _ => {}
    }
    Ok(())
  }

  pub fn analyze_expr(&mut self, ast: &AST, expr: ExprHandle) -> ExprAnalisysRes {
    match ast.get_expression(expr) {
      Expr::Closure {
        parameters, body, ..
      } => {
        self.function_depth += 1;
        for s in body {
          self.analyze_stmt(ast, s)?;
        }
        self.function_depth -= 1;
        Ok(Type::Function(Vec::new()))
      }
      Expr::Assignment { id, id_info, value } => {
        let value_type = self.analyze_expr(ast, value)?;
        self
          .type_check
          .set_type_or_err(id, value_type.clone(), ast.get_source_info(id_info))?;
        Ok(value_type)
      }
      Expr::Variable { id, id_info } => Ok(self.type_check.get_type(id)),
      Expr::Literal { literal, info } => Ok(Type::from_literal(literal)),
      _ => Ok(Type::Undefined),
    }
  }

  pub fn analyze(ast: &AST) -> Result<(), SourceError> {
    let mut analizer = Self {
      loop_depth: 0,
      function_depth: 0,
      type_check: TypeChecker::new(),
    };
    for stmt in ast.get_program() {
      analizer.analyze_stmt(ast, stmt.clone())?;
    }
    Ok(())
  }
}
