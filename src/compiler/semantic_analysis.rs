use super::{ast::*, error_from_source_info};
use crate::errors::{SourceError, SourceErrorType};

pub type SemAnalysisRes = Result<(), SourceError>;

pub struct SemanticAnalizer {
  loop_depth: u32,
  function_depth: u32,
}

impl SemanticAnalizer {
  fn analyze_var_decl(
    &mut self,
    ast: &AST,
    identifier: Identifier,
    expression: Option<ExprHandle>,
  ) -> SemAnalysisRes {
    if let Some(Expr::Variable { id, id_info }) =
      expression.clone().map(|handle| ast.get_expression(handle))
    {
      if id == identifier {
        return Err(error_from_source_info(
          &ast.get_source_info(id_info),
          "cannot initialize identifier with itself".to_string(),
        ));
      }
    }
    if let Some(expr) = expression {
      self.analyze_expr(ast, expr)?;
    }
    Ok(())
  }

  pub fn analyze_stmt(&mut self, ast: &AST, stmt: StmtHandle) -> SemAnalysisRes {
    match ast.get_statement(stmt) {
      Stmt::VarDecl {
        identifier,
        id_info: _,
        expression,
      } => self.analyze_var_decl(ast, identifier, expression)?,
      Stmt::While {
        info: _,
        condition: _,
        loop_body,
      } => {
        self.loop_depth += 1;
        self.analyze_stmt(ast, loop_body)?;
        self.loop_depth -= 1;
      }
      Stmt::IfBranch {
        if_info: _,
        condition: _,
        true_branch,
        else_branch,
      } => {
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
            "cannot have return outside of loop body".to_string(),
          ));
        }
      }
      _ => {}
    }
    Ok(())
  }

  pub fn analyze_expr(&mut self, ast: &AST, expr: ExprHandle) -> SemAnalysisRes {
    match ast.get_expression(expr) {
      Expr::Closure { parameters, body } => {
        self.function_depth += 1;
        for s in body {
          self.analyze_stmt(ast, s)?;
        }
        self.function_depth -= 1;
      }
      _ => {}
    }
    Ok(())
  }

  pub fn analyze(ast: &AST) -> SemAnalysisRes {
    let mut analizer = Self {
      loop_depth: 0,
      function_depth: 0,
    };
    for stmt in ast.get_program() {
      analizer.analyze_stmt(ast, stmt.clone())?;
    }
    Ok(())
  }
}
