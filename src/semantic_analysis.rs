use crate::ast::*;
use crate::errors::SourceError;
use crate::lexer::SourceInfo;

pub type SemAnalysisRes = Result<(), SourceError>;

pub struct SemanticAnalizer {
  loop_depth: u32,
  function_depth: u32,
}

impl SemanticAnalizer {
  pub fn analyze_stmt(&mut self, ast: &AST, stmt: StmtHandle) -> SemAnalysisRes {
    match ast.get_statement(stmt) {
      Stmt::VarDecl {
        identifier,
        id_info,
        expression,
      } => {
        if let Some(Expr::Variable { id, id_info }) =
          expression.and_then(|handle| Some(ast.get_expression(handle)))
        {
          if id == identifier {
            return Err(SourceError::from_token_info(
              &ast.get_source_info(id_info),
              "cannot initialize identifier with itself".to_string(),
              crate::errors::SourceErrorType::Parsing,
            ));
          }
        }
      }
      Stmt::While {
        info,
        condition,
        loop_body,
      } => {
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
        id,
        name_info,
        parameters,
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
          return Err(SourceError::from_token_info(
            &ast.get_source_info(info),
            "cannot have break outside of loop body".to_string(),
            crate::errors::SourceErrorType::Parsing,
          ));
        }
      }
      Stmt::Return(_) => {
        if self.function_depth == 0 {
          return Err(SourceError::from_token_info(
            &SourceInfo::temporary(),
            "cannot have break outside of loop body".to_string(),
            crate::errors::SourceErrorType::Parsing,
          ));
        }
      }
      _ => {}
    }
    Ok(())
  }

  pub fn analyze_expr(ast: &AST, expr: ExprHandle) {}

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