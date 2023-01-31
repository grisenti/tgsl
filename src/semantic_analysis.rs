use std::collections::HashSet;

use crate::{ast::AST, errors::SourceError};

struct SemanticAnalyzer<'ast> {
  ast: &'ast AST,
  env: Vec<HashSet<&'ast str>>,
}

impl<'ast> SemanticAnalyzer<'ast> {
  fn analyze_stmt(&mut self) {}
  fn analyze_expr(&mut self) {}

  fn analyze(ast: &'ast AST) -> Result<(), SourceError> {
    {
      let analyzer = Self {
        ast,
        env: Vec::new(),
      };
      let errors = Vec::new();
    }
    Ok(())
  }
}
