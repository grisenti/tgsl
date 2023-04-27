use crate::errors::SourceInfo;

use super::{Expr, Stmt, AST};

macro_rules! generate_ast_handle {
  ($name:ident) => {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct $name {
      pub(super) index: u32,
    }

    impl $name {
      pub const INVALID: Self = Self { index: u32::MAX };

      pub(super) fn new(index: u32) -> Self {
        Self { index }
      }
    }
  };
}

generate_ast_handle!(ExprHandle);
impl ExprHandle {
  pub fn get<'a>(&'a self, ast: &'a AST) -> &'a Expr {
    assert!(self.index < ast.expressions.len() as u32);
    &ast.expressions[self.index as usize]
  }
}

generate_ast_handle!(StmtHandle);
impl StmtHandle {
  pub fn get<'a>(&'a self, ast: &'a AST) -> &'a Stmt {
    assert!(self.index < ast.statements.len() as u32);
    &ast.statements[self.index as usize]
  }
}

generate_ast_handle!(SourceInfoHandle);
impl SourceInfoHandle {
  pub fn get(&self, ast: &AST) -> SourceInfo {
    assert!(self.index < ast.source_ptrs.len() as u32);
    ast.source_ptrs[self.index as usize]
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StrHandle {
  pub(super) start: u32,
  pub(super) end: u32,
}

impl StrHandle {
  pub const INVALID: Self = Self {
    start: u32::MAX,
    end: u32::MAX,
  };

  pub(super) fn new(start: u32, end: u32) -> Self {
    Self { start, end }
  }

  pub fn get<'ast>(&self, ast: &'ast AST) -> &'ast str {
    &ast.strings[self.start as usize..self.end as usize]
  }
}
