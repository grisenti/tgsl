use crate::{compiler::ast::ASTNode, errors::SourceInfo};

use super::{Expr, Stmt, AST};

macro_rules! generate_ast_handle {
  ($name:ident) => {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct $name {
      pub(super) index: u32,
    }

    impl $name {
      pub(super) fn new(index: u32) -> Self {
        Self { index }
      }
    }
  };
}

generate_ast_handle!(ExprHandle);
impl ExprHandle {
  pub fn get(&self, ast: &AST) -> Expr {
    assert!(self.index < ast.nodes.len() as u32);
    if let ASTNode::Expr(e) = &ast.nodes[self.index as usize] {
      e.clone()
    } else {
      panic!("expression handle refers to a node thats not an expression");
    }
  }
}

generate_ast_handle!(StmtHandle);
impl StmtHandle {
  pub fn get(&self, ast: &AST) -> Stmt {
    assert!(self.index < ast.nodes.len() as u32);
    if let ASTNode::Stmt(s) = &ast.nodes[self.index as usize] {
      s.clone()
    } else {
      panic!("statement handle refers to a node thats not a statement");
    }
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
  pub(super) fn new(start: u32, end: u32) -> Self {
    Self { start, end }
  }

  pub fn get<'ast>(&self, ast: &'ast AST) -> &'ast str {
    &ast.strings[self.start as usize..self.end as usize]
  }
}
