mod debug_print;
mod expression;
mod handle;
mod statement;

use std::default::Default;

pub use expression::*;
pub use handle::*;
pub use statement::*;

use crate::errors::SourceInfo;

pub enum ASTNode {
  Expr(Expr),
  Stmt(Stmt),
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Default)]
pub struct AST {
  strings: String,
  nodes: Vec<ASTNode>,
  source_ptrs: Vec<SourceInfo>,
  program: Vec<StmtHandle>,
}

impl AST {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn add_expression(&mut self, expr: Expr) -> ExprHandle {
    self.nodes.push(ASTNode::Expr(expr));
    ExprHandle::new((self.nodes.len() - 1) as u32)
  }

  pub fn add_statement(&mut self, stmt: Stmt) -> StmtHandle {
    self.nodes.push(ASTNode::Stmt(stmt));
    StmtHandle::new((self.nodes.len() - 1) as u32)
  }

  pub fn add_str(&mut self, s: &str) -> StrHandle {
    let start = self.strings.len() as u32;
    self.strings.push_str(s);
    let end = self.strings.len() as u32;
    StrHandle::new(start, end)
  }

  pub fn add_source_info(&mut self, info: SourceInfo) -> SourceInfoHandle {
    self.source_ptrs.push(info);
    SourceInfoHandle::new((self.source_ptrs.len() - 1) as u32)
  }

  pub fn program_push(&mut self, stmt: StmtHandle) {
    self.program.push(stmt);
  }

  pub fn get_program(&self) -> &Vec<StmtHandle> {
    &self.program
  }
}
