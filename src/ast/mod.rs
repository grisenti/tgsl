//pub mod codegen;
mod expression;
mod statement;

use core::panic;
use std::{default::Default, marker::PhantomData};

pub use expression::*;
pub use statement::*;

use crate::lexer::SourceInfo;

#[derive(Debug, Clone, Copy)]
pub struct ASTHandle<T> {
  index: u32,
  __: PhantomData<T>,
}

impl<T> ASTHandle<T> {
  fn new(index: u32) -> Self {
    Self {
      index,
      __: PhantomData {},
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub struct HandleSlice<T> {
  start: u32,
  end: u32,
  __: PhantomData<T>,
}

impl<T> HandleSlice<T> {
  fn new(start: u32, end: u32) -> Self {
    Self {
      start,
      end,
      __: PhantomData {},
    }
  }
}

pub type ExprHandle = ASTHandle<Expr>;
pub type StmtHandle = ASTHandle<Stmt>;
pub type SourceInfoHandle = ASTHandle<SourceInfo>;
pub type StrHandle = HandleSlice<String>;

pub enum ASTNode {
  Expr(Expr),
  Stmt(Stmt),
}

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

  pub fn get_statement(&self, handle: StmtHandle) -> Stmt {
    assert!(handle.index < self.nodes.len() as u32);
    if let ASTNode::Stmt(s) = &self.nodes[handle.index as usize] {
      s.clone()
    } else {
      panic!("statement handle refers to a node thats not a statement");
    }
  }

  pub fn get_expression(&self, handle: ExprHandle) -> Expr {
    assert!(handle.index < self.nodes.len() as u32);
    if let ASTNode::Expr(s) = &self.nodes[handle.index as usize] {
      s.clone()
    } else {
      panic!("expression handle refers to a node thats not an expression");
    }
  }

  pub fn add_expression(&mut self, expr: Expr) -> ExprHandle {
    self.nodes.push(ASTNode::Expr(expr));
    ExprHandle::new((self.nodes.len() - 1) as u32)
  }

  pub fn add_statement(&mut self, stmt: Stmt) -> StmtHandle {
    self.nodes.push(ASTNode::Stmt(stmt));
    StmtHandle::new((self.nodes.len() - 1) as u32)
  }

  pub fn get_str(&self, handle: StrHandle) -> &str {
    &self.strings[handle.start as usize..handle.end as usize]
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

  pub fn get_source_info(&self, handle: SourceInfoHandle) -> SourceInfo {
    assert!(handle.index < self.source_ptrs.len() as u32);
    self.source_ptrs[handle.index as usize].clone()
  }
}
