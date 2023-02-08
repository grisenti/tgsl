use std::marker::PhantomData;

use crate::errors::SourceInfo;

use super::{Expr, Stmt};

#[derive(Debug, Clone, Copy)]
pub struct ASTHandle<T> {
  pub(super) index: u32,
  __: PhantomData<T>,
}

impl<T> ASTHandle<T> {
  pub(super) fn new(index: u32) -> Self {
    Self {
      index,
      __: PhantomData {},
    }
  }
}

impl<T> PartialEq for ASTHandle<T> {
  fn eq(&self, other: &Self) -> bool {
    self.index == other.index
  }
}

#[derive(Debug, Clone, Copy)]
pub struct ASTSliceHandle<T> {
  pub(super) start: u32,
  pub(super) end: u32,
  __: PhantomData<T>,
}

impl<T> PartialEq for ASTSliceHandle<T> {
  fn eq(&self, other: &Self) -> bool {
    self.start == other.start && self.end == other.end
  }
}

impl<T> ASTSliceHandle<T> {
  pub(super) fn new(start: u32, end: u32) -> Self {
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
pub type StrHandle = ASTSliceHandle<String>;
