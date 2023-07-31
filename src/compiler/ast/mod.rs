pub mod expression;
mod handle;
pub mod json;
pub mod statement;
pub mod visitor;

use std::default::Default;

pub use handle::*;

use self::{expression::Expr, statement::Stmt};

#[allow(clippy::upper_case_acronyms)]
#[derive(Default)]
pub struct AST {
  strings: String,
  statements: Vec<Stmt>,
  expressions: Vec<Expr>,
  program: Vec<StmtHandle>,
}

impl AST {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn add_expression<E: Into<Expr>>(&mut self, expr: E) -> ExprHandle {
    self.expressions.push(expr.into());
    ExprHandle::new((self.expressions.len() - 1) as u32)
  }

  pub fn add_statement<S: Into<Stmt>>(&mut self, stmt: S) -> StmtHandle {
    self.statements.push(stmt.into());
    StmtHandle::new((self.statements.len() - 1) as u32)
  }

  pub fn add_str(&mut self, s: &str) -> StrHandle {
    let start = self.strings.len() as u32;
    self.strings.push_str(s);
    let end = self.strings.len() as u32;
    StrHandle::new(start, end)
  }

  pub fn program_push(&mut self, stmt: StmtHandle) {
    self.program.push(stmt);
  }

  pub fn get_program(&self) -> &Vec<StmtHandle> {
    &self.program
  }
}
