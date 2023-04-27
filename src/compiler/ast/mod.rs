mod debug_print;
mod expression;
mod handle;
mod statement;

use std::default::Default;

pub use expression::*;
pub use handle::*;
pub use statement::*;

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

  pub fn add_expression(&mut self, expr: Expr) -> ExprHandle {
    self.expressions.push(expr);
    ExprHandle::new((self.expressions.len() - 1) as u32)
  }

  pub fn add_statement(&mut self, stmt: Stmt) -> StmtHandle {
    self.statements.push(stmt);
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
