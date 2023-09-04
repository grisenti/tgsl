pub mod expression;
mod handle;
pub mod json;
pub mod parsed_type;
pub mod statement;
pub mod visitor;

use std::default::Default;

use crate::compiler::ast::parsed_type::ParsedType;
use crate::compiler::lexer::SourceRange;
pub use handle::*;

use self::{expression::Expr, statement::Stmt};

#[allow(clippy::upper_case_acronyms)]
pub struct AST<'src> {
  statements: Vec<Stmt<'src>>,
  statement_source_range: Vec<SourceRange>,
  expressions: Vec<Expr<'src>>,
  expression_source_range: Vec<SourceRange>,
  types: Vec<ParsedType<'src>>,
  program: Vec<StmtHandle>,
}

impl<'src> AST<'src> {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn add_expression<E: Into<Expr<'src>>>(
    &mut self,
    expr: E,
    source_range: SourceRange,
  ) -> ExprHandle {
    self.expressions.push(expr.into());
    self.expression_source_range.push(source_range);
    ExprHandle::new((self.expressions.len() - 1) as u32)
  }

  pub fn add_statement<S: Into<Stmt<'src>>>(
    &mut self,
    stmt: S,
    source_range: SourceRange,
  ) -> StmtHandle {
    self.statements.push(stmt.into());
    self.statement_source_range.push(source_range);
    StmtHandle::new((self.statements.len() - 1) as u32)
  }

  pub fn add_parsed_type(&mut self, parsed_type: ParsedType<'src>) -> TypeHandle {
    self.types.push(parsed_type);
    TypeHandle::new((self.types.len() - 1) as u32)
  }

  pub fn program_push(&mut self, stmt: StmtHandle) {
    self.program.push(stmt);
  }

  pub fn get_program(&self) -> &Vec<StmtHandle> {
    &self.program
  }
}

impl Default for AST<'_> {
  fn default() -> Self {
    Self {
      statements: vec![],
      statement_source_range: vec![],
      expressions: vec![],
      expression_source_range: vec![],
      types: vec![
        ParsedType::Num,
        ParsedType::Str,
        ParsedType::Bool,
        ParsedType::Nothing,
      ],
      program: vec![],
    }
  }
}
