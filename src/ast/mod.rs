mod expression;
mod statement;

pub use expression::*;
pub use statement::*;

pub enum ASTNode<'src> {
  Expr(Expr<'src>),
  Stmt(Stmt<'src>),
  Program(Vec<Stmt<'src>>),
}
