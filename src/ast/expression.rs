use super::*;
use crate::Token;

pub trait Expr {
  fn accept(&mut self, visitor: &mut dyn NodeVisitor);
}

impl<T: Expr> ASTNode for T {
  fn accept(&mut self, visitor: &mut dyn NodeVisitor) {
    self.accept(visitor);
  }
}

type DynExpr = Box<dyn Expr>;

pub struct BinaryExpr<'src> {
  pub left: DynExpr,
  pub operator: Token<'src>,
  pub right: DynExpr,
}

pub struct UnaryExpr<'src> {
  pub operator: Token<'src>,
  pub right: DynExpr,
}

pub struct Literal<'src> {
  pub token: Token<'src>,
}
