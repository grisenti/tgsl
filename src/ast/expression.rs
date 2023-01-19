use crate::Token;

pub enum Expr<'src> {
  BinaryExpr {
    left: DynExpr<'src>,
    operator: Token<'src>,
    right: DynExpr<'src>,
  },
  UnaryExpr {
    operator: Token<'src>,
    right: DynExpr<'src>,
  },
  Literal {
    literal: Token<'src>,
  },
}

type DynExpr<'src> = Box<Expr<'src>>;
