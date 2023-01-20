use crate::lexer::TokenPair;

pub enum Expr<'src> {
  BinaryExpr {
    left: DynExpr<'src>,
    operator: TokenPair<'src>,
    right: DynExpr<'src>,
  },
  UnaryExpr {
    operator: TokenPair<'src>,
    right: DynExpr<'src>,
  },
  Literal {
    literal: TokenPair<'src>,
  },
}

type DynExpr<'src> = Box<Expr<'src>>;
