use crate::lexer::{TokenInfo, TokenPair};

#[derive(Debug)]
pub enum Expr<'src> {
  BinaryExpr {
    left: DynExpr<'src>,
    operator: TokenPair<'src>,
    right: DynExpr<'src>,
  },
  Logical {
    left: DynExpr<'src>,
    operator: TokenPair<'src>,
    right: DynExpr<'src>,
  },
  UnaryExpr {
    operator: TokenPair<'src>,
    right: DynExpr<'src>,
  },
  Literal(TokenPair<'src>),
  Variable {
    id: &'src str,
    id_info: TokenInfo<'src>,
  },
  Assignment {
    name: &'src str,
    name_info: TokenInfo<'src>,
    value: DynExpr<'src>,
  },
}

type DynExpr<'src> = Box<Expr<'src>>;
