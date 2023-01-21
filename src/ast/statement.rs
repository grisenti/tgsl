use crate::lexer::TokenInfo;

use super::Expr;

pub enum Stmt<'src> {
  VarDecl {
    identifier: &'src str,
    id_info: TokenInfo<'src>,
    expression: Option<Expr<'src>>,
  },
  ExprStmt(Expr<'src>),
  Print {
    expression: Expr<'src>,
  },
}
