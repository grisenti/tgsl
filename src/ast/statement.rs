use crate::lexer::TokenInfo;

use super::Expr;

pub enum Stmt<'src> {
  VarDecl {
    identifier: &'src str,
    id_info: TokenInfo<'src>,
    expression: Option<Expr<'src>>,
  },
  Expr(Expr<'src>),
  Print {
    expression: Expr<'src>,
  },
  Block(Vec<Stmt<'src>>),
  IfBranch {
    if_info: TokenInfo<'src>,
    condition: Expr<'src>,
    true_branch: Box<Stmt<'src>>,
    else_branch: Option<Box<Stmt<'src>>>,
  },
}
