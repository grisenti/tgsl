use super::{ExprHandle, SourceInfoHandle, StmtHandle, StrHandle};

#[derive(Debug, Clone)]
pub enum Stmt {
  VarDecl {
    identifier: StrHandle,
    id_info: SourceInfoHandle,
    expression: Option<ExprHandle>,
  },
  Expr(ExprHandle),
  Print(ExprHandle),
  Block(Vec<StmtHandle>),
  IfBranch {
    if_info: SourceInfoHandle,
    condition: ExprHandle,
    true_branch: StmtHandle,
    else_branch: Option<StmtHandle>,
  },
  While {
    info: SourceInfoHandle,
    condition: ExprHandle,
    loop_body: StmtHandle,
  },
  Function {
    name: StrHandle,
    name_info: SourceInfoHandle,
    parameters: Vec<StrHandle>,
    body: Vec<StmtHandle>,
  },
  Break,
}
