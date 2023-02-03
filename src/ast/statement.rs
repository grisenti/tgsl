use super::{ExprHandle, Identifier, SourceInfoHandle, StmtHandle, StrHandle};

#[derive(Debug, Clone)]
pub struct Method {
  pub name_info: SourceInfoHandle,
  pub parameters: Vec<Identifier>,
  pub body: Vec<StmtHandle>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
  VarDecl {
    identifier: Identifier,
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
    id: Identifier,
    name_info: SourceInfoHandle,
    parameters: Vec<Identifier>,
    body: Vec<StmtHandle>,
  },
  Break(SourceInfoHandle),
  Return {
    expr: ExprHandle,
    src_info: SourceInfoHandle,
  },
  Class {
    name: Identifier,
    name_info: SourceInfoHandle,
    methods: Vec<(StrHandle, Method)>,
  },
}
