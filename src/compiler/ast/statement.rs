use super::{
  types::Type, ExprHandle, Identifier, SourceInfoHandle, StmtHandle, StrHandle, UserTypeId,
};

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
    var_type: Type,
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
    fn_type: Type,
    body: Vec<StmtHandle>,
  },
  Break(SourceInfoHandle),
  Return {
    expr: ExprHandle,
    src_info: SourceInfoHandle,
  },
  Struct {
    name: Identifier,
    type_id: Type,
    name_info: SourceInfoHandle,
    members: Vec<(StrHandle, Type)>,
  },
}
