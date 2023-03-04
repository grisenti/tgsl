use super::{types::Type, ExprHandle, Identifier, SourceInfoHandle, StmtHandle, StrHandle};

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
  Block {
    statements: Vec<StmtHandle>,
    locals: u8,
  },
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
    captures: Vec<Identifier>,
    parameters: Vec<Identifier>,
    fn_type: Vec<Type>,
    body: Vec<StmtHandle>,
  },
  ExternFunction {
    id: Identifier,
    name_info: SourceInfoHandle,
    fn_type: Vec<Type>,
  },
  Break(SourceInfoHandle),
  Return {
    expr: Option<ExprHandle>,
    src_info: SourceInfoHandle,
  },
  Struct {
    name: Identifier,
    name_info: SourceInfoHandle,
    member_names: Vec<StrHandle>,
    member_types: Vec<Type>,
  },
}
