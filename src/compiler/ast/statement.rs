use crate::compiler::{
  identifier::{ExternId, Identifier, ModuleId},
  lexer::SourceRange,
  types::TypeId,
};

use super::{ExprHandle, StmtHandle, StrHandle};

#[derive(Debug, Clone)]
pub enum Stmt {
  VarDecl {
    identifier: Identifier,
    id_sr: SourceRange,
    var_type: TypeId,
    expression: Option<ExprHandle>,
  },
  Expr(ExprHandle),
  Block {
    statements: Vec<StmtHandle>,
    locals: u8,
  },
  IfBranch {
    if_sr: SourceRange,
    condition: ExprHandle,
    true_branch: StmtHandle,
    else_branch: Option<StmtHandle>,
  },
  While {
    while_sr: SourceRange,
    condition: ExprHandle,
    loop_body: StmtHandle,
  },
  FunctionDefinition {
    id: Identifier,
    name_sr: SourceRange,
    captures: Vec<Identifier>,
    parameter_types: Vec<TypeId>,
    return_type: TypeId,
    fn_type: TypeId,
    body: Vec<StmtHandle>,
  },
  ExternFunction {
    name_id: Identifier,
    extern_id: ExternId,
    name_sr: SourceRange,
    fn_type: TypeId,
  },
  Break(SourceRange),
  Return {
    expr: Option<ExprHandle>,
    return_sr: SourceRange,
  },
  Struct {
    id: Identifier,
    name_sr: SourceRange,
    constructor_type: TypeId,
    struct_type: TypeId,
    member_names: Vec<StrHandle>,
    member_types: Vec<TypeId>,
  },
  Import {
    module_id: ModuleId,
  },
}
