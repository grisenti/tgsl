use crate::compiler::{
  identifier::{ExternId, Identifier, ModuleId, StructId, VariableIdentifier},
  lexer::SourceRange,
  types::TypeId,
};

use super::{ExprHandle, StmtHandle, StrHandle};

#[derive(Debug, Clone)]
pub enum Stmt {
  VarDecl {
    identifier: VariableIdentifier,
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
    id: VariableIdentifier,
    name_sr: SourceRange,
    captures: Vec<VariableIdentifier>,
    parameter_types: Vec<TypeId>,
    return_type: TypeId,
    fn_type: TypeId,
    body: Vec<StmtHandle>,
  },
  FunctionDeclaration {
    id: VariableIdentifier,
    name_sr: SourceRange,
    parameter_types: Vec<TypeId>,
    return_type: TypeId,
    fn_type: TypeId,
  },
  ExternFunction {
    identifier: ExternId,
    name_sr: SourceRange,
    fn_type: TypeId,
  },
  Break(SourceRange),
  Return {
    expr: Option<ExprHandle>,
    return_sr: SourceRange,
  },
  Struct {
    id: StructId,
    name_sr: SourceRange,
    constructor_type: TypeId,
    constructor_id: VariableIdentifier,
    struct_type: TypeId,
    member_names: Vec<StrHandle>,
    member_types: Vec<TypeId>,
  },
  Import {
    module_id: ModuleId,
  },
}
