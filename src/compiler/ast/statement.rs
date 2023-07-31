pub mod stmt {

  use crate::compiler::{
    ast::{ExprHandle, StmtHandle, StrHandle},
    identifier::{ExternId, ModuleId, StructId, VariableIdentifier},
    lexer::SourceRange,
    types::TypeId,
  };

  use super::Stmt;

  macro_rules! stmt_node {
  ($name:tt, $($member:ident : $t:ty),+) => {
    #[derive(Debug, Clone)]
		pub struct $name {
  		$(pub $member: $t),+
		}

    impl From<$name> for Stmt {
      fn from(value: $name) -> Self {
        Self::$name(value)
      }
    }
  };
}

  stmt_node!(VarDecl,
    identifier: VariableIdentifier,
    id_sr: SourceRange,
    var_type: TypeId,
    init_expr: ExprHandle
  );

  stmt_node!(StmtExpr,
    expr: ExprHandle,
    expr_type: TypeId
  );

  stmt_node!(Block,
    statements: Vec<StmtHandle>,
    locals: u8
  );

  stmt_node!(IfBranch,
    if_sr: SourceRange,
    condition: ExprHandle,
    true_branch: StmtHandle,
    else_branch: Option<StmtHandle>
  );

  stmt_node!(While,
    while_sr: SourceRange,
    condition: ExprHandle,
    loop_body: StmtHandle
  );

  stmt_node!(FunctionDefinition,
    id: VariableIdentifier,
    name_sr: SourceRange,
    captures: Vec<VariableIdentifier>,
    parameter_types: Vec<TypeId>,
    return_type: TypeId,
    fn_type: TypeId,
    body: Vec<StmtHandle>
  );

  stmt_node!(FunctionDeclaration,
    id: VariableIdentifier,
    name_sr: SourceRange,
    parameter_types: Vec<TypeId>,
    return_type: TypeId,
    fn_type: TypeId
  );

  stmt_node!(ExternFunction,
    identifier: ExternId,
    name_sr: SourceRange,
    fn_type: TypeId
  );

  stmt_node!(Break,
    sr: SourceRange
  );

  stmt_node!(Return,
    expr: Option<ExprHandle>,
    return_sr: SourceRange
  );

  stmt_node!(Struct,
    id: StructId,
    name_sr: SourceRange,
    constructor_type: TypeId,
    constructor_id: VariableIdentifier,
    struct_type: TypeId,
    member_names: Vec<StrHandle>,
    member_types: Vec<TypeId>
  );

  stmt_node!(Import,
    module_id: ModuleId
  );
}

use stmt::*;

#[derive(Debug, Clone)]
pub enum Stmt {
  VarDecl(VarDecl),
  StmtExpr(StmtExpr),
  Block(Block),
  IfBranch(IfBranch),
  While(While),
  FunctionDefinition(FunctionDefinition),
  FunctionDeclaration(FunctionDeclaration),
  ExternFunction(ExternFunction),
  Break(Break),
  Return(Return),
  Struct(Struct),
  Import(Import),
}
