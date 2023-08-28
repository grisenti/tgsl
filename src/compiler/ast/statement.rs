pub mod stmt {

  use crate::compiler::identifier::FunctionId;
  use crate::compiler::{
    ast::{ExprHandle, StmtHandle},
    identifier::{ExternId, ModuleId, StructId, VariableIdentifier},
    lexer::SourceRange,
    types::Type,
  };

  use super::Stmt;

  macro_rules! stmt_node {
  ($name:tt, $($member:ident : $t:ty),*) => {
    #[derive(Debug, Clone)]
		pub struct $name {
  		$(pub $member: $t),*
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
    var_type: Type,
    init_expr: ExprHandle
  );

  stmt_node!(StmtExpr,
    expr: ExprHandle,
    expr_type: Type
  );

  stmt_node!(Block,
    statements: Vec<StmtHandle>,
    locals: u8
  );

  stmt_node!(IfBranch,
    condition: ExprHandle,
    true_branch: StmtHandle,
    else_branch: Option<StmtHandle>
  );

  stmt_node!(While,
    condition: ExprHandle,
    loop_body: StmtHandle
  );

  stmt_node!(FunctionDefinition,
    id: FunctionId,
    captures: Vec<VariableIdentifier>,
    parameter_types: Vec<Type>,
    return_type: Type,
    body: Vec<StmtHandle>
  );

  stmt_node!(FunctionDeclaration,
    id: FunctionId,
    parameter_types: Vec<Type>,
    return_type: Type
  );

  stmt_node!(ExternFunction,
    identifier: ExternId,
    parameter_types: Vec<Type>,
    return_type: Type
  );

  stmt_node!(Break,);

  stmt_node!(Return,
    expr: Option<ExprHandle>
  );

  stmt_node!(Struct,
    id: StructId
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
