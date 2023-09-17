pub mod stmt {

  use crate::compiler::ast::{ExprHandle, StmtHandle, TypeHandle};

  use super::Stmt;

  macro_rules! stmt_node {
    ($name:tt, $($member:ident : $t:ty),*) => {
      #[derive(Debug, Clone)]
      pub struct $name {
        $(pub $member: $t),*
      }

      impl<'src> From<$name> for Stmt<'src> {
        fn from(value: $name) -> Self {
          Self::$name(value)
        }
      }
    };
  }

  macro_rules! src_stmt_node {
    ($name:tt, $($member:ident : $t:ty),*) => {
      #[derive(Debug, Clone)]
      pub struct $name<'src> {
        $(pub $member: $t),*
      }

      impl<'src> From<$name<'src>> for Stmt<'src> {
        fn from(value: $name<'src>) -> Self {
          Self::$name(value)
        }
      }
    };
  }

  src_stmt_node!(
    VarDecl,
    name: &'src str,
    specified_type: TypeHandle,
    init_expr: ExprHandle
  );

  stmt_node!(StmtExpr, expr: ExprHandle);

  stmt_node!(Block, statements: Vec<StmtHandle>);

  stmt_node!(
    IfBranch,
    condition: ExprHandle,
    true_branch: StmtHandle,
    else_branch: Option<StmtHandle>
  );

  stmt_node!(While, condition: ExprHandle, loop_body: StmtHandle);

  src_stmt_node!(
    FunctionDefinition,
    name: &'src str,
    parameter_names: Vec<&'src str>,
    parameter_types: Vec<TypeHandle>,
    return_type: TypeHandle,
    body: Vec<StmtHandle>
  );

  src_stmt_node!(
    FunctionDeclaration,
    name: &'src str,
    parameter_names: Vec<&'src str>,
    parameter_types: Vec<TypeHandle>,
    return_type: TypeHandle
  );

  src_stmt_node!(
    ExternFunction,
    name: &'src str,
    parameter_names: Vec<&'src str>,
    parameter_types: Vec<TypeHandle>,
    return_type: TypeHandle
  );

  stmt_node!(Return, expr: Option<ExprHandle>);

  src_stmt_node!(StructDeclaration, name: &'src str);

  src_stmt_node!(
    StructDefinition,
    name: &'src str,
    member_names: Vec<&'src str>,
    member_types: Vec<TypeHandle>
  );

  src_stmt_node!(Import, module_name: &'src str);

  src_stmt_node!(ModuleDecl, name: &'src str);
}

use stmt::*;

#[derive(Debug, Clone)]
pub enum Stmt<'src> {
  VarDecl(VarDecl<'src>),
  StmtExpr(StmtExpr),
  Block(Block),
  IfBranch(IfBranch),
  While(While),
  FunctionDefinition(FunctionDefinition<'src>),
  FunctionDeclaration(FunctionDeclaration<'src>),
  ExternFunction(ExternFunction<'src>),
  Break,
  Return(Return),
  StructDeclaration(StructDeclaration<'src>),
  StructDefinition(StructDefinition<'src>),
  Import(Import<'src>),
  ModuleDecl(ModuleDecl<'src>),
}
