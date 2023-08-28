pub mod expr {
  use crate::compiler::global_env::MemberIndex;
  use crate::compiler::identifier::{FunctionId, StructId};
  use crate::compiler::operators::{BinaryOperator, LogicalOperator, UnaryOperator};
  use crate::compiler::{
    ast::{ExprHandle, StmtHandle, StrHandle},
    identifier::{Identifier, VariableIdentifier},
    types::Type,
  };

  use super::Expr;

  macro_rules! expr_node {
  ($name:tt, $($member:ident : $t:ty),+) => {
    #[derive(Debug, Clone)]
		pub struct $name {
  		$(pub $member: $t),+
		}

    impl From<$name> for Expr {
      fn from(value: $name) -> Self {
        Self::$name(value)
      }
    }
  };
}
  expr_node!(LiteralString,
    handle: StrHandle
  );

  expr_node!(LiteralNumber,
    value: f64
  );

  expr_node!(LiteralBool,
    value: bool
  );

  expr_node!(Id,
    id: Identifier,
    id_type: Type
  );

  expr_node!(Paren,
    inner: ExprHandle
  );

  expr_node!(Assignment,
    id: VariableIdentifier,
    type_: Type,
    value: ExprHandle
  );

  expr_node!(Binary,
    left: ExprHandle,
    operator: BinaryOperator,
    right: ExprHandle,
    expr_type: Type
  );

  expr_node!(Logical,
    left: ExprHandle,
    operator: LogicalOperator,
    right: ExprHandle,
    expr_type: Type
  );

  expr_node!(Unary,
    operator: UnaryOperator,
    right: ExprHandle,
    expr_type: Type
  );

  expr_node!(Lambda,
    id: FunctionId,
    captures: Vec<VariableIdentifier>,
    parameter_types: Vec<Type>,
    return_type: Type,
    body: Vec<StmtHandle>
  );

  expr_node!(FnCall,
    func: ExprHandle,
    arguments: Vec<ExprHandle>,
    expr_type: Type
  );

  expr_node!(MemberGet,
    lhs: ExprHandle,
    member_index: MemberIndex
  );

  expr_node!(MemberSet,
    member: MemberGet,
    value: ExprHandle
  );

  expr_node!(DotCall,
    lhs: ExprHandle,
    function: VariableIdentifier,
    arguments: Vec<ExprHandle>
  );

  expr_node!(Construct,
    struct_id: StructId,
    arguments: Vec<ExprHandle>
  );
}

use expr::*;

#[derive(Debug, Clone)]
pub enum Expr {
  LiteralString(LiteralString),
  LiteralNumber(LiteralNumber),
  LiteralBool(LiteralBool),
  Id(Id),
  Paren(Paren),
  Assignment(Assignment),
  Binary(Binary),
  Logical(Logical),
  Unary(Unary),
  Lambda(Lambda),
  FnCall(FnCall),
  MemberGet(MemberGet),
  MemberSet(MemberSet),
  DotCall(DotCall),
  Construct(Construct),
}
