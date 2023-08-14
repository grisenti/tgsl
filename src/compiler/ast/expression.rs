use std::fmt::{write, Display, Formatter};

use crate::compiler::lexer::Token;

pub mod expr {
  use crate::compiler::global_env::MemberIndex;
  use crate::compiler::operators::{BinaryOperator, LogicalOperator, UnaryOperator};
  use crate::compiler::{
    ast::{ExprHandle, StmtHandle, StrHandle},
    identifier::{Identifier, VariableIdentifier},
    lexer::SourceRange,
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
    handle: StrHandle,
    value_sr: SourceRange
  );

  expr_node!(LiteralNumber,
    value: f64,
    value_sr: SourceRange
  );

  expr_node!(LiteralBool,
    value: bool,
    value_sr: SourceRange
  );

  expr_node!(Id,
    id: Identifier,
    id_type: Type,
    id_sr: SourceRange
  );

  expr_node!(Paren,
    inner: ExprHandle
  );

  expr_node!(Assignment,
    id: VariableIdentifier,
    type_: Type,
    id_sr: SourceRange,
    value: ExprHandle
  );

  expr_node!(Binary,
    left: ExprHandle,
    operator: BinaryOperator,
    operator_sr: SourceRange,
    right: ExprHandle,
    expr_type: Type
  );

  expr_node!(Logical,
    left: ExprHandle,
    operator: LogicalOperator,
    operator_sr: SourceRange,
    right: ExprHandle,
    expr_type: Type
  );

  expr_node!(Unary,
    operator: UnaryOperator,
    operator_sr: SourceRange,
    right: ExprHandle,
    expr_type: Type
  );

  expr_node!(Lambda,
    parameters_sr: SourceRange,
    captures: Vec<VariableIdentifier>,
    parameter_types: Vec<Type>,
    return_type: Type,
    body: Vec<StmtHandle>
  );

  expr_node!(FnCall,
    func: ExprHandle,
    call_sr: SourceRange,
    arguments: Vec<ExprHandle>,
    expr_type: Type
  );

  expr_node!(MemberGet,
    lhs: ExprHandle,
    rhs_sr: SourceRange,
    member_index: MemberIndex
  );

  expr_node!(MemberSet,
    member: MemberGet,
    eq_sr: SourceRange,
    value: ExprHandle
  );

  expr_node!(DotCall,
    lhs: ExprHandle,
    function: VariableIdentifier,
    arguments: Vec<ExprHandle>,
    call_sr: SourceRange
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
}
