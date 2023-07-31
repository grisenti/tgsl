use std::fmt::Display;

use crate::compiler::lexer::Token;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Operator {
  Basic(char),

  // wide operators
  Leq,
  Geq,
  Same,
  Different,
  And,
  Or,
}

impl Display for Operator {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Basic(c) => write!(f, "{c}"),
      Self::Leq => write!(f, "<="),
      Self::Geq => write!(f, ">="),
      Self::Same => write!(f, "=="),
      Self::Different => write!(f, "!="),
      Self::And => write!(f, "and"),
      Self::Or => write!(f, "or"),
    }
  }
}

pub fn to_operator(token: Token) -> Operator {
  match token {
    Token::Leq => Operator::Leq,
    Token::Geq => Operator::Geq,
    Token::Same => Operator::Same,
    Token::Different => Operator::Different,
    Token::And => Operator::And,
    Token::Or => Operator::Or,
    Token::Basic(c) => Operator::Basic(c),
    _ => panic!(),
  }
}

pub mod expr {
  use crate::compiler::{
    ast::{ExprHandle, StmtHandle, StrHandle},
    identifier::{Identifier, VariableIdentifier},
    lexer::SourceRange,
    types::TypeId,
  };

  use super::{Expr, Operator};

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
    id_type: TypeId,
    id_sr: SourceRange
  );

  expr_node!(Paren,
    inner: ExprHandle
  );

  expr_node!(Assignment,
    id: VariableIdentifier,
    type_id: TypeId,
    id_sr: SourceRange,
    value: ExprHandle
  );

  expr_node!(Binary,
    left: ExprHandle,
    operator: Operator,
    operator_sr: SourceRange,
    right: ExprHandle,
    expr_type: TypeId
  );

  expr_node!(Logical,
    left: ExprHandle,
    operator: Operator,
    operator_sr: SourceRange,
    right: ExprHandle,
    expr_type: TypeId
  );

  expr_node!(Unary,
    operator: Operator,
    operator_sr: SourceRange,
    right: ExprHandle,
    expr_type: TypeId
  );

  expr_node!(Lambda,
    parameters_sr: SourceRange,
    captures: Vec<VariableIdentifier>,
    parameter_types: Vec<TypeId>,
    return_type: TypeId,
    function_type_id: TypeId,
    body: Vec<StmtHandle>
  );

  expr_node!(FnCall,
    func: ExprHandle,
    call_sr: SourceRange,
    arguments: Vec<ExprHandle>,
    expr_type: TypeId
  );

  expr_node!(Dot,
    lhs: ExprHandle,
    rhs_name: StrHandle,
    rhs_id: Option<VariableIdentifier>,
    rhs_sr: SourceRange
  );

  expr_node!(Set,
    object: ExprHandle,
    member_name: StrHandle,
    member_name_sr: SourceRange,
    value: ExprHandle
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
  Dot(Dot),
  Set(Set),
}
