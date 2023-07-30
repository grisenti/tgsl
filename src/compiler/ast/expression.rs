use std::fmt::Display;

use crate::compiler::{
  identifier::{Identifier, VariableIdentifier},
  lexer::{SourceRange, Token},
  types::TypeId,
};

use super::{ExprHandle, StmtHandle, StrHandle, AST};

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

#[derive(Debug, Clone)]
pub enum Expr {
  LiteralString {
    handle: StrHandle,
    value_sr: SourceRange,
  },
  LiteralNumber {
    value: f64,
    value_sr: SourceRange,
  },
  LiteralBool {
    value: bool,
    value_sr: SourceRange,
  },
  Identifier {
    id: Identifier,
    id_type: TypeId,
    id_sr: SourceRange,
  },
  Paren(ExprHandle),
  Assignment {
    id: VariableIdentifier,
    type_id: TypeId,
    id_sr: SourceRange,
    value: ExprHandle,
  },
  Binary {
    left: ExprHandle,
    operator: Operator,
    operator_sr: SourceRange,
    right: ExprHandle,
    expr_type: TypeId,
  },
  Logical {
    left: ExprHandle,
    operator: Operator,
    operator_sr: SourceRange,
    right: ExprHandle,
    expr_type: TypeId,
  },
  Unary {
    operator: Operator,
    operator_sr: SourceRange,
    right: ExprHandle,
    expr_type: TypeId,
  },
  Lambda {
    parameters_sr: SourceRange,
    captures: Vec<VariableIdentifier>,
    parameter_types: Vec<TypeId>,
    return_type: TypeId,
    function_type_id: TypeId,
    body: Vec<StmtHandle>,
  },
  FnCall {
    func: ExprHandle,
    call_sr: SourceRange,
    arguments: Vec<ExprHandle>,
    expr_type: TypeId,
  },
  // MemberGet {},
  // DotCall {},
  // MemberSet {},
  Dot {
    lhs: ExprHandle,
    rhs_name: StrHandle,
    rhs_id: Option<VariableIdentifier>,
    rhs_sr: SourceRange,
  },
  Set {
    object: ExprHandle,
    member_name: StrHandle,
    member_name_sr: SourceRange,
    value: ExprHandle,
  },
}
