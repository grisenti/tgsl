use std::fmt::Display;

use crate::compiler::lexer::Token;

use super::{ExprHandle, SourceInfoHandle, StmtHandle, StrHandle, Type, AST};

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
      Self::Basic(c) => write!(f, "{}", c),
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
pub enum Literal {
  String(StrHandle),
  Number(f64),
  True,
  False,
  Null,
}

impl Literal {
  pub fn display(&self, ast: &AST) -> String {
    match self {
      Self::String(s) => format!("\"{}\"", ast.get_str(s.clone())),
      Self::Number(num) => format!("{num}"),
      _ => format!("{self:?}").to_lowercase(),
    }
  }
}

pub fn literal_from_token(token: Token, ast: &mut AST) -> Literal {
  match token {
    Token::String(s) => Literal::String(ast.add_str(s)),
    Token::Number(num) => Literal::Number(num),
    Token::True => Literal::True,
    Token::False => Literal::False,
    Token::Null => Literal::Null,
    _ => panic!(),
  }
}

#[derive(Debug, Clone)]
pub struct OperatorPair {
  pub op: Operator,
  pub src_info: SourceInfoHandle,
}

impl OperatorPair {
  pub fn new(op: Operator, src_info: SourceInfoHandle) -> Self {
    Self { op, src_info }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub struct Identifier(pub u32);

#[derive(Debug, Clone)]
pub enum Expr {
  Binary {
    left: ExprHandle,
    operator: OperatorPair,
    right: ExprHandle,
  },
  Unary {
    operator: OperatorPair,
    right: ExprHandle,
  },
  Literal {
    literal: Literal,
    info: SourceInfoHandle,
  },
  Variable {
    id: Identifier,
    id_info: SourceInfoHandle,
  },
  Assignment {
    id: Identifier,
    id_info: SourceInfoHandle,
    value: ExprHandle,
  },
  Closure {
    parameters: Vec<Identifier>,
    fn_type: Vec<Type>,
    body: Vec<StmtHandle>,
  },
  FnCall {
    func: ExprHandle,
    call_info: SourceInfoHandle,
    arguments: Vec<ExprHandle>,
  },
  Dot {
    lhs: ExprHandle,
    name: StrHandle,
    identifier: Identifier,
    name_info: SourceInfoHandle,
  },
  Set {
    object: ExprHandle,
    name: StrHandle,
    name_info: SourceInfoHandle,
    value: ExprHandle,
  },
}
