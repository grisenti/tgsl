use std::collections::HashMap;

use crate::ast::*;
use crate::errors::{SourceError, SourceErrorType};
use crate::lexer::{Token, TokenInfo, TokenPair};

#[derive(Debug, Clone)]
pub enum ExprValue {
  Str(String),
  Num(f64),
  Boolean(bool),
  Null,
}

type ExprResult = Result<ExprValue, SourceError>;

fn unary_minus(rhs: ExprValue, op_info: TokenInfo) -> ExprResult {
  if let ExprValue::Num(x) = rhs {
    Ok(ExprValue::Num(-x))
  } else {
    Err(SourceError::from_token_info(
      op_info,
      format!("unary - cannot be applyed to operand {:?}", rhs),
      SourceErrorType::Runtime,
    ))
  }
}

fn unary_not(rhs: ExprValue, op_info: TokenInfo) -> ExprResult {
  if let ExprValue::Boolean(x) = rhs {
    Ok(ExprValue::Boolean(!x))
  } else {
    Err(SourceError::from_token_info(
      op_info,
      format!("unary ! cannot be applyed to operand {:?}", rhs),
      SourceErrorType::Runtime,
    ))
  }
}

fn binary_num<F>(lhs: ExprValue, rhs: ExprValue, op_info: TokenInfo, op: F) -> ExprResult
where
  F: Fn(f64, f64) -> f64,
{
  match (lhs, rhs) {
    (ExprValue::Num(l), ExprValue::Num(r)) => Ok(ExprValue::Num(op(l, r))),
    (lhs, rhs) => Err(SourceError::from_token_info(
      op_info,
      format!(
        "operation only works for numbers, not supported for operands {:?} {:?}",
        lhs, rhs
      ),
      SourceErrorType::Runtime,
    )),
  }
}

fn add(lhs: ExprValue, rhs: ExprValue, op_info: TokenInfo) -> ExprResult {
  match (lhs, rhs) {
    (ExprValue::Num(l), ExprValue::Num(r)) => Ok(ExprValue::Num(l + r)),
    (ExprValue::Str(l), ExprValue::Str(r)) => Ok(ExprValue::Str(l + &r)),
    (lhs, rhs) => Err(SourceError::from_token_info(
      op_info,
      format!("cannot add {:?} and {:?}", lhs, rhs),
      SourceErrorType::Runtime,
    )),
  }
}

fn equal(lhs: ExprValue, rhs: ExprValue, op_info: TokenInfo) -> ExprResult {
  match (lhs, rhs) {
    (ExprValue::Num(l), ExprValue::Num(r)) => Ok(ExprValue::Boolean(l == r)),
    (ExprValue::Str(l), ExprValue::Str(r)) => Ok(ExprValue::Boolean(l == r)),
    (ExprValue::Boolean(l), ExprValue::Boolean(r)) => Ok(ExprValue::Boolean(l == r)),
    (lhs, rhs) => Err(SourceError::from_token_info(
      op_info,
      format!("cannot determine if {:?} and {:?} are equal", lhs, rhs),
      SourceErrorType::Runtime,
    )),
  }
}

fn compare<Nc, Sc>(
  lhs: ExprValue,
  rhs: ExprValue,
  op_info: TokenInfo,
  num_cmp: Nc,
  str_cmp: Sc,
) -> ExprResult
where
  Sc: Fn(&str, &str) -> bool,
  Nc: Fn(f64, f64) -> bool,
{
  match (lhs, rhs) {
    (ExprValue::Num(l), ExprValue::Num(r)) => Ok(ExprValue::Boolean(num_cmp(l, r))),
    (ExprValue::Str(l), ExprValue::Str(r)) => Ok(ExprValue::Boolean(str_cmp(&l, &r))),
    (lhs, rhs) => Err(SourceError::from_token_info(
      op_info,
      format!("operation not supported for operands {:?}, {:?}", lhs, rhs),
      SourceErrorType::Runtime,
    )),
  }
}

pub struct Interpreter<'src> {
  identifiers: HashMap<&'src str, ExprValue>,
}

pub type IntepreterResult = Result<(), SourceError>;

impl<'src> Interpreter<'src> {
  fn install_identifier(
    &mut self,
    id: &'src str,
    id_info: TokenInfo,
    exp_opt: Option<Expr>,
  ) -> IntepreterResult {
    if !self.identifiers.contains_key(id) {
      match exp_opt {
        Some(exp) => self.identifiers.insert(id, self.interpret_expression(exp)?),
        None => self.identifiers.insert(id, ExprValue::Null),
      };
      Ok(())
    } else {
      Err(SourceError::from_token_info(
        id_info,
        format!("identifier {} already declared", id),
        SourceErrorType::Runtime,
      ))
    }
  }

  fn get_identifier(&self, id: &str, id_info: TokenInfo) -> ExprResult {
    match self.identifiers.get(id) {
      Some(value) => Ok(value.clone()),
      None => Err(SourceError::from_token_info(
        id_info,
        format!("unknown identifier {}", id),
        SourceErrorType::Runtime,
      )),
    }
  }

  fn handle_literal_expression(&self, literal: TokenPair<'src>) -> ExprResult {
    match literal.token {
      Token::Number(num) => Ok(ExprValue::Num(num)),
      Token::String(s) => Ok(ExprValue::Str(s.to_string())),
      Token::True => Ok(ExprValue::Boolean(true)),
      Token::False => Ok(ExprValue::Boolean(false)),
      Token::Null => Ok(ExprValue::Null),
      Token::Id(id) => self.get_identifier(id, literal.info),
      _ => panic!(),
    }
  }

  fn handle_binary_expression(
    &self,
    left: Expr<'src>,
    op: TokenPair<'src>,
    right: Expr<'src>,
  ) -> ExprResult {
    let lhs = self.interpret_expression(left)?;
    let rhs = self.interpret_expression(right)?;
    match op.token {
      Token::Basic('+') => add(lhs, rhs, op.info),
      Token::Basic('-') => binary_num(lhs, rhs, op.info, |x, y| x - y),
      Token::Basic('*') => binary_num(lhs, rhs, op.info, |x, y| x * y),
      Token::Basic('/') => binary_num(lhs, rhs, op.info, |x, y| x / y),
      Token::Basic('<') => compare(lhs, rhs, op.info, |x, y| x < y, |x, y| x < y),
      Token::Basic('>') => compare(lhs, rhs, op.info, |x, y| x > y, |x, y| x > y),
      Token::Leq => compare(lhs, rhs, op.info, |x, y| x <= y, |x, y| x <= y),
      Token::Geq => compare(lhs, rhs, op.info, |x, y| x >= y, |x, y| x >= y),
      Token::Same => equal(lhs, rhs, op.info),
      Token::Different => unary_not(equal(lhs, rhs, op.info)?, op.info),
      _ => panic!(),
    }
  }

  fn handle_unary_expression(&self, op: TokenPair<'src>, right: Expr<'src>) -> ExprResult {
    let rhs = self.interpret_expression(right)?;
    match op.token {
      Token::Basic('-') => unary_minus(rhs, op.info),
      Token::Basic('!') => unary_not(rhs, op.info),
      _ => panic!(),
    }
  }

  fn interpret_expression(&self, exp: Expr<'src>) -> ExprResult {
    match exp {
      Expr::Literal { literal } => self.handle_literal_expression(literal),
      Expr::BinaryExpr {
        left,
        operator,
        right,
      } => self.handle_binary_expression(*left, operator, *right),
      Expr::UnaryExpr { operator, right } => self.handle_unary_expression(operator, *right),
    }
  }

  pub fn new() -> Self {
    Self {
      identifiers: HashMap::new(),
    }
  }

  pub fn interpret(&mut self, ast: Vec<Stmt<'src>>) -> Result<(), SourceError> {
    for stmt in ast {
      match stmt {
        Stmt::VarDecl {
          identifier,
          id_info,
          expression,
        } => self.install_identifier(identifier, id_info, expression)?,
        Stmt::Print { expression } => {
          println!("{:?}", self.interpret_expression(expression)?);
        }
        _ => {}
      }
    }
    Ok(())
  }
}
