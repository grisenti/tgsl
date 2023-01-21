use std::ops::AddAssign;

use crate::ast::Expr;
use crate::errors::{SourceError, SourceErrorType};
use crate::lexer::{Token, TokenInfo};

#[derive(Debug)]
pub enum RetVal {
  Str(String),
  Num(f64),
  Boolean(bool),
  Null,
}

type IntepreterResult = Result<RetVal, SourceError>;

fn unary_minus(rhs: RetVal, op_info: TokenInfo) -> IntepreterResult {
  if let RetVal::Num(x) = rhs {
    Ok(RetVal::Num(-x))
  } else {
    Err(SourceError::from_token_info(
      op_info,
      format!("unary - cannot be applyed to operand {:?}", rhs),
      SourceErrorType::Runtime,
    ))
  }
}

fn unary_not(rhs: RetVal, op_info: TokenInfo) -> IntepreterResult {
  if let RetVal::Boolean(x) = rhs {
    Ok(RetVal::Boolean(!x))
  } else {
    Err(SourceError::from_token_info(
      op_info,
      format!("unary ! cannot be applyed to operand {:?}", rhs),
      SourceErrorType::Runtime,
    ))
  }
}

fn binary_num<F>(lhs: RetVal, rhs: RetVal, op_info: TokenInfo, op: F) -> IntepreterResult
where
  F: Fn(f64, f64) -> f64,
{
  match (lhs, rhs) {
    (RetVal::Num(l), RetVal::Num(r)) => Ok(RetVal::Num(op(l, r))),
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

fn add(lhs: RetVal, rhs: RetVal, op_info: TokenInfo) -> IntepreterResult {
  match (lhs, rhs) {
    (RetVal::Num(l), RetVal::Num(r)) => Ok(RetVal::Num(l + r)),
    (RetVal::Str(l), RetVal::Str(r)) => Ok(RetVal::Str(l + &r)),
    (lhs, rhs) => Err(SourceError::from_token_info(
      op_info,
      format!("cannot add {:?} and {:?}", lhs, rhs),
      SourceErrorType::Runtime,
    )),
  }
}

fn equal(lhs: RetVal, rhs: RetVal, op_info: TokenInfo) -> IntepreterResult {
  match (lhs, rhs) {
    (RetVal::Num(l), RetVal::Num(r)) => Ok(RetVal::Boolean(l == r)),
    (RetVal::Str(l), RetVal::Str(r)) => Ok(RetVal::Boolean(l == r)),
    (RetVal::Boolean(l), RetVal::Boolean(r)) => Ok(RetVal::Boolean(l == r)),
    (lhs, rhs) => Err(SourceError::from_token_info(
      op_info,
      format!("cannot determine if {:?} and {:?} are equal", lhs, rhs),
      SourceErrorType::Runtime,
    )),
  }
}

fn compare<Nc, Sc>(
  lhs: RetVal,
  rhs: RetVal,
  op_info: TokenInfo,
  num_cmp: Nc,
  str_cmp: Sc,
) -> IntepreterResult
where
  Sc: Fn(&str, &str) -> bool,
  Nc: Fn(f64, f64) -> bool,
{
  match (lhs, rhs) {
    (RetVal::Num(l), RetVal::Num(r)) => Ok(RetVal::Boolean(num_cmp(l, r))),
    (RetVal::Str(l), RetVal::Str(r)) => Ok(RetVal::Boolean(str_cmp(&l, &r))),
    (lhs, rhs) => Err(SourceError::from_token_info(
      op_info,
      format!("operation not supported for operands {:?}, {:?}", lhs, rhs),
      SourceErrorType::Runtime,
    )),
  }
}

pub fn interpret(exp: Expr) -> IntepreterResult {
  match exp {
    Expr::Literal { literal } => match literal.token {
      Token::Number(num) => Ok(RetVal::Num(num)),
      Token::String(s) => Ok(RetVal::Str(s.to_string())),
      Token::True => Ok(RetVal::Boolean(true)),
      Token::False => Ok(RetVal::Boolean(false)),
      Token::Null => Ok(RetVal::Null),
      _ => panic!(),
    },
    Expr::BinaryExpr {
      left,
      operator,
      right,
    } => {
      let lhs = interpret(*left)?;
      let rhs = interpret(*right)?;
      match operator.token {
        Token::Basic('+') => add(lhs, rhs, operator.info),
        Token::Basic('-') => binary_num(lhs, rhs, operator.info, |x, y| x - y),
        Token::Basic('*') => binary_num(lhs, rhs, operator.info, |x, y| x * y),
        Token::Basic('/') => binary_num(lhs, rhs, operator.info, |x, y| x / y),
        Token::Basic('<') => compare(lhs, rhs, operator.info, |x, y| x < y, |x, y| x < y),
        Token::Basic('>') => compare(lhs, rhs, operator.info, |x, y| x > y, |x, y| x > y),
        Token::Leq => compare(lhs, rhs, operator.info, |x, y| x <= y, |x, y| x <= y),
        Token::Geq => compare(lhs, rhs, operator.info, |x, y| x >= y, |x, y| x >= y),
        Token::Same => equal(lhs, rhs, operator.info),
        Token::Different => unary_not(equal(lhs, rhs, operator.info)?, operator.info),
        _ => panic!(),
      }
    }
    Expr::UnaryExpr { operator, right } => {
      let rhs = interpret(*right)?;
      match operator.token {
        Token::Basic('-') => unary_minus(rhs, operator.info),
        Token::Basic('!') => unary_not(rhs, operator.info),
        _ => panic!(),
      }
    }
  }
}
