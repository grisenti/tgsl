use crate::ast::Expr;
use crate::errors::{ErrorType, SourceError};
use crate::lexer::{Token, TokenInfo};

pub enum RetVal {
  Str(String),
  Num(f64),
}

type IntepreterResult = Result<RetVal, SourceError>;

fn nums_or_error(
  lhs: &RetVal,
  rhs: &RetVal,
  op_info: TokenInfo,
) -> Result<(f64, f64), SourceError> {
  if let (RetVal::Num(l), RetVal::Num(r)) = (lhs, rhs) {
    Ok((*l, *r))
  } else {
    Err(SourceError::from_token_info(
      op_info,
      "operator only works for numbers".to_string(),
      ErrorType::Runtime,
    ))
  }
}

fn plus(lhs: &RetVal, rhs: &RetVal, op_info: TokenInfo) -> IntepreterResult {
  let (x, y) = nums_or_error(&lhs, &rhs, op_info)?;
  Ok(RetVal::Num(x + y))
}

fn minus(lhs: &RetVal, rhs: &RetVal, op_info: TokenInfo) -> IntepreterResult {
  let (x, y) = nums_or_error(&lhs, &rhs, op_info)?;
  Ok(RetVal::Num(x - y))
}

fn times(lhs: &RetVal, rhs: &RetVal, op_info: TokenInfo) -> IntepreterResult {
  let (x, y) = nums_or_error(&lhs, &rhs, op_info)?;
  Ok(RetVal::Num(x * y))
}

fn divide(lhs: &RetVal, rhs: &RetVal, op_info: TokenInfo) -> IntepreterResult {
  let (x, y) = nums_or_error(&lhs, &rhs, op_info)?;
  Ok(RetVal::Num(x / y))
}

fn unary_minus(rhs: &RetVal, op_info: TokenInfo) -> IntepreterResult {
  if let RetVal::Num(x) = rhs {
    Ok(RetVal::Num(-x))
  } else {
    panic!()
  }
}

pub fn interpret(exp: Expr) -> IntepreterResult {
  match exp {
    Expr::Literal { literal } => match literal.token {
      Token::Number(num) => Ok(RetVal::Num(num)),
      Token::String(s) => Ok(RetVal::Str(s.to_string())),
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
        Token::Basic('+') => plus(&lhs, &rhs, operator.info),
        Token::Basic('-') => minus(&lhs, &rhs, operator.info),
        Token::Basic('*') => times(&lhs, &rhs, operator.info),
        Token::Basic('/') => divide(&lhs, &rhs, operator.info),
        _ => panic!(),
      }
    }
    Expr::UnaryExpr { operator, right } => {
      let rhs = interpret(*right)?;
      match operator.token {
        Token::Basic('-') => unary_minus(&rhs, operator.info),
        _ => panic!(),
      }
    }
  }
}
