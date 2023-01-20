use std::sync::mpsc::RecvTimeoutError;

use crate::ast::Expr;
use crate::lexer::Token;

pub enum RetVal {
  Str(String),
  Num(f64),
}

type IntepreterResult = Result<RetVal, ()>;

fn nums_or_error(lhs: &RetVal, rhs: &RetVal) -> Result<(f64, f64), ()> {
  if let (RetVal::Num(l), RetVal::Num(r)) = (lhs, rhs) {
    Ok((*l, *r))
  } else {
    Err(())
  }
}

fn plus(lhs: &RetVal, rhs: &RetVal) -> IntepreterResult {
  let (x, y) = nums_or_error(&lhs, &rhs)?;
  Ok(RetVal::Num(x + y))
}

fn minus(lhs: &RetVal, rhs: &RetVal) -> IntepreterResult {
  let (x, y) = nums_or_error(&lhs, &rhs)?;
  Ok(RetVal::Num(x - y))
}

fn times(lhs: &RetVal, rhs: &RetVal) -> IntepreterResult {
  let (x, y) = nums_or_error(&lhs, &rhs)?;
  Ok(RetVal::Num(x * y))
}

fn divide(lhs: &RetVal, rhs: &RetVal) -> IntepreterResult {
  let (x, y) = nums_or_error(&lhs, &rhs)?;
  Ok(RetVal::Num(x / y))
}

fn unary_minus(rhs: &RetVal) -> IntepreterResult {
  if let RetVal::Num(x) = rhs {
    Ok(RetVal::Num(-x))
  } else {
    Err(())
  }
}

pub fn interpret(exp: Expr) -> IntepreterResult {
  match exp {
    Expr::Literal { literal } => match literal.token {
      Token::Number(num) => Ok(RetVal::Num(num)),
      _ => Err(()),
    },
    Expr::BinaryExpr {
      left,
      operator,
      right,
    } => {
      let lhs = interpret(*left)?;
      let rhs = interpret(*right)?;
      match operator.token {
        Token::Basic('+') => plus(&lhs, &rhs),
        Token::Basic('-') => minus(&lhs, &rhs),
        Token::Basic('*') => times(&lhs, &rhs),
        Token::Basic('/') => divide(&lhs, &rhs),
        _ => panic!(),
      }
    }
    Expr::UnaryExpr { operator, right } => {
      let rhs = interpret(*right)?;
      match operator.token {
        Token::Basic('-') => unary_minus(&rhs),
        _ => panic!(),
      }
    }
  }
}
