mod environment;

use environment::*;

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

pub type ExprResult = Result<ExprValue, SourceError>;

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
  env: Environment<'src>,
}

pub type IntepreterResult = Result<(), SourceError>;

impl<'src> Interpreter<'src> {
  fn install_identifier(
    &mut self,
    id: &'src str,
    id_info: TokenInfo,
    exp_opt: Option<Expr<'src>>,
  ) -> IntepreterResult {
    let value = exp_opt
      .map(|exp| self.interpret_expression(exp))
      .unwrap_or(Ok(ExprValue::Null))?;
    self.env.declare_identifier(id, id_info, value)
  }

  fn handle_literal_expression(&self, literal: TokenPair<'src>) -> ExprResult {
    match literal.token {
      Token::Number(num) => Ok(ExprValue::Num(num)),
      Token::String(s) => Ok(ExprValue::Str(s.to_string())),
      Token::True => Ok(ExprValue::Boolean(true)),
      Token::False => Ok(ExprValue::Boolean(false)),
      Token::Null => Ok(ExprValue::Null),
      _ => panic!(),
    }
  }

  fn handle_binary_expression(
    &mut self,
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

  fn handle_unary_expression(&mut self, op: TokenPair<'src>, right: Expr<'src>) -> ExprResult {
    let rhs = self.interpret_expression(right)?;
    match op.token {
      Token::Basic('-') => unary_minus(rhs, op.info),
      Token::Basic('!') => unary_not(rhs, op.info),
      _ => panic!(),
    }
  }

  fn interpret_expression(&mut self, exp: Expr<'src>) -> ExprResult {
    match exp {
      Expr::Literal(literal) => self.handle_literal_expression(literal),
      Expr::BinaryExpr {
        left,
        operator,
        right,
      } => self.handle_binary_expression(*left, operator, *right),
      Expr::UnaryExpr { operator, right } => self.handle_unary_expression(operator, *right),
      Expr::Variable { id, id_info } => self.env.get_id_value(id, id_info),
      Expr::Assignment {
        name,
        name_info,
        value,
      } => {
        let rhs = self.interpret_expression(*value)?;
        self.env.assign(name, name_info, rhs)
      }
    }
  }

  fn interpret_if_branch(
    &mut self,
    condition: Expr<'src>,
    true_branch: Stmt<'src>,
    false_branch: Option<Stmt<'src>>,
  ) -> Result<(), SourceError> {
    if let ExprValue::Boolean(value) = self.interpret_expression(condition)? {
      if value {
        self.interpret_statement(true_branch)
      } else if let Some(branch) = false_branch {
        self.interpret_statement(branch)
      } else {
        Ok(())
      }
    } else {
      panic!()
    }
  }

  fn interpret_statement(&mut self, stmt: Stmt<'src>) -> Result<(), SourceError> {
    match stmt {
      Stmt::VarDecl {
        identifier,
        id_info,
        expression,
      } => self.install_identifier(identifier, id_info, expression)?,
      Stmt::Print { expression } => {
        println!("{:?}", self.interpret_expression(expression)?);
      }
      Stmt::Expr(expr) => {
        self.interpret_expression(expr)?;
      }
      Stmt::Block(stmts) => {
        self.env.push();
        for stmt in stmts {
          self.interpret_statement(stmt)?;
        }
        self.env.pop();
      }
      Stmt::IfBranch {
        condition,
        true_branch,
        else_branch,
      } => {
        self.interpret_if_branch(
          condition,
          *true_branch,
          else_branch.and_then(|branch| Some(*branch)),
        )?;
      }
    };
    Ok(())
  }

  pub fn new() -> Self {
    Self {
      env: Environment::global(),
    }
  }

  pub fn interpret(&mut self, program: ASTNode<'src>) -> Result<(), SourceError> {
    match program {
      ASTNode::Expr(exp) => print!("{:?}", self.interpret_expression(exp)?),
      ASTNode::Stmt(stmt) => self.interpret_statement(stmt)?,
      ASTNode::Program(stmts) => {
        for s in stmts {
          self.interpret_statement(s)?;
        }
      }
    }
    Ok(())
  }
}
