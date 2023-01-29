mod environment;

use std::clone;
use std::fmt::Debug;

use environment::*;

use crate::ast::*;
use crate::errors::{SourceError, SourceErrorType};
use crate::lexer::{SourceInfo, Token, TokenPair};

pub trait ClonableFn {
  fn clone_box<'a>(&self) -> Box<dyn ClonableFn + 'a>
  where
    Self: 'a;

  fn call(&self, interpreter: &mut Interpreter, arguments: Vec<ExprValue>) -> InterpreterFnResult;
}

impl<T> ClonableFn for T
where
  T: Fn(&mut Interpreter, Vec<ExprValue>) -> InterpreterFnResult + Clone,
{
  fn clone_box<'a>(&self) -> Box<dyn 'a + ClonableFn>
  where
    Self: 'a,
  {
    Box::new(self.clone())
  }

  fn call(&self, interpreter: &mut Interpreter, arguments: Vec<ExprValue>) -> InterpreterFnResult {
    self(interpreter, arguments)
  }
}

impl Clone for Box<dyn ClonableFn> {
  fn clone(&self) -> Self {
    ClonableFn::clone_box(&**self) // passing self directly causes and infinite loop
  }
}

#[derive(Clone)]
struct NativeFn {
  body: Vec<StmtHandle>,
  parameters: Vec<StrHandle>,
}

impl ClonableFn for NativeFn {
  fn call(&self, interpreter: &mut Interpreter, arguments: Vec<ExprValue>) -> InterpreterFnResult {
    interpreter.interpret_function(&self.parameters, &self.body, arguments)
  }

  fn clone_box<'a>(&self) -> Box<dyn ClonableFn + 'a>
  where
    Self: 'a,
  {
    Box::new(self.clone())
  }
}

#[derive(Clone)]
pub struct InterpreterFn {
  pub arity: u32,
  pub name: String,
  pub callable: Box<dyn ClonableFn>,
}

impl Debug for InterpreterFn {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.name)
  }
}

#[derive(Debug, Clone)]
pub enum ExprValue {
  Str(String),
  Num(f64),
  Boolean(bool),
  Func(InterpreterFn),
  Null,
}

pub type ExprResult = Result<ExprValue, SourceError>;
pub type InterpreterFnResult = Result<ExprValue, ()>;

fn unary_minus(rhs: ExprValue, op_info: SourceInfo) -> ExprResult {
  if let ExprValue::Num(x) = rhs {
    Ok(ExprValue::Num(-x))
  } else {
    Err(SourceError::from_token_info(
      &op_info,
      format!("unary - cannot be applyed to operand {rhs:?}"),
      SourceErrorType::Runtime,
    ))
  }
}

fn unary_not(rhs: ExprValue, op_info: SourceInfo) -> ExprResult {
  if let ExprValue::Boolean(x) = rhs {
    Ok(ExprValue::Boolean(!x))
  } else {
    Err(SourceError::from_token_info(
      &op_info,
      format!("unary ! cannot be applyed to operand {rhs:?}"),
      SourceErrorType::Runtime,
    ))
  }
}

fn binary_num<F>(lhs: ExprValue, rhs: ExprValue, op_info: SourceInfo, op: F) -> ExprResult
where
  F: Fn(f64, f64) -> f64,
{
  match (lhs, rhs) {
    (ExprValue::Num(l), ExprValue::Num(r)) => Ok(ExprValue::Num(op(l, r))),
    (lhs, rhs) => Err(SourceError::from_token_info(
      &op_info,
      format!("operation only works for numbers, not supported for operands {lhs:?} {rhs:?}"),
      SourceErrorType::Runtime,
    )),
  }
}

fn add(lhs: ExprValue, rhs: ExprValue, op_info: SourceInfo) -> ExprResult {
  match (lhs, rhs) {
    (ExprValue::Num(l), ExprValue::Num(r)) => Ok(ExprValue::Num(l + r)),
    (ExprValue::Str(l), ExprValue::Str(r)) => Ok(ExprValue::Str(l + &r)),
    (lhs, rhs) => Err(SourceError::from_token_info(
      &op_info,
      format!("cannot add {lhs:?} and {rhs:?}"),
      SourceErrorType::Runtime,
    )),
  }
}

fn equal(lhs: ExprValue, rhs: ExprValue, op_info: SourceInfo) -> ExprResult {
  match (lhs, rhs) {
    (ExprValue::Num(l), ExprValue::Num(r)) => Ok(ExprValue::Boolean(l == r)),
    (ExprValue::Str(l), ExprValue::Str(r)) => Ok(ExprValue::Boolean(l == r)),
    (ExprValue::Boolean(l), ExprValue::Boolean(r)) => Ok(ExprValue::Boolean(l == r)),
    (lhs, rhs) => Err(SourceError::from_token_info(
      &op_info,
      format!("cannot determine if {lhs:?} and {rhs:?} are equal"),
      SourceErrorType::Runtime,
    )),
  }
}

fn compare<Nc, Sc>(
  lhs: ExprValue,
  rhs: ExprValue,
  op_info: SourceInfo,
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
      &op_info,
      format!("operation not supported for operands {lhs:?}, {rhs:?}"),
      SourceErrorType::Runtime,
    )),
  }
}

fn check_bool(val: ExprValue, info: SourceInfo) -> Result<bool, SourceError> {
  if let ExprValue::Boolean(value) = val {
    Ok(value)
  } else {
    Err(SourceError::from_token_info(
      &info,
      format!("binary operation cannot be applied to type {val:?}, only to booleans"),
      SourceErrorType::Runtime,
    ))
  }
}

pub struct Interpreter {
  env: Environment,
  ast: AST,
}

enum EarlyOut {
  Break,
  Return(ExprValue),
}

pub type IntepreterResult = Result<(), SourceError>;
type StmtRes = Result<Option<EarlyOut>, SourceError>;

impl Interpreter {
  fn install_identifier(
    &mut self,
    id: &str,
    id_info: SourceInfoHandle,
    exp_opt: Option<ExprHandle>,
  ) -> IntepreterResult {
    let value = exp_opt
      .as_ref()
      .map(|exp| self.interpret_expression(exp.clone()))
      .unwrap_or(Ok(ExprValue::Null))?;
    self
      .env
      .declare_source_identifier(id, &self.ast.get_source_info(id_info), value)
  }

  fn handle_literal_expression(&self, ast: &AST, literal: Literal) -> ExprResult {
    match literal {
      Literal::Number(num) => Ok(ExprValue::Num(num)),
      Literal::String(s) => Ok(ExprValue::Str(self.ast.get_str(s).to_string())),
      Literal::True => Ok(ExprValue::Boolean(true)),
      Literal::False => Ok(ExprValue::Boolean(false)),
      Literal::Null => Ok(ExprValue::Null),
      _ => panic!(),
    }
  }

  fn handle_binary_expression(
    &mut self,
    left: ExprHandle,
    op: Operator,
    op_src_info: SourceInfo,
    right: ExprHandle,
  ) -> ExprResult {
    let lhs = self.interpret_expression(left)?;
    let rhs = self.interpret_expression(right)?;
    match op {
      Operator::Basic('+') => add(lhs, rhs, op_src_info),
      Operator::Basic('-') => binary_num(lhs, rhs, op_src_info, |x, y| x - y),
      Operator::Basic('*') => binary_num(lhs, rhs, op_src_info, |x, y| x * y),
      Operator::Basic('/') => binary_num(lhs, rhs, op_src_info, |x, y| x / y),
      Operator::Basic('<') => compare(lhs, rhs, op_src_info, |x, y| x < y, |x, y| x < y),
      Operator::Basic('>') => compare(lhs, rhs, op_src_info, |x, y| x > y, |x, y| x > y),
      Operator::Leq => compare(lhs, rhs, op_src_info, |x, y| x <= y, |x, y| x <= y),
      Operator::Geq => compare(lhs, rhs, op_src_info, |x, y| x >= y, |x, y| x >= y),
      Operator::Same => equal(lhs, rhs, op_src_info),
      Operator::Different => unary_not(equal(lhs, rhs, op_src_info)?, op_src_info),
      _ => panic!(),
    }
  }

  fn handle_logical_expression(
    &mut self,
    left: ExprHandle,
    op: Operator,
    op_src_info: SourceInfo,
    right: ExprHandle,
  ) -> ExprResult {
    let lhs = check_bool(self.interpret_expression(left)?, op_src_info)?;
    match op {
      Operator::And => Ok(ExprValue::Boolean(
        lhs && check_bool(self.interpret_expression(right)?, op_src_info)?,
      )),
      Operator::Or => Ok(ExprValue::Boolean(
        lhs || check_bool(self.interpret_expression(right)?, op_src_info)?,
      )),
      _ => panic!(),
    }
  }

  fn handle_unary_expression(
    &mut self,
    op: Operator,
    op_src_info: SourceInfo,
    right: ExprHandle,
  ) -> ExprResult {
    let rhs = self.interpret_expression(right)?;
    match op {
      Operator::Basic('-') => unary_minus(rhs, op_src_info),
      Operator::Basic('!') => unary_not(rhs, op_src_info),
      _ => panic!(),
    }
  }

  fn handle_function_call(
    &mut self,
    func: ExprHandle,
    call_info: SourceInfoHandle,
    arguments: Vec<ExprHandle>,
  ) -> ExprResult {
    let mut argument_values = Vec::new();
    for arg in arguments {
      argument_values.push(self.interpret_expression(arg)?);
    }
    let func_val = self.interpret_expression(func)?;
    if let ExprValue::Func(func) = func_val {
      if func.arity as usize == argument_values.len() {
        func.callable.call(self, argument_values).map_err(|_| {
          SourceError::from_token_info(
            &self.ast.get_source_info(call_info),
            "function call error".to_string(),
            SourceErrorType::Runtime,
          )
        })
      } else {
        Err(SourceError::from_token_info(
          &self.ast.get_source_info(call_info),
          format!(
            "incorrect number of arguments ({}) for function {} (arity: {})",
            argument_values.len(),
            func.name,
            func.arity
          ),
          SourceErrorType::Runtime,
        ))
      }
    } else {
      Err(SourceError::from_token_info(
        &self.ast.get_source_info(call_info),
        format!("cannot call {func_val:?}, only functions"),
        SourceErrorType::Runtime,
      ))
    }
  }

  fn interpret_expression(&mut self, exp: ExprHandle) -> ExprResult {
    match self.ast.get_expression(exp) {
      Expr::Literal { literal, info: _ } => self.handle_literal_expression(&self.ast, literal),
      Expr::BinaryExpr {
        left,
        operator,
        right,
      } => self.handle_binary_expression(
        left,
        operator.op,
        self.ast.get_source_info(operator.src_info),
        right,
      ),
      Expr::Logical {
        left,
        operator,
        right,
      } => self.handle_logical_expression(
        left,
        operator.op,
        self.ast.get_source_info(operator.src_info),
        right,
      ),
      Expr::UnaryExpr { operator, right } => self.handle_unary_expression(
        operator.op,
        self.ast.get_source_info(operator.src_info),
        right,
      ),
      Expr::Variable { id, id_info } => self
        .env
        .get_id_value(self.ast.get_str(id), &self.ast.get_source_info(id_info)),
      Expr::Assignment {
        name,
        name_info,
        value,
      } => {
        let rhs = self.interpret_expression(value)?;
        self.env.assign(
          self.ast.get_str(name),
          self.ast.get_source_info(name_info),
          rhs,
        )
      }
      Expr::FnCall {
        func,
        call_info,
        arguments,
      } => self.handle_function_call(func, call_info, arguments),
    }
  }

  fn interpret_if_branch(
    &mut self,
    if_info: SourceInfoHandle,
    condition: ExprHandle,
    true_branch: StmtHandle,
    false_branch: Option<StmtHandle>,
  ) -> StmtRes {
    if let ExprValue::Boolean(value) = self.interpret_expression(condition)? {
      if value {
        self.interpret_statement(true_branch)
      } else if let Some(branch) = false_branch {
        self.interpret_statement(branch)
      } else {
        Ok(None)
      }
    } else {
      Err(SourceError::from_token_info(
        &self.ast.get_source_info(if_info),
        "if condition has to evaluate to boolean".to_string(),
        SourceErrorType::Runtime,
      ))
    }
  }

  fn interpret_while_loop(
    &mut self,
    info: SourceInfoHandle,
    condition: ExprHandle,
    body: StmtHandle,
  ) -> StmtRes {
    loop {
      match self.interpret_expression(condition.clone())? {
        ExprValue::Boolean(val) => {
          if val {
            if let Some(EarlyOut::Break) = self.interpret_statement(body.clone())? {
              break;
            }
          } else {
            break;
          }
        }
        val => {
          return Err(SourceError::from_token_info(
            &self.ast.get_source_info(info),
            format!("while condition has to evaluate to a boolean, got {val:?}"),
            SourceErrorType::Runtime,
          ))
        }
      }
    }
    Ok(None)
  }

  fn interpret_function(
    &mut self,
    parameters: &[StrHandle],
    body: &[StmtHandle],
    arguments: Vec<ExprValue>,
  ) -> InterpreterFnResult {
    self.env.push();
    for (param, arg) in parameters.iter().zip(arguments) {
      self
        .env
        .declare_source_identifier(
          self.ast.get_str(param.clone()),
          &SourceInfo {
            line_no: 0,
            line_start: 0,
            start: 0,
            end: 0,
          },
          arg,
        )
        .or(Err(()))?;
    }
    for stmt in body.iter().cloned() {
      if let Some(EarlyOut::Return(val)) = self.interpret_statement(stmt).or(Err(()))? {
        return Ok(val);
      };
    }
    self.env.pop();
    Ok(ExprValue::Null)
  }

  fn interpret_statement(&mut self, stmt: StmtHandle) -> StmtRes {
    match self.ast.get_statement(stmt) {
      Stmt::VarDecl {
        identifier,
        id_info,
        expression,
      } => {
        let id = self.ast.get_str(identifier).to_string();
        self.install_identifier(&id, id_info, expression)?
      }
      Stmt::Print(expression) => {
        println!("{:?}", self.interpret_expression(expression)?);
      }
      Stmt::Expr(expr) => {
        self.interpret_expression(expr)?;
      }
      Stmt::Block(stmts) => {
        self.env.push();
        for stmt in stmts {
          let res = self.interpret_statement(stmt)?;
          if res.is_some() {
            return Ok(res);
          }
        }
        self.env.pop();
      }
      Stmt::IfBranch {
        if_info,
        condition,
        true_branch,
        else_branch,
      } => {
        self.interpret_if_branch(if_info, condition, true_branch, else_branch)?;
      }
      Stmt::While {
        info,
        condition,
        loop_body,
      } => {
        self.interpret_while_loop(info, condition, loop_body)?;
      }
      Stmt::Break => return Ok(Some(EarlyOut::Break)),
      Stmt::Function {
        name,
        name_info,
        parameters,
        body,
      } => {
        let arity = parameters.len() as u32;
        let func = NativeFn { body, parameters };
        let interpreter_fn = InterpreterFn {
          arity,
          name: self.ast.get_str(name.clone()).to_string(),
          callable: Box::new(func),
        };
        self.env.declare_source_identifier(
          self.ast.get_str(name),
          &self.ast.get_source_info(name_info),
          ExprValue::Func(interpreter_fn),
        )?;
      }
      Stmt::Return(expr) => return Ok(Some(EarlyOut::Return(self.interpret_expression(expr)?))),
      _ => panic!(),
    };
    Ok(None)
  }

  pub fn interpret(ast: AST) -> Result<(), SourceError> {
    let mut interpreter = Self {
      env: Environment::global(),
      ast,
    };
    for stmt in interpreter.ast.get_program().clone() {
      interpreter.interpret_statement(stmt.clone())?;
    }
    Ok(())
  }

  pub fn add_native_function(&mut self, name: String, func: InterpreterFn) {
    self
      .env
      .declare_native_identifier(name, ExprValue::Func(func));
  }
}
