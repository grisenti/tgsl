mod class;
mod environment;
mod expression_interpreter;
mod statement_interpreter;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Rc;

use crate::ast::*;
use crate::errors::{SourceError, SourceErrorType};
use crate::lexer::SourceInfo;

use self::class::ClassInstance;
use self::environment::{EnvRef, Environment};

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
  parameters: Vec<Identifier>,
  capture: EnvRef,
}

impl ClonableFn for NativeFn {
  fn call(&self, interpreter: &mut Interpreter, arguments: Vec<ExprValue>) -> InterpreterFnResult {
    interpreter.interpret_function(
      &self.parameters,
      &self.body,
      arguments,
      self.capture.clone(),
    )
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
  pub callable: Box<dyn ClonableFn>,
}

impl InterpreterFn {
  pub fn native(func: Method, capture: EnvRef) -> Self {
    Self {
      arity: func.parameters.len() as u32,
      callable: Box::new(NativeFn {
        body: func.body,
        parameters: func.parameters,
        capture,
      }),
    }
  }
}

impl Debug for InterpreterFn {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "function<{}>", self.arity)
  }
}

#[derive(Debug, Clone)]
pub enum ExprValue {
  Str(String),
  Num(f64),
  Boolean(bool),
  Func(InterpreterFn),
  ClassInstance(ClassInstance),
  Null,
}

pub type ExprResult = Result<ExprValue, SourceError>;
pub type InterpreterFnResult = Result<ExprValue, SourceError>;

pub struct Interpreter {
  env: EnvRef,
  ast: AST,
}

enum EarlyOut {
  Break,
  Return(ExprValue),
}

pub type IntepreterResult = Result<(), SourceError>;
type StmtRes = Result<Option<EarlyOut>, SourceError>;

impl Interpreter {
  pub fn runtime_error(info: &SourceInfo, error_msg: String) -> SourceError {
    SourceError::from_token_info(info, error_msg, SourceErrorType::Runtime)
  }

  fn new_scope(&mut self) {
    let new_env = Environment::inner_scope(self.env.clone());
    self.env = new_env;
  }

  fn set_env(&mut self, env: EnvRef) -> EnvRef {
    let old = self.env.clone();
    self.env = env;
    old
  }

  fn pop_scope(&mut self) {
    let parent = self.env.borrow().parent();
    self.env = parent;
  }

  fn install_identifier(
    &mut self,
    id: Identifier,
    exp_opt: Option<ExprHandle>,
  ) -> IntepreterResult {
    let value = exp_opt
      .as_ref()
      .map(|exp| self.interpret_expression(exp.clone()))
      .unwrap_or(Ok(ExprValue::Null))?;
    self.env.borrow_mut().set(id, value);
    Ok(())
  }

  pub fn new(ast: AST) -> Self {
    Self {
      env: Environment::global(),
      ast,
    }
  }

  pub fn interpret(&mut self) -> Result<(), SourceError> {
    for stmt in self.ast.get_program().clone() {
      self.interpret_statement(stmt.clone())?;
    }
    Ok(())
  }
}
