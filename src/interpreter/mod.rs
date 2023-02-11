mod class;
mod environment;
mod expression_interpreter;
mod statement_interpreter;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Rc;

use crate::compiler::{ast::*, CompilerResult};
use crate::errors::SourceInfo;
use crate::errors::{SourceError, SourceErrorType};

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
  arity: u32,
  callable: Box<dyn ClonableFn>,
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

  pub fn foreign<F>(func: F, arity: u32) -> Self
  where
    F: Fn(&mut Interpreter, Vec<ExprValue>) -> InterpreterFnResult + Clone + 'static,
  {
    Self {
      arity,
      callable: Box::new(func),
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
  Func(Rc<InterpreterFn>),
  PartialCall {
    func: Rc<InterpreterFn>,
    args: Vec<ExprValue>,
  },
  ClassInstance(ClassInstance),
  Null,

  Undefined,
}

pub type ExprResult = Result<ExprValue, SourceError>;
pub type InterpreterFnResult = Result<ExprValue, SourceError>;

pub struct Interpreter {
  env: EnvRef,
  ast: AST,
  global_names: HashMap<String, Identifier>,
}

enum EarlyOut {
  Break,
  Return(ExprValue),
}

pub type IntepreterResult = Result<(), SourceError>;
type StmtRes = Result<Option<EarlyOut>, SourceError>;

impl Interpreter {
  pub fn runtime_error(info: &SourceInfo, error_msg: String) -> SourceError {
    SourceError::from_source_info(info, error_msg, SourceErrorType::Runtime)
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
      .map(|exp| self.interpret_expression(*exp))
      .unwrap_or(Ok(ExprValue::Undefined))?;
    self.env.borrow_mut().set(id, value);
    Ok(())
  }

  pub fn new(compiler_result: CompilerResult) -> Self {
    Self {
      env: Environment::global(),
      ast: compiler_result.ast,
      global_names: compiler_result.global_environment,
    }
  }

  pub fn add_global_variable(&mut self, name: &str, value: ExprValue) {
    if let Some(id) = self.global_names.get(name) {
      self.env.borrow_mut().set(*id, value);
    }
  }

  pub fn interpret(&mut self) -> Result<(), SourceError> {
    for stmt in self.ast.get_program().clone() {
      self.interpret_statement(stmt)?;
    }
    Ok(())
  }
}
