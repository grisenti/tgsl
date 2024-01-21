use std::fmt;
use std::fmt::Formatter;

use crate::compiler::errors::{CompilerError, ErrorPrinter};

pub trait UserError {
  fn message(&self) -> String;
}

impl UserError for &str {
  fn message(&self) -> String {
    self.to_string()
  }
}

pub enum RuntimeError {
  StackOverflow,
  MissingContext,
  Panic(String),
  User(Box<dyn UserError>),
}

impl From<Box<dyn UserError>> for RuntimeError {
  fn from(user_error: Box<dyn UserError>) -> Self {
    RuntimeError::User(user_error)
  }
}

impl fmt::Debug for RuntimeError {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      RuntimeError::StackOverflow => write!(f, "stack overflow"),
      RuntimeError::MissingContext => write!(f, "context missing in function call"),
      RuntimeError::Panic(message) => write!(f, "panic: \"{message}\""),
      RuntimeError::User(user_error) => write!(f, "user error: {}", user_error.message()),
    }
  }
}

pub enum ForeignBindingError {
  InconsistentReturnType {
    source_function: String,
    provided_return_type: String,
  },
  FunctionNameNotInSource(String),
  MissingForeignFunction,
}

impl fmt::Debug for ForeignBindingError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ForeignBindingError::InconsistentReturnType {
        source_function,
        provided_return_type,
      } => {
        write!(
          f,
          "provided return type ({provided_return_type}), does not match function '{source_function}'"
        )
      }
      ForeignBindingError::FunctionNameNotInSource(function) => {
        write!(f, "no function {function} declared in the program source")
      }
      ForeignBindingError::MissingForeignFunction => write!(f, "missing foreign function"),
    }
  }
}

pub enum LoadModuleError {
  CompilerErrors(Vec<CompilerError>),
  RuntimeError(RuntimeError),
  ForeignBindingErrors(Vec<ForeignBindingError>),
}

impl fmt::Debug for LoadModuleError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      LoadModuleError::CompilerErrors(compiler_errors) => {
        for err in compiler_errors {
          writeln!(f, "{err:?}")?;
        }
      }
      LoadModuleError::RuntimeError(runtime_error) => {
        write!(f, "{runtime_error:?}")?;
      }
      LoadModuleError::ForeignBindingErrors(foreign_binding_errors) => {
        for err in foreign_binding_errors {
          writeln!(f, "{err:?}")?;
        }
      }
    }
    Ok(())
  }
}

impl LoadModuleError {
  pub fn detailed(&self, source: &str) -> String {
    match self {
      LoadModuleError::CompilerErrors(compiler_errors) => {
        ErrorPrinter::to_string(compiler_errors, source)
      }
      _ => format!("{self:?}"),
    }
  }
}
