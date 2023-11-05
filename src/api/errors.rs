use std::fmt;

use crate::compiler::errors::{CompilerError, ErrorPrinter};

#[derive(Debug)]
pub enum RuntimeError {
  StackOverflow,
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