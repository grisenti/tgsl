use std::borrow::Cow;
use std::fmt::Display;

use super::{global_env::GlobalEnv, identifier::StructId};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
  // signature has always at least one element, the return type.
  signature: Vec<Type>,
}

impl Function {
  pub fn new(parameters: Vec<Type>, return_type: Type) -> Self {
    let mut signature = parameters;
    signature.push(return_type);
    Self { signature }
  }

  pub fn get_return_type(&self) -> &Type {
    self.signature.last().unwrap() // from invariant
  }

  pub fn get_parameters(&self) -> &[Type] {
    &self.signature[0..self.signature.len()]
  }

  pub fn into_parts(mut self) -> (Vec<Type>, Type) {
    let return_type = self.signature.pop().unwrap();
    (self.signature, return_type)
  }
}

impl From<Function> for Type {
  fn from(value: Function) -> Self {
    Type::Function(value)
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
  Any,
  Str,
  Num,
  Bool,
  Struct(StructId),
  Function(Function),
  Unknown,
  Nothing,
  Error,
}

impl Type {
  pub fn print_pretty(&self) -> String {
    match self {
      Type::Function(function) => {
        let parameters = function
          .get_parameters()
          .iter()
          .map(|val| val.print_pretty())
          .collect::<Vec<_>>()
          .join(" ,");
        let return_type = function.get_return_type().print_pretty();
        format!("fn ({}) -> {}", parameters, return_type)
      }
      other => format!("{:?}", self),
    }
  }
}
