use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionSignature {
  // signature has always at least one element, the return type.
  signature: Rc<[Type]>,
}

pub fn parameter_types_to_string(parameters: &[Type]) -> String {
  parameters
    .iter()
    .map(|val| format!("{val}"))
    .collect::<Vec<_>>()
    .join(" ,")
}

impl FunctionSignature {
  pub fn new(parameters: Vec<Type>, return_type: Type) -> Self {
    let mut signature = parameters;
    signature.push(return_type);
    Self {
      signature: Rc::from(signature),
    }
  }

  pub fn get_return_type(&self) -> &Type {
    self.signature.last().unwrap() // from invariant
  }

  pub fn get_parameters(&self) -> &[Type] {
    &self.signature[0..self.signature.len() - 1]
  }
}

impl Display for FunctionSignature {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "fn ({}) -> {}",
      parameter_types_to_string(self.get_parameters()),
      self.get_return_type()
    )
  }
}

impl From<FunctionSignature> for Type {
  fn from(value: FunctionSignature) -> Self {
    Type::Function(value)
  }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Type {
  Any,
  Str,
  Num,
  Bool,
  Struct { module_name: Rc<str>, name: Rc<str> },
  Function(FunctionSignature),
  UnresolvedOverload,
  Unknown,
  Nothing,
  Error,
}

impl Type {
  pub const ANONYMOUS_MODULE: &'static str = "<this>";

  pub fn is_error(&self) -> bool {
    *self == Type::Error
  }
}

impl Display for Type {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Type::Function(signature) => write!(f, "{signature}"),
      Type::Struct { module_name, name } => write!(f, "{module_name}::{name}"),
      other => write!(f, "{}", format!("{other:?}").to_lowercase()),
    }
  }
}

#[cfg(test)]
mod test {
  use json::JsonValue;

  use crate::compiler::types::Type;

  impl PartialEq<JsonValue> for Type {
    fn eq(&self, other: &JsonValue) -> bool {
      format!("{:?}", self) == *other
    }
  }
}
