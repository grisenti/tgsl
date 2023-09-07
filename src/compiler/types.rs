use super::identifier::StructId;
use crate::compiler::ast::parsed_type::ParsedFunctionType;
use crate::compiler::ast::visitor::ParsedTypeVisitor;
use crate::compiler::ast::AST;
use crate::compiler::identifier::OverloadId;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionSignature {
  // signature has always at least one element, the return type.
  signature: Vec<Type>,
}

impl FunctionSignature {
  pub fn new(parameters: Vec<Type>, return_type: Type) -> Self {
    let mut signature = parameters;
    signature.push(return_type);
    Self { signature }
  }

  pub fn get_return_type(&self) -> &Type {
    self.signature.last().unwrap() // from invariant
  }

  pub fn get_parameters(&self) -> &[Type] {
    &self.signature[0..self.signature.len() - 1]
  }

  pub fn into_parts(mut self) -> (Vec<Type>, Type) {
    let return_type = self.signature.pop().unwrap();
    (self.signature, return_type)
  }
}

impl From<FunctionSignature> for Type {
  fn from(value: FunctionSignature) -> Self {
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
  Function(FunctionSignature),
  UnresolvedOverload(OverloadId),
  Unknown,
  Nothing,
  Error,
}

impl Type {
  pub fn is_error(&self) -> bool {
    *self == Type::Error
  }

  pub fn print_pretty(&self) -> String {
    match self {
      Type::Function(signature) => {
        let parameters = signature
          .get_parameters()
          .iter()
          .map(|val| val.print_pretty())
          .collect::<Vec<_>>()
          .join(" ,");
        let return_type = signature.get_return_type().print_pretty();
        format!("fn ({}) -> {}", parameters, return_type)
      }
      other => format!("{:?}", other),
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
