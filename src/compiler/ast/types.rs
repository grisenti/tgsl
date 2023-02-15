use super::{Identifier, Literal};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
  Str,
  Num,
  Bool,
  Struct(Identifier),
  PartialCall {
    func_types: Vec<Type>,
    partial_arguments: Vec<Type>,
  },
  FunctionType(Vec<Type>),
  Any,
  Undefined,
  Error,
}

impl Type {
  pub fn from_literal(lit: Literal) -> Self {
    match lit {
      Literal::String(_) => Type::Str,
      Literal::Number(_) => Type::Num,
      Literal::True | Literal::False => Type::Bool,
      Literal::Null => Type::Any,
    }
  }
}
