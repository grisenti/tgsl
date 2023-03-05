use super::{ast::Literal, identifier::Identifier};

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
  Function(Vec<Type>),
  Unknown,
  Nothing,
  Error,
}

pub struct TypeId(pub u32);

impl Type {
  pub fn from_literal(lit: Literal) -> Self {
    match lit {
      Literal::String(_) => Type::Str,
      Literal::Number(_) => Type::Num,
      Literal::True | Literal::False => Type::Bool,
      Literal::Null => Type::Unknown,
    }
  }
}
