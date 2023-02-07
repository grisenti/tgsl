use std::any::Any;

use super::Literal;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UserTypeId {
  pub id: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
  Str,
  Num,
  Bool,
  User(UserTypeId),
  Function(Vec<Type>),

  Any,
  Undefined,
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
