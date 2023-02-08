use std::any::Any;

use super::{ExprHandle, Literal, StmtHandle};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructId {
  pub id: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
  Str,
  Num,
  Bool,
  Struct(StructId),
  NamedFunction(StmtHandle),
  AnonymusFunction(ExprHandle),
  //FunctionType(Vec<Type>)
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
