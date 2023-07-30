use super::{
  ast::Literal,
  identifier::{Identifier, StructId, VariableIdentifier},
};

pub mod type_map;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
  Any,
  Str,
  Num,
  Bool,
  Struct(StructId),
  Function {
    parameters: Vec<TypeId>,
    ret: TypeId,
  },
  Unknown,
  Nothing,
  Error,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub u32);

impl TypeId {
  pub const ANY: TypeId = TypeId(0);
  pub const STR: TypeId = TypeId(1);
  pub const NUM: TypeId = TypeId(2);
  pub const BOOL: TypeId = TypeId(3);
  pub const UNKNOWN: TypeId = TypeId(4);
  pub const NOTHING: TypeId = TypeId(5);
  pub const ERROR: TypeId = TypeId(6);

  pub fn from_literal(lit: Literal) -> Self {
    match lit {
      Literal::String(_) => TypeId::STR,
      Literal::Number(_) => TypeId::NUM,
      Literal::True | Literal::False => TypeId::BOOL,
      Literal::Null => TypeId::UNKNOWN,
    }
  }
}

pub const DEFAULT_TYPEIDS: [(Type, TypeId); 7] = [
  (Type::Any, TypeId::ANY),
  (Type::Str, TypeId::STR),
  (Type::Num, TypeId::NUM),
  (Type::Bool, TypeId::BOOL),
  (Type::Unknown, TypeId::UNKNOWN),
  (Type::Nothing, TypeId::NOTHING),
  (Type::Error, TypeId::ERROR),
];
