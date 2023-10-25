pub use crate::compiler::types::Type;

pub trait ToType {
  fn to_type() -> Type;
}

impl ToType for () {
  fn to_type() -> Type {
    Type::Nothing
  }
}

impl ToType for bool {
  fn to_type() -> Type {
    Type::Bool
  }
}

impl ToType for f64 {
  fn to_type() -> Type {
    Type::Num
  }
}

impl ToType for String {
  fn to_type() -> Type {
    Type::Str
  }
}

impl ToType for &mut String {
  fn to_type() -> Type {
    Type::Str
  }
}

impl ToType for &str {
  fn to_type() -> Type {
    Type::Str
  }
}
