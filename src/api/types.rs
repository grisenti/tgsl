use crate::compiler::types::Type as InternalType;

pub struct Type(pub(crate) InternalType);

impl PartialEq<InternalType> for Type {
  fn eq(&self, other: &InternalType) -> bool {
    self.0 == *other
  }
}

impl Type {
  pub fn unit() -> Self {
    Self(InternalType::Nothing)
  }

  pub fn str() -> Self {
    Self(InternalType::Str)
  }

  pub fn bool() -> Self {
    Self(InternalType::Bool)
  }

  pub fn num() -> Self {
    Self(InternalType::Num)
  }
}

pub trait ToType {
  fn to_type() -> Type;
}

impl ToType for () {
  fn to_type() -> Type {
    Type::unit()
  }
}

impl ToType for bool {
  fn to_type() -> Type {
    Type::bool()
  }
}

impl ToType for f64 {
  fn to_type() -> Type {
    Type::num()
  }
}

impl ToType for String {
  fn to_type() -> Type {
    Type::str()
  }
}

impl ToType for &mut String {
  fn to_type() -> Type {
    Type::str()
  }
}

impl ToType for &str {
  fn to_type() -> Type {
    Type::str()
  }
}
