#[derive(Debug, Clone, Copy)]
pub struct UserTypeId {
  pub id: u32,
}

#[derive(Debug, Clone, Copy)]
pub enum Type {
  Str,
  Num,
  Bool,
  User(UserTypeId),

  Any,
  Undefined,
}

impl Type {
  pub fn from_name(name: &str) -> Option<Self> {
    match name {
      "str" => Some(Type::Str),
      "num" => Some(Type::Num),
      "bool" => Some(Type::Bool),
      _ => None,
    }
  }
}
