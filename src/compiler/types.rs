use std::fmt::Display;

use super::{global_env::GlobalEnv, identifier::StructId};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
  Any,
  Str,
  Num,
  Bool,
  Struct(StructId),
  Function(Vec<Type>),
  Unknown,
  Nothing,
  Error,
}

impl Type {
  pub fn print_pretty(&self) -> String {
    match self {
      Type::Function(types) => {
        let arguments = types
          .iter()
          .take(types.len() - 1)
          .map(|val| val.print_pretty())
          .collect::<Vec<_>>()
          .join(" ,");
        let return_type = types
          .last()
          .expect("function does not have a return type")
          .print_pretty();
        format!("fn ({}) -> {}", arguments, return_type)
      }
      other => format!("{:?}", self),
    }
  }
}
