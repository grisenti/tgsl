use std::collections::{hash_map::Entry, HashMap};

use crate::{
  ast::Identifier,
  errors::{SourceError, SourceErrorType},
  lexer::SourceInfo,
};

use super::ExprValue;

#[derive(Default)]
pub struct Environment {
  memory: HashMap<u32, ExprValue>,
}

impl Environment {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn get_or_err(&self, id: Identifier, info: SourceInfo) -> Result<ExprValue, SourceError> {
    self.memory.get(&id.0).cloned().ok_or_else(|| {
      SourceError::from_token_info(
        &info,
        "identifier was not declared before".to_string(),
        SourceErrorType::Runtime,
      )
    })
  }

  pub fn update_value_or_err(
    &mut self,
    id: Identifier,
    id_info: SourceInfo,
    value: ExprValue,
  ) -> Result<ExprValue, SourceError> {
    if let Entry::Occupied(mut e) = self.memory.entry(id.0) {
      *e.get_mut() = value.clone();
      Ok(value)
    } else {
      Err(SourceError::from_token_info(
        &id_info,
        "identifier was not declared".to_string(),
        SourceErrorType::Runtime,
      ))
    }
  }

  pub fn set(&mut self, id: Identifier, value: ExprValue) {
    self.memory.insert(id.0, value);
  }
}
