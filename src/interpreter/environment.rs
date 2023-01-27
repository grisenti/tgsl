use super::*;
use std::{collections::HashMap, ops::Not};

pub struct Environment<'src> {
  scopes: Vec<HashMap<&'src str, ExprValue>>,
}

impl<'src> Environment<'src> {
  fn innermost(&mut self) -> &mut HashMap<&'src str, ExprValue> {
    // we are always guaranteed the global scope so unwrapping is safe
    self.scopes.last_mut().unwrap()
  }

  pub fn declare_source_identifier(
    &mut self,
    id: &'src str,
    id_info: &TokenInfo,
    value: ExprValue,
  ) -> Result<(), SourceError> {
    self
      .innermost()
      .contains_key(id)
      .not()
      .then(|| {
        self.innermost().insert(id, value);
      })
      .ok_or_else(|| {
        SourceError::from_token_info(
          id_info,
          format!("identifier '{}' already declared in the current scope", id),
          SourceErrorType::Runtime,
        )
      })
  }

  pub fn declare_native_identifier(&mut self, id: &'src str, value: ExprValue) {
    self.scopes.first_mut().unwrap().insert(id, value);
  }

  pub fn get_id_value(&self, id: &str, id_info: &TokenInfo) -> ExprResult {
    self
      .scopes
      .iter()
      .rev()
      .find_map(|scope| scope.get(id))
      .cloned()
      .ok_or_else(|| {
        SourceError::from_token_info(
          &id_info,
          format!("unknown identifier {}", id),
          SourceErrorType::Runtime,
        )
      })
  }

  pub fn assign(&mut self, name: &str, info: &TokenInfo<'src>, value: ExprValue) -> ExprResult {
    self
      .scopes
      .iter_mut()
      .rev()
      .find_map(|scope| scope.get_mut(name))
      .map(|var_value| {
        *var_value = value.clone();
        value
      })
      .ok_or_else(|| {
        SourceError::from_token_info(
          info,
          format!("variable {} was not declared before", name),
          SourceErrorType::Runtime,
        )
      })
  }

  pub fn pop(&mut self) {
    self.scopes.pop();
  }

  pub fn push(&mut self) {
    self.scopes.push(HashMap::new())
  }

  pub fn global() -> Self {
    Self {
      scopes: vec![HashMap::new()],
    }
  }
}
