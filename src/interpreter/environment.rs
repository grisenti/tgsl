use super::*;
use std::collections::HashMap;

pub struct Environment<'src> {
  identifiers: HashMap<&'src str, ExprValue>,
  enclosing: Option<Box<Environment<'src>>>,
}

impl<'src> Environment<'src> {
  pub fn declare_identifier(
    &mut self,
    id: &'src str,
    id_info: TokenInfo,
    value: ExprValue,
  ) -> Result<(), SourceError> {
    if !self.identifiers.contains_key(id) {
      self.identifiers.insert(id, value);
      Ok(())
    } else {
      Err(SourceError::from_token_info(
        id_info,
        format!("identifier {} already declared", id),
        SourceErrorType::Runtime,
      ))
    }
  }

  pub fn get_id_value(&self, id: &str, id_info: TokenInfo) -> ExprResult {
    if let Some(value) = self.identifiers.get(id) {
      Ok(value.clone())
    } else {
      self
        .enclosing
        .as_ref()
        .map(|parent| parent.get_id_value(id, id_info))
        .unwrap_or(Err(SourceError::from_token_info(
          id_info,
          format!("unknown identifier {}", id),
          SourceErrorType::Runtime,
        )))
    }
  }

  pub fn assign(&mut self, name: &str, info: TokenInfo<'src>, value: ExprValue) -> ExprResult {
    if let Some(name_value) = self.identifiers.get_mut(name) {
      *name_value = value.clone();
      Ok(value)
    } else {
      Err(SourceError::from_token_info(
        info,
        format!("variable {} was not declared before", name),
        SourceErrorType::Runtime,
      ))
    }
  }

  pub fn global() -> Self {
    Self {
      identifiers: HashMap::new(),
      enclosing: None,
    }
  }

  pub fn sub(parent: Environment<'src>) -> Self {
    Self {
      identifiers: HashMap::new(),
      enclosing: Some(Box::new(parent)),
    }
  }

  pub fn pop(self) -> Self {
    *self.enclosing.unwrap()
  }
}
