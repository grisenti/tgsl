use std::{
  cell::RefCell,
  collections::{hash_map::Entry, HashMap},
  rc::Rc,
};

use crate::compiler::{ast::Identifier, lexer::SourceInfo};
use crate::errors::SourceError;

use super::{ExprValue, Interpreter};

pub type EnvRef = Rc<RefCell<Environment>>;

pub struct Environment {
  parent: Option<EnvRef>,
  memory: HashMap<u32, ExprValue>,
}

impl Environment {
  pub fn global() -> EnvRef {
    Rc::new(RefCell::new(Self {
      parent: None,
      memory: HashMap::new(),
    }))
  }
  pub fn inner_scope(parent: EnvRef) -> EnvRef {
    Rc::new(RefCell::new(Self {
      parent: Some(parent),
      memory: HashMap::new(),
    }))
  }

  pub fn get_or_err(&self, id: Identifier, info: SourceInfo) -> Result<ExprValue, SourceError> {
    if let Some(val) = self.memory.get(&id.0) {
      Ok(val.clone())
    } else if let Some(parent) = &self.parent {
      parent.borrow().get_or_err(id, info)
    } else {
      Err(Interpreter::runtime_error(
        &info,
        "identifier was not declared before".to_string(),
      ))
    }
  }

  pub fn get(&self, id: Identifier) -> Option<ExprValue> {
    self.memory.get(&id.0).cloned()
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
    } else if let Some(parent) = &self.parent {
      parent.borrow_mut().update_value_or_err(id, id_info, value)
    } else {
      Err(Interpreter::runtime_error(
        &id_info,
        "identifier was not declared".to_string(),
      ))
    }
  }

  pub fn set(&mut self, id: Identifier, value: ExprValue) {
    self.memory.insert(id.0, value);
  }

  pub fn parent(&self) -> EnvRef {
    self.parent.as_ref().unwrap().clone()
  }
}
