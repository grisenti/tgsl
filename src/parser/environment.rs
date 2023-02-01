use super::*;
use std::{collections::HashMap, ops::Not};

pub struct Environment {
  scopes: Vec<HashMap<String, Identifier>>,
  last_id: u32,
}

impl Environment {
  fn innermost(&mut self) -> &mut HashMap<String, Identifier> {
    // we are always guaranteed the global scope so unwrapping is safe
    self.scopes.last_mut().unwrap()
  }

  pub fn get_name_or_add(&mut self, name: &str) -> Identifier {
    self
      .scopes
      .iter()
      .rev()
      .find_map(|scope| scope.get(name))
      .cloned()
      .unwrap_or_else(|| self.declare_name(name))
  }

  pub fn declare_name(&mut self, name: &str) -> Identifier {
    let id = self.last_id;
    self.innermost().insert(name.to_string(), Identifier(id));
    self.last_id += 1;
    Identifier(id)
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
      last_id: 0,
    }
  }
}
