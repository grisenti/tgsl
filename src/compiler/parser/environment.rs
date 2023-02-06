use super::*;
use std::collections::HashMap;

pub type Scope = HashMap<String, Identifier>;

pub struct Environment {
  global: Scope,
  scopes: Vec<Scope>,
  last_id: u32,
}

impl Environment {
  fn innermost(&mut self) -> &mut Scope {
    self.scopes.last_mut().unwrap_or(&mut self.global)
  }

  pub fn get_name_or_add(&mut self, name: &str) -> Identifier {
    self
      .scopes
      .iter()
      .rev()
      .find_map(|scope| scope.get(name))
      .or_else(|| self.global.get(name))
      .cloned()
      .unwrap_or_else(|| self.declare_name(name))
  }

  pub fn declare_name(&mut self, name: &str) -> Identifier {
    let id = Identifier(self.last_id);
    self.innermost().insert(name.to_string(), id);
    self.last_id += 1;
    id
  }

  pub fn pop(&mut self) {
    self.scopes.pop();
  }

  pub fn push(&mut self) {
    self.scopes.push(HashMap::new())
  }

  pub fn get_global(self) -> Scope {
    self.global
  }

  pub fn new() -> Self {
    Self {
      global: HashMap::new(),
      scopes: Vec::new(),
      last_id: 0,
    }
  }
}