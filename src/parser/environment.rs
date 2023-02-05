use super::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct NameInfo {
  id: Identifier,
  is_const: bool,
}

pub struct Environment {
  scopes: Vec<HashMap<String, NameInfo>>,
  last_id: u32,
}

impl Environment {
  fn innermost(&mut self) -> &mut HashMap<String, NameInfo> {
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
      .map(|name_info| name_info.id)
      .unwrap_or_else(|| self.declare_name(name, false))
  }

  pub fn declare_name(&mut self, name: &str, is_const: bool) -> Identifier {
    let id = Identifier(self.last_id);
    self
      .innermost()
      .insert(name.to_string(), NameInfo { id, is_const });
    self.last_id += 1;
    id
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
