use super::*;
use std::collections::HashMap;

pub type Scope = HashMap<String, Identifier>;
type StructMap = HashMap<String, StructId>;

pub struct Environment {
  global: Scope,
  scopes: Vec<Scope>,
  last_id: u32,
  last_user_type_id: u32,
  structs: StructMap,
}

impl Environment {
  fn declare_global_name(&mut self, name: &str) -> Identifier {
    let id = Identifier(self.last_id);
    self.global.insert(name.to_string(), id);
    self.last_id += 1;
    id
  }

  fn declare_user_type(&mut self, name: &str) -> StructId {
    let id = StructId {
      id: self.last_user_type_id,
    };
    self.structs.insert(name.to_string(), id);
    self.last_user_type_id += 1;
    id
  }

  pub fn get_name_or_add_global(&mut self, name: &str) -> Identifier {
    self
      .scopes
      .iter()
      .rev()
      .find_map(|scope| scope.get(name))
      .or_else(|| self.global.get(name))
      .cloned()
      .unwrap_or_else(|| self.declare_global_name(name))
  }

  pub fn declare_name_or_err(
    &mut self,
    name: &str,
    name_src_info: SourceInfo,
  ) -> Result<Identifier, SourceError> {
    let innermost = self.scopes.last_mut().unwrap_or(&mut self.global);
    if innermost.contains_key(name) {
      Err(error_from_source_info(
        &name_src_info,
        format!("cannot redeclare name '{name}' in the same scope"),
      ))
    } else {
      let id = Identifier(self.last_id);
      innermost.insert(name.to_string(), id);
      self.last_id += 1;
      Ok(id)
    }
  }

  pub fn get_struct_id_or_add(&mut self, name: &str) -> StructId {
    self
      .structs
      .get(name)
      .cloned()
      .unwrap_or_else(|| self.declare_user_type(name))
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
      global: Scope::new(),
      scopes: Vec::new(),
      structs: StructMap::new(),
      last_id: 0,
      last_user_type_id: 0,
    }
  }
}
