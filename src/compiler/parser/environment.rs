use super::*;
use std::collections::HashMap;

pub type Scope = HashMap<String, Identifier>;

pub struct Environment {
  global: Scope,
  scopes: Vec<Scope>,
  types: Vec<Type>,
  last_id: u32,
}

pub struct FinalizedEnvironment {
  pub global_scope: Scope,
  pub type_map: Vec<Type>,
}

impl Environment {
  fn declare_global_name(&mut self, name: &str) -> Identifier {
    let id = Identifier(self.last_id);
    self.global.insert(name.to_string(), id);
    self.last_id += 1;
    id
  }

  pub fn set_type(&mut self, id: Identifier, new_type: Type) {
    let index = id.0 as usize;
    if self.types.len() <= index {
      self.types.resize(index + 1, Type::Unknown);
    }
    self.types[index] = new_type;
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

  pub fn declare_anonymous_closure(&mut self) -> Identifier {
    self.declare_global_name(&format!("{{{}}}", self.last_id))
  }

  pub fn pop(&mut self) {
    self.scopes.pop();
  }

  pub fn push(&mut self) {
    self.scopes.push(HashMap::new())
  }

  pub fn new() -> Self {
    Self {
      global: Scope::new(),
      scopes: Vec::new(),
      types: Vec::new(),
      last_id: 0,
    }
  }

  pub fn finalize(mut self) -> FinalizedEnvironment {
    self.types.resize(self.last_id as usize + 1, Type::Unknown);
    FinalizedEnvironment {
      global_scope: self.global,
      type_map: self.types,
    }
  }
}
