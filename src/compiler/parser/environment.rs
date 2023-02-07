use super::*;
use std::collections::HashMap;

pub type Scope = HashMap<String, Identifier>;
type TypeMap = HashMap<String, Type>;

pub struct Environment {
  global: Scope,
  scopes: Vec<Scope>,
  last_id: u32,
  last_user_type_id: u32,
  user_types: TypeMap,
}

impl Environment {
  fn declare_name(&mut self, name: &str) -> Identifier {
    let id = Identifier(self.last_id);
    self.global.insert(name.to_string(), id);
    self.last_id += 1;
    id
  }

  fn declare_user_type(&mut self, name: &str) -> Type {
    let id = Type::User(UserTypeId {
      id: self.last_user_type_id,
    });
    self.user_types.insert(name.to_string(), id.clone());
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
      .unwrap_or_else(|| self.declare_name(name))
  }

  pub fn get_type_or_add(&mut self, name: &str) -> Type {
    self
      .user_types
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
      user_types: TypeMap::from([
        ("str".to_string(), Type::Str),
        ("num".to_string(), Type::Num),
        ("bool".to_string(), Type::Bool),
      ]),
      last_id: 0,
      last_user_type_id: 0,
    }
  }
}
