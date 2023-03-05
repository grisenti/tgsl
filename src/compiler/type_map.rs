use std::collections::{hash_map::Entry, HashMap};

use crate::compiler::{
  identifier::Identifier,
  types::{Type, TypeId, DEFAULT_TYPEIDS},
};

pub struct TypeMap {
  last_type_id: u32,
  map: HashMap<Type, TypeId>,
}

impl TypeMap {
  pub fn get_or_add(&mut self, type_: Type) -> TypeId {
    match self.map.entry(type_) {
      Entry::Occupied(e) => *e.get(),
      Entry::Vacant(e) => {
        let id = TypeId(self.last_type_id);
        e.insert(id);
        self.last_type_id += 1;
        id
      }
    }
  }

  pub fn add_struct_type(
    &mut self,
    struct_id: Identifier,
    member_types: Vec<TypeId>,
  ) -> (TypeId, TypeId) {
    let struct_type_id = self.get_or_add(Type::Struct(struct_id));
    let constructor_type_id = self.get_or_add(Type::Function {
      parameters: member_types,
      ret: struct_type_id,
    });
    (struct_type_id, constructor_type_id)
  }

  pub fn new() -> Self {
    Self {
      last_type_id: DEFAULT_TYPEIDS.len() as u32,
      map: HashMap::from(DEFAULT_TYPEIDS),
    }
  }

  pub fn reverse_map(self) -> ReverseTypeMap {
    let mut res = vec![Type::Nothing; self.map.len()];
    for (t, id) in self.map {
      res[id.0 as usize] = t;
    }
    ReverseTypeMap(res)
  }
}

pub struct ReverseTypeMap(Vec<Type>);

impl ReverseTypeMap {
  pub fn get_type(&self, id: TypeId) -> &Type {
    &self.0[id.0 as usize]
  }

  pub fn type_to_string(&self, id: TypeId) -> String {
    match self.get_type(id) {
      Type::Function { parameters, ret } => {
        let parameters = parameters
          .iter()
          .map(|id| self.type_to_string(*id))
          .collect::<Vec<String>>()
          .join(",");
        format!("fn ({parameters}) -> {}", self.type_to_string(*ret))
      }
      other => format!("{other:?}"),
    }
  }
}
