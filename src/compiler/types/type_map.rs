use std::collections::{hash_map::Entry, HashMap};

use crate::compiler::{
  identifier::{Identifier, StructId, VariableIdentifier},
  types::{Type, TypeId, DEFAULT_TYPEIDS},
};

#[derive(Clone)]
pub struct TypeMap {
  last_type_id: u32,
  from_typeid: Vec<Type>,
  from_type: HashMap<Type, TypeId>,
}

impl TypeMap {
  pub fn get_or_add(&mut self, type_: Type) -> TypeId {
    match self.from_type.entry(type_.clone()) {
      Entry::Occupied(e) => *e.get(),
      Entry::Vacant(e) => {
        let id = TypeId(self.last_type_id);
        e.insert(id);
        self.last_type_id += 1;
        self.from_typeid.push(type_);
        id
      }
    }
  }

  pub fn add_struct_type(
    &mut self,
    struct_id: StructId,
    member_types: Vec<TypeId>,
  ) -> (TypeId, TypeId) {
    let struct_type_id = self.get_or_add(Type::Struct(struct_id));
    let constructor_type_id = self.get_or_add(Type::Function {
      parameters: member_types,
      ret: struct_type_id,
    });
    (struct_type_id, constructor_type_id)
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

  pub fn get_type(&self, id: TypeId) -> &Type {
    &self.from_typeid[id.0 as usize]
  }

  pub fn new() -> Self {
    Self {
      last_type_id: DEFAULT_TYPEIDS.len() as u32,
      from_type: HashMap::from(DEFAULT_TYPEIDS),
      from_typeid: DEFAULT_TYPEIDS.iter().map(|(t, _)| t.clone()).collect(),
    }
  }
}
