use std::collections::{hash_map::Entry, HashMap};

use crate::{
  compiler::{
    ast::{Identifier, Type},
    error_from_source_info,
  },
  errors::{SourceError, SourceInfo},
};

type TypeMap = HashMap<Identifier, Type>;
type FunctionMap = HashMap<String, Type>;

#[derive(Default)]
pub struct TypeChecker {
  type_map: TypeMap,
}

impl TypeChecker {
  pub fn new() -> Self {
    Default::default()
  }

  pub fn set_type_or_err(
    &mut self,
    id: Identifier,
    typ: Type,
    name_info: SourceInfo,
  ) -> Result<(), SourceError> {
    match self.type_map.entry(id) {
      Entry::Occupied(mut e) => {
        if *e.get() != Type::Any {
          Err(error_from_source_info(&name_info, "type error".to_string()))
        } else {
          e.insert(typ);
          Ok(())
        }
      }
      Entry::Vacant(v) => {
        v.insert(typ);
        Ok(())
      }
    }
  }

  pub fn get_type(&self, id: Identifier) -> Type {
    self.type_map.get(&id).unwrap().clone()
  }

  pub fn add_function(&mut self, id: Identifier, types: &[Type]) {}
}
