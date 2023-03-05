use super::{identifier::GlobalId, types::TypeId};

pub struct OverloadSet {
  functions: Vec<Vec<TypeId>>,
}

impl OverloadSet {
  pub fn add_overload(&mut self, fn_name_id: GlobalId, overload_typeid: TypeId) -> bool {
    let overloads = &mut self.functions[fn_name_id as usize];
    if overloads.contains(&overload_typeid) {
      false
    } else {
      overloads.push(overload_typeid);
      true
    }
  }
}
