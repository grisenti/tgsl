use crate::compiler::{
  identifier::{ExternId, GlobalVarId},
  CompiledModule,
};

#[derive(Default)]
pub struct AddressTable {
  global_variables: u32,
  extern_functions: u32,
}

impl AddressTable {
  pub fn resolve_variable(&self, global_id: GlobalVarId) -> u32 {
    if global_id.is_relative() {
      global_id.get_id() + self.global_variables
    } else {
      global_id.get_id()
    }
  }

  pub fn resolve_extern_function(&self, extern_id: ExternId) -> u32 {
    if extern_id.is_relative() {
      self.extern_functions + extern_id.get_id()
    } else {
      extern_id.get_id()
    }
  }

  pub fn update_table(&mut self, module_globals_count: u32, extern_functions_count: u32) {
    self.global_variables += module_globals_count;
    self.extern_functions += extern_functions_count;
  }

  pub fn new() -> Self {
    Default::default()
  }
}
