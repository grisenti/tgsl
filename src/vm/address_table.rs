use crate::compiler::identifier::{ExternId, FunctionId, GlobalVarId};

#[derive(Default)]
pub struct AddressTable {
  global_variables: u32,
  extern_functions: u32,
  global_functions: u32,
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

  pub fn resolve_global_function(&self, function_id: FunctionId) -> u32 {
    if function_id.is_relative() {
      self.global_functions + function_id.get_id()
    } else {
      function_id.get_id()
    }
  }

  pub fn update_table(
    &mut self,
    module_globals_count: u32,
    extern_functions_count: u32,
    module_global_functions: u32,
  ) {
    self.global_variables += module_globals_count;
    self.extern_functions += extern_functions_count;
    self.global_functions += module_global_functions;
  }

  pub fn new() -> Self {
    Default::default()
  }
}
