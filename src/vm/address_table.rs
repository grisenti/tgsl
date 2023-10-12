#[derive(Default)]
pub struct AddressTable {
  global_variables: u32,
  foreign_functions: u32,
  global_functions: u32,
}

impl AddressTable {
  pub fn resolve_variable(&self, global_id: u32) -> u32 {
    global_id + self.global_variables
  }

  pub fn resolve_foreign_function(&self, foreign_id: u32) -> u32 {
    self.foreign_functions + foreign_id
  }

  pub fn resolve_global_function(&self, function_id: u32) -> u32 {
    self.global_functions + function_id
  }

  pub fn update_table(
    &mut self,
    module_globals_count: u32,
    foreign_functions_count: u32,
    module_global_functions: u32,
  ) {
    self.global_variables += module_globals_count;
    self.foreign_functions += foreign_functions_count;
    self.global_functions += module_global_functions;
  }

  pub fn new() -> Self {
    Default::default()
  }
}
