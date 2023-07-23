use crate::compiler::{
  identifier::{ExternId, GlobalId},
  CompiledModule,
};

pub struct AddressTable {
  module_variables_address_space: Vec<(u32, u32)>,
  module_extern_functions_address_space: Vec<(u32, u32)>,
}

impl AddressTable {
  pub fn resolve_variable(&self, global_id: GlobalId) -> u32 {
    if global_id.is_relative() {
      let last_end = self.module_variables_address_space.last().unwrap().1;
      last_end + global_id.get_relative() as u32
    } else {
      let (id, module) = global_id.split_absolute();
      self.module_variables_address_space[module as usize].0 + id as u32
    }
  }

  pub fn resolve_extern_function(&self, extern_id: ExternId) -> u32 {
    if extern_id.is_relative() {
      let last_end = self.module_extern_functions_address_space.last().unwrap().1;
      last_end + extern_id.get_relative() as u32
    } else {
      let (id, module) = extern_id.split_absolute();
      self.module_extern_functions_address_space[module as usize].0 + id as u32
    }
  }

  pub fn update_table(&mut self, compiled_module: &CompiledModule) {
    let last_end = self
      .module_variables_address_space
      .last()
      .map(|(_, end)| end)
      .copied()
      .unwrap_or(0);
    self
      .module_variables_address_space
      .push((last_end, last_end + compiled_module.globals_count as u32));

    let last_end = self
      .module_extern_functions_address_space
      .last()
      .map(|(_, end)| end)
      .copied()
      .unwrap_or(0);
    self.module_extern_functions_address_space.push((
      last_end,
      last_end + compiled_module.extern_functions.len() as u32,
    ));
  }

  pub fn new() -> Self {
    Self {
      module_variables_address_space: Vec::new(),
      module_extern_functions_address_space: Vec::new(),
    }
  }
}
