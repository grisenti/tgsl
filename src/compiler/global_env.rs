use std::collections::HashMap;

use crate::compiler::functions::LinkedFunctions;
use crate::compiler::semantics::ModuleExports;
use crate::compiler::structs::ExportedGlobalStructs;
use crate::compiler::variables::LinkedGlobalVariables;

use super::types::Type;

pub struct Module {
  pub global_variables: LinkedGlobalVariables,
  pub structs: ExportedGlobalStructs,
  pub functions: LinkedFunctions,
}

#[derive(Default)]
pub struct GlobalEnv {
  variable_types: Vec<Type>,
  modules: HashMap<String, Module>,
  global_variables_count: u32,
  last_extern_function_address: u32,
  last_native_function_address: u32,
}

impl GlobalEnv {
  pub fn get_module(&self, module_name: &str) -> Option<&Module> {
    self.modules.get(module_name)
  }

  pub fn export_module(&mut self, module_exports: ModuleExports) {
    debug_assert!(
      !self.modules.contains_key(&module_exports.module_name),
      "this error should have been detected earlier by calling `is_module_name_available`"
    );

    let global_variables = module_exports.global_variables.count();
    let native_functions = module_exports.functions.native_count;
    let extern_functions = module_exports.functions.extern_count;

    let linked_functions = module_exports.functions.link(
      self.last_native_function_address,
      self.last_extern_function_address,
    );
    let linked_global_variables = module_exports
      .global_variables
      .link(self.global_variables_count);

    self.global_variables_count += global_variables;
    self.last_extern_function_address += extern_functions;
    self.last_native_function_address += native_functions;
    self.modules.insert(
      module_exports.module_name,
      Module {
        global_variables: linked_global_variables,
        structs: module_exports.structs,
        functions: linked_functions,
      },
    );
  }

  pub fn is_module_name_available(&self, name: &str) -> bool {
    !self.modules.contains_key(name)
  }

  pub fn new() -> Self {
    Default::default()
  }
}

#[cfg(test)]
pub mod test {}
