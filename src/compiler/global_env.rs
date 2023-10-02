use core::panic;
use std::collections::HashMap;

use crate::compiler::functions::LinkedFunctions;
use crate::compiler::semantics::ModuleExports;
use crate::compiler::structs::ExportedGlobalStructs;

use super::identifier::{GlobalIdentifier, GlobalVarId, ModuleId};
use super::types::Type;

pub struct Module {
  pub global_names: HashMap<String, GlobalIdentifier>,
  pub structs: ExportedGlobalStructs,
  pub functions: LinkedFunctions,
}

#[derive(Default)]
pub struct GlobalEnv {
  variable_types: Vec<Type>,
  module_names: HashMap<String, ModuleId>,
  modules: Vec<Module>,
  global_variables_count: u32,
  last_extern_function_address: u32,
  last_native_function_address: u32,
}

impl GlobalEnv {
  pub fn get_module(&self, module_name: &str) -> Option<&Module> {
    let id = self.module_names.get(module_name)?;
    Some(&self.modules[id.0 as usize])
  }

  pub fn get_variable_type(&self, id: GlobalVarId) -> &Type {
    assert!(!id.is_relative());
    &self.variable_types[id.get_id() as usize]
  }

  pub fn export_module(&mut self, module_exports: ModuleExports) {
    debug_assert!(
      !self.module_names.contains_key(&module_exports.module_name),
      "this error should have been detected earlier by calling `is_module_name_available`"
    );

    let module_id = self.modules.len() as u16;
    self
      .module_names
      .insert(module_exports.module_name, ModuleId(module_id));

    self
      .variable_types
      .extend(module_exports.global_variables_types.into_iter());

    let mut module_names = HashMap::new();
    let mut exported_variables = 0;
    let mut exported_extern_functions = 0;
    for (name, id) in module_exports.global_names {
      match id {
        GlobalIdentifier::Variable(var_id) => {
          if var_id.is_relative() && var_id.is_public() {
            let absolute_id = GlobalVarId::absolute(var_id.get_id() + self.global_variables_count);
            module_names.insert(name, GlobalIdentifier::Variable(absolute_id));
            exported_variables += 1;
          }
        }
        _ => panic!(),
      }
    }
    let native_functions = module_exports.functions.native_count;
    let extern_functions = module_exports.functions.extern_count;

    let linked_functions = module_exports.functions.link(
      self.last_native_function_address,
      self.last_extern_function_address,
    );

    self.global_variables_count += exported_variables;
    self.last_extern_function_address += extern_functions;
    self.last_native_function_address += native_functions;
    self.modules.push(Module {
      global_names: module_names,
      structs: module_exports.structs,
      functions: linked_functions,
    });
  }

  pub fn is_module_name_available(&self, name: &str) -> bool {
    !self.module_names.contains_key(name)
  }

  pub fn new() -> Self {
    Default::default()
  }
}

#[cfg(test)]
pub mod test {}
