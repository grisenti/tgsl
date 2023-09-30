use core::panic;
use std::collections::HashMap;

use crate::compiler::overload_set::OverloadSet;
use crate::compiler::semantics::ModuleExports;
use crate::compiler::structs::ExportedGlobalStructs;

use super::identifier::{ExternId, GlobalIdentifier, GlobalVarId, ModuleId};
use super::types::Type;

pub struct Module {
  pub global_names: HashMap<String, GlobalIdentifier>,
  pub overloads: Vec<OverloadSet>,
  pub structs: ExportedGlobalStructs,
}

#[derive(Default)]
pub struct GlobalEnv {
  variable_types: Vec<Type>,
  extern_functions_types: Vec<Type>,
  module_names: HashMap<String, ModuleId>,
  modules: Vec<Module>,
  global_variables_count: u32,
  extern_functions_count: u32,
  exported_functions: u32,
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

  pub fn get_extern_function_type(&self, id: ExternId) -> &Type {
    assert!(!id.is_relative());
    &self.extern_functions_types[id.get_id() as usize]
  }

  pub fn export_module(&mut self, module_exports: ModuleExports) -> Option<ModuleId> {
    let module_name = module_exports.module_name?;

    debug_assert!(
      !self.module_names.contains_key(&module_name),
      "this error should have been detected earlier by calling `is_module_name_available`"
    );

    let module_id = self.modules.len() as u16;
    self.module_names.insert(module_name, ModuleId(module_id));

    self
      .variable_types
      .extend(module_exports.global_variables_types.into_iter());
    self
      .extern_functions_types
      .extend(module_exports.extern_function_types.into_iter());

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
        GlobalIdentifier::ExternFunction(ext_id) => {
          if ext_id.is_relative() && ext_id.is_public() {
            let absolute_id = ExternId::absolute(ext_id.get_id() + self.extern_functions_count);
            module_names.insert(name, GlobalIdentifier::ExternFunction(absolute_id));
            exported_extern_functions += 1;
          }
        }
        GlobalIdentifier::OverloadId(overload_id) => {
          module_names.insert(name, GlobalIdentifier::OverloadId(overload_id));
        }
        _ => panic!(),
      }
    }
    let mut exported_functions = 0;
    let overloads = module_exports
      .overloads
      .into_iter()
      .map(|overload_set| {
        let (set, count) = overload_set.export_set(self.exported_functions);
        exported_functions += count;
        set
      })
      .collect();

    self.global_variables_count += exported_variables;
    self.extern_functions_count += exported_extern_functions;
    self.exported_functions += exported_functions as u32;
    self.modules.push(Module {
      global_names: module_names,
      overloads,
      structs: module_exports.structs,
    });
    Some(ModuleId(module_id))
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
