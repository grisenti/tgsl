use std::collections::HashMap;
use std::rc::Rc;

use crate::compiler::semantics::ModuleExports;

use super::types::Type;

pub struct Module {
  pub name: Rc<str>,
  pub index: ModuleIndex,
  pub exports: ModuleExports,
}

pub type ModuleIndex = u32;

#[derive(Default)]
pub struct GlobalEnv {
  variable_types: Vec<Type>,
  modules: Vec<Module>,
  module_indices: HashMap<Rc<str>, ModuleIndex>,
}

impl GlobalEnv {
  pub fn get_module(&self, module_name: &str) -> Option<ModuleIndex> {
    self.module_indices.get(module_name).copied()
  }

  pub fn is_module_name_available(&self, name: &str) -> bool {
    !self.module_indices.contains_key(name)
  }

  pub fn add_module(&mut self, module_exports: ModuleExports) -> ModuleIndex {
    debug_assert!(
      !self.is_module_name_available(&module_exports.module_name),
      "this error should have been detected earlier"
    );
    let index = self.modules.len() as ModuleIndex;
    self
      .module_indices
      .insert(module_exports.module_name.clone(), index);
    self.modules.push(Module {
      name: module_exports.module_name,
      index,
      global_variables: module_exports.global_variables.link(index),
      structs: module_exports.structs,
      functions: module_exports.functions.link(index),
    });
    index
  }

  pub fn new() -> Self {
    Default::default()
  }
}

#[cfg(test)]
pub mod test {}
