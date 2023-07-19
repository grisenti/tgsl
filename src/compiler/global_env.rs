use std::collections::hash_map::Entry;
use std::collections::HashMap;

use super::errors::{ge_err, parser_err, CompilerResult};
use super::identifier::{GlobalId, Identifier, ModuleId};
use super::lexer::SourceRange;
use super::types::TypeId;

type GlobalName = Vec<(GlobalId, ModuleId)>;

pub struct Module {
  pub public_names: HashMap<String, GlobalId>,
}

#[derive(Default)]
pub struct GlobalEnv {
  names: HashMap<String, GlobalName>,
  pub types: Vec<TypeId>,
  module_names: HashMap<String, ModuleId>,
  modules: Vec<Module>,
  last_global_id: GlobalId,
  last_module_id: u16,
}

impl GlobalEnv {
  pub fn new_global_id(&mut self) -> GlobalId {
    let id = self.last_global_id;
    self.last_global_id += 1;
    self.types.push(TypeId::UNKNOWN);
    id
  }

  pub fn get_module_id(
    &mut self,
    module_name: &str,
    name_sr: SourceRange,
  ) -> CompilerResult<ModuleId> {
    if let Some(module_id) = self.module_names.get(module_name) {
      Ok(*module_id)
    } else {
      Err(ge_err::not_a_loaded_module(name_sr, module_name))
    }
  }

  pub fn get_module(&self, id: ModuleId) -> &Module {
    &self.modules[id.0 as usize]
  }

  pub fn export_module(&mut self, module_id: ModuleId, module: Module) {
    assert_eq!(
      module_id.0 as usize,
      self.modules.len(),
      "don't need this case for now"
    );

    self.modules.push(module);
  }

  pub fn new_module(
    &mut self,
    module_name: &str,
    name_sr: SourceRange,
  ) -> CompilerResult<ModuleId> {
    match self.module_names.entry(module_name.to_string()) {
      Entry::Vacant(e) => {
        let id = ModuleId(self.last_module_id);
        self.last_module_id += 1;
        e.insert(id);
        Ok(id)
      }
      Entry::Occupied(_) => Err(ge_err::trying_to_redeclare_a_module(name_sr, module_name)),
    }
  }

  pub fn get_id(&self, module_id: ModuleId, name: &str) -> Option<GlobalId> {
    self.modules[module_id.0 as usize]
      .public_names
      .get(name)
      .copied()
  }

  pub fn new() -> Self {
    Default::default()
  }
}

#[cfg(never)]
mod test {
  use crate::errors::SourceInfo;

  use super::GlobalEnv;

  #[test]
  fn no_declaration_for_name() {
    let mut env = GlobalEnv::new();
    env.get_or_add(&[], "test", Sou).expect("error adding name");
    env
      .finalize_current_module()
      .expect_err("absence of declaration not detected");
  }

  #[test]
  fn no_declaration_for_name() {
    let mut env = GlobalEnv::new();
    env
      .get_or_add(&[], "test", FAKE_SOURCE_INFO)
      .expect("error adding name");
    env
      .finalize_current_module()
      .expect_err("absence of declaration not detected");
  }
}
