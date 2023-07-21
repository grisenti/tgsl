use std::collections::HashMap;

use super::errors::{ge_err, parser_err, CompilerResult};
use super::identifier::{GlobalId, Identifier, ModuleId};
use super::lexer::SourceRange;
use super::parser::ParsedModule;
use super::types::TypeId;

type GlobalName = Vec<(GlobalId, ModuleId)>;

pub struct Module {
  pub public_names: HashMap<String, GlobalId>,
}

#[derive(Clone, Copy)]
pub struct GlobalTypes<'a> {
  types: &'a [TypeId],
  base_ids: &'a [u16],
}

impl<'a> GlobalTypes<'a> {
  pub fn get_type(&self, gid: GlobalId) -> TypeId {
    assert!(!gid.is_relative());

    let (id, module) = gid.split_absolute();
    let module_start = self.base_ids[module as usize];
    self.types[(module_start + id) as usize]
  }

  pub fn new(global_env: &'a GlobalEnv) -> Self {
    Self {
      base_ids: &global_env.module_base_identifier,
      types: &global_env.types,
    }
  }
}

pub struct GlobalEnv {
  types: Vec<TypeId>,
  module_names: HashMap<String, ModuleId>,
  modules: Vec<Module>,
  module_base_identifier: Vec<u16>,
}

impl GlobalEnv {
  pub fn get_module_id(&self, module_name: &str, name_sr: SourceRange) -> CompilerResult<ModuleId> {
    if let Some(module_id) = self.module_names.get(module_name) {
      Ok(*module_id)
    } else {
      Err(ge_err::not_a_loaded_module(name_sr, module_name))
    }
  }

  pub fn get_module(&self, id: ModuleId) -> &Module {
    &self.modules[id.0 as usize]
  }

  pub fn export_module(&mut self, parsed_module: ParsedModule) -> Option<ModuleId> {
    if let Some(module_name) = parsed_module.module_name {
      debug_assert!(!self.module_names.contains_key(&module_name));
      let module_id = self.modules.len() as u16;
      self.types.extend(parsed_module.module_global_types.iter());
      self.module_names.insert(module_name, ModuleId(module_id));
      let module_public_names = parsed_module
        .globals
        .into_iter()
        .filter(|(_, gid)| gid.is_public())
        .filter_map(|(name, gid)| {
          if gid.is_relative() {
            let relative_id = gid.get_relative();
            Some((name, GlobalId::absolute(module_id, relative_id)))
          } else {
            None
          }
        })
        .collect::<HashMap<_, _>>();
      let exported_names = module_public_names.len();
      //we always have at least one from initialization
      let last_base_identifier = self.module_base_identifier.last().unwrap();
      self
        .module_base_identifier
        .push(last_base_identifier + exported_names as u16);
      self.modules.push(Module {
        public_names: module_public_names,
      });
      Some(ModuleId(module_id))
    } else {
      None
    }
  }

  pub fn is_module_name_available(&self, name: &str) -> bool {
    !self.module_names.contains_key(name)
  }

  pub fn get_id(&self, module_id: ModuleId, name: &str) -> Option<GlobalId> {
    self.modules[module_id.0 as usize]
      .public_names
      .get(name)
      .copied()
  }

  pub fn new() -> Self {
    Self {
      types: Vec::new(),
      module_names: HashMap::new(),
      modules: Vec::new(),
      module_base_identifier: vec![0],
    }
  }
}

#[cfg(test)]
mod test {
  use super::{GlobalEnv, Module};

  impl GlobalEnv {
    pub fn add_fake_module(&mut self, module: Module) {}
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
