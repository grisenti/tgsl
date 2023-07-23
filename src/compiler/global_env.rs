use std::collections::HashMap;

use crate::compiler::identifier::VariableIdentifier;

use super::errors::{ge_err, CompilerResult};
use super::identifier::{ExternId, GlobalId, Identifier, ModuleId};
use super::lexer::SourceRange;
use super::parser::ParsedModule;
use super::types::TypeId;

pub struct Module {
  pub public_global_variables: HashMap<String, GlobalId>,
  pub public_extern_functions: HashMap<String, ExternId>,
}

#[derive(Clone, Copy)]
pub struct GlobalTypes<'a> {
  types: &'a HashMap<Identifier, TypeId>,
}

impl<'a> GlobalTypes<'a> {
  pub fn get_type(&self, id: Identifier) -> TypeId {
    *self.types.get(&id).unwrap()
  }

  pub fn new(global_env: &'a GlobalEnv) -> Self {
    Self {
      types: &global_env.types,
    }
  }
}

pub struct GlobalEnv {
  types: HashMap<Identifier, TypeId>,
  module_names: HashMap<String, ModuleId>,
  modules: Vec<Module>,
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
      debug_assert!(
        !self.module_names.contains_key(&module_name),
        "this error should have been detected earlier by calling `is_module_name_available`"
      );

      let module_id = self.modules.len() as u16;
      self.module_names.insert(module_name, ModuleId(module_id));

      // TODO: CLEANUP THIS!!!!!
      for id in parsed_module.globals.values() {
        if id.is_relative() && id.is_public() {
          let type_id = parsed_module.module_global_types[id.get_relative() as usize];
          self.types.insert(
            GlobalId::absolute(module_id, id.get_relative()).into(),
            type_id,
          );
        }
      }

      for id in parsed_module.extern_functions.values() {
        if id.is_relative() && id.is_public() {
          let type_id = parsed_module.module_extern_functions_types[id.get_relative() as usize];
          self.types.insert(
            ExternId::absolute(module_id, id.get_relative()).into(),
            type_id,
          );
        }
      }

      let public_global_variables = parsed_module
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

      let public_extern_functions = parsed_module
        .extern_functions
        .into_iter()
        .filter(|(_, gid)| gid.is_public())
        .filter_map(|(name, gid)| {
          if gid.is_relative() {
            let relative_id = gid.get_relative();
            Some((name, ExternId::absolute(module_id, relative_id)))
          } else {
            None
          }
        })
        .collect::<HashMap<_, _>>();

      self.modules.push(Module {
        public_global_variables,
        public_extern_functions,
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
      .public_global_variables
      .get(name)
      .copied()
  }

  pub fn new() -> Self {
    Self {
      types: HashMap::new(),
      module_names: HashMap::new(),
      modules: Vec::new(),
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
