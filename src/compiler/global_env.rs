use std::collections::HashMap;

use crate::compiler::identifier::VariableIdentifier;

use super::errors::{ge_err, CompilerResult};
use super::identifier::{ExternId, GlobalIdentifier, GlobalVarId, Identifier, ModuleId};
use super::lexer::SourceRange;
use super::parser::ParsedModule;
use super::types::TypeId;

pub struct Module {
  pub global_names: HashMap<String, GlobalIdentifier>,
}

#[derive(Clone, Copy)]
pub struct GlobalTypes<'a> {
  variable_types: &'a [TypeId],
  extern_functions_types: &'a [TypeId],
}

impl<'a> GlobalTypes<'a> {
  pub fn get_type(&self, id: Identifier) -> TypeId {
    match id {
      Identifier::Variable(VariableIdentifier::Global(id)) => {
        assert!(!id.is_relative());
        self.variable_types[id.get_id() as usize]
      }
      Identifier::ExternFunction(id) => {
        assert!(!id.is_relative());
        self.extern_functions_types[id.get_id() as usize]
      }
      _ => panic!("invalid identifier"),
    }
  }

  pub fn new(global_env: &'a GlobalEnv) -> Self {
    Self {
      variable_types: &global_env.variable_types,
      extern_functions_types: &global_env.extern_functions_types,
    }
  }
}

#[derive(Default)]
pub struct GlobalEnv {
  variable_types: Vec<TypeId>,
  extern_functions_types: Vec<TypeId>,
  module_names: HashMap<String, ModuleId>,
  modules: Vec<Module>,
  global_variables_count: u32,
  extern_functions_count: u32,
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

      self
        .variable_types
        .extend(parsed_module.module_global_variable_types.iter());
      self
        .extern_functions_types
        .extend(parsed_module.module_extern_functions_types.iter());

      let mut module_names = HashMap::new();
      for (name, id) in parsed_module.global_names {
        match id {
          GlobalIdentifier::Variable(var_id) => {
            if var_id.is_relative() && var_id.is_public() {
              let absolute_id =
                GlobalVarId::absolute(var_id.get_id() + self.global_variables_count);
              module_names.insert(name, GlobalIdentifier::Variable(absolute_id));
            }
          }
          GlobalIdentifier::ExternFunction(ext_id) => {
            if ext_id.is_relative() && ext_id.is_public() {
              let absolute_id = ExternId::absolute(ext_id.get_id() + self.global_variables_count);
              module_names.insert(name, GlobalIdentifier::ExternFunction(absolute_id));
            }
          }
        }
      }

      self.modules.push(Module {
        global_names: module_names,
      });
      Some(ModuleId(module_id))
    } else {
      None
    }
  }

  pub fn is_module_name_available(&self, name: &str) -> bool {
    !self.module_names.contains_key(name)
  }

  pub fn get_id(&self, module_id: ModuleId, name: &str) -> Option<GlobalIdentifier> {
    self.modules[module_id.0 as usize]
      .global_names
      .get(name)
      .copied()
  }

  pub fn new() -> Self {
    Default::default()
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
