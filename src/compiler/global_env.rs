use std::collections::hash_map::Entry;
use std::collections::HashMap;

use super::errors::{ge_err, parser_err, CompilerResult};
use super::identifier::{GlobalId, ModuleId};
use super::lexer::SourceRange;
use super::types::TypeId;

type GlobalName = Vec<(GlobalId, ModuleId)>;

#[derive(Default)]
pub struct GlobalEnv {
  names: HashMap<String, GlobalName>,
  pub types: Vec<TypeId>,
  modules: HashMap<String, ModuleId>,
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

  pub fn get_name(
    &self,
    modules: &[ModuleId],
    name: &str,
    name_sr: SourceRange,
  ) -> CompilerResult<Option<GlobalId>> {
    if let Some(global_name) = self.names.get(name) {
      let candidates = global_name
        .iter()
        .filter(|(_, module_id)| modules.contains(module_id))
        .collect::<Vec<_>>(); // TODO: could avoid allocation by reusing the memory
      if candidates.is_empty() {
        Err(ge_err::undeclared_global(name_sr))
      } else if candidates.len() > 1 {
        Err(ge_err::identifier_declare_in_multiple_modules(
          name_sr, name,
        ))
      } else {
        Ok(Some(candidates[0].0))
      }
    } else {
      Ok(None)
    }
  }

  pub fn declare_name(
    &mut self,
    module: ModuleId,
    name: &str,
    name_sr: SourceRange,
  ) -> CompilerResult<GlobalId> {
    let id = self.new_global_id();
    if let Some(global_name) = self.names.get_mut(name) {
      let count = global_name
        .iter()
        .filter(|(_, module_id)| module == *module_id)
        .count();
      if count != 0 {
        Err(ge_err::identifier_redeclaration(name_sr, name))
      } else {
        global_name.push((id, module));
        Ok(id)
      }
    } else {
      self.names.insert(name.to_owned(), vec![(id, module)]);
      Ok(id)
    }
  }

  pub fn new_module(
    &mut self,
    module_name: &str,
    name_sr: SourceRange,
  ) -> CompilerResult<ModuleId> {
    match self.modules.entry(module_name.to_string()) {
      Entry::Vacant(e) => {
        let id = ModuleId(self.last_module_id);
        self.last_module_id += 1;
        e.insert(id);
        Ok(id)
      }
      Entry::Occupied(_) => Err(ge_err::trying_to_redeclare_a_module(name_sr, module_name)),
    }
  }

  pub fn get_module(
    &mut self,
    module_name: &str,
    name_sr: SourceRange,
  ) -> CompilerResult<ModuleId> {
    if let Some(module_id) = self.modules.get(module_name) {
      Ok(*module_id)
    } else {
      Err(ge_err::not_a_loaded_module(name_sr, module_name))
    }
  }

  pub fn get_id(&self, module_id: ModuleId, name: &str) -> Option<GlobalId> {
    self.names.get(name).and_then(|v| {
      v.iter()
        .find(|(_, module)| *module == module_id)
        .map(|(id, _)| id)
        .copied()
    })
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
