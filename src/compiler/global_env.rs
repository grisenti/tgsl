use std::collections::HashMap;

use super::errors::{ge_err, CompilerError, CompilerResult};
use super::identifier::{GlobalId, Identifier, ModuleId};
use super::lexer::SourceRange;
use super::types::TypeId;

type GlobalName = Vec<(GlobalId, ModuleId)>;

#[derive(Default)]
pub struct GlobalEnv {
  names: HashMap<String, GlobalName>,
  pub types: Vec<TypeId>,
  last_global_id: GlobalId,
  last_module_id: u16,
}

impl GlobalEnv {
  fn new_global_id(&mut self) -> GlobalId {
    let id = self.last_global_id;
    self.last_global_id += 1;
    id
  }

  pub fn get_name(
    &self,
    modules: &[ModuleId],
    name: &str,
    name_sr: SourceRange,
  ) -> CompilerResult<GlobalId> {
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
        Ok(candidates[0].0)
      }
    } else {
      Err(ge_err::undeclared_global(name_sr))
    }
  }

  pub fn declare_name(
    &mut self,
    module: ModuleId,
    name: &str,
    name_sr: SourceRange,
  ) -> CompilerResult<GlobalId> {
    let id = self.new_global_id();
    self.types.push(TypeId::UNKNOWN);
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

  pub fn new_module(&mut self) -> ModuleId {
    let id = self.last_module_id;
    self.last_module_id += 1;
    ModuleId(id)
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

  /*

    pub fn get_global_function(&self, name: &str, arguments: &[TypeId]) {}

    pub fn create_new_global_variable() -> CompilerResult<GlobalId> {}

    pub fn get_type(&self, id: GlobalId) -> TypeId {}

    pub fn new_module(name: &str) -> ModuleId {}

    pub fn get_current_module_id(&self) -> ModuleId {}

    fn add_name(&mut self, name: String, type_id: TypeId, name_sr: SourceRange) -> Identifier {
      debug_assert!(!self.current_module_names.contains_key(&name));

      let id = self.last_global_id;
      self.current_module_names.insert(name, (id, type_id));
      self.last_global_id += 1;
      self.types.push(TypeId::NONE);
      self.current_module_first_uses.push(name_sr);
      self.current_module_declarations.push(false);
      Identifier::Global(id)
    }

    /// returns:
    /// - `Ok(Some(id))` if the name exists in one of the provided modules or in the current module
    /// - `Ok(None)` if it doesn't exist
    /// - `Err(...)` if there are multiple
    pub fn get_name(
      &self,
      modules: &[ModuleId],
      name: &str,
      name_sr: SourceRange,
    ) -> CompilerResult<(Identifier, TypeId)> {
      let in_current_module = self
        .current_module_names
        .get(name)
        .copied()
        .map(Identifier::Global);
      modules
        .iter()
        .filter_map(|id| self.names[id.0 as usize].get(name))
        .try_fold(in_current_module, |acc, id| match acc {
          None => Ok(Some(Identifier::Global(*id))),
          Some(_) => Err(ge_err::identifier_declare_in_multiple_modules(
            name_sr, name,
          )),
        })
    }



    pub fn get_globals_count(&self, module_id: ModuleId) -> usize {
      self.names[module_id.0 as usize].len()
    }

    pub fn get_or_add(
      &mut self,
      imported_modules: &[ModuleId],
      name: &str,
      name_sr: SourceRange,
    ) -> CompilerResult<Identifier> {
      self
        .get_name(imported_modules, name, name_sr)
        .map(|id| id.unwrap_or_else(|| self.add_name(name.to_owned(), name_sr)))
    }

    pub fn declare_name(&mut self, name: &str, name_sr: SourceRange) -> CompilerResult<Identifier> {
      // FIXME: copy paste from finalize_current_module
      let first_module_id = self.last_global_id as usize - self.current_module_names.len();
      if let Some(&id) = self.current_module_names.get(name) {
        let module_relative_id = id as usize - first_module_id;
        if self.current_module_declarations[module_relative_id] {
          Err(ge_err::identifier_redeclaration(name_sr, name))
        } else {
          self.current_module_declarations[module_relative_id] = true;
          Ok(Identifier::Global(id))
        }
      } else {
        let id = self.add_name(name.to_string(), name_sr);
        *self.current_module_declarations.last_mut().unwrap() = true;
        Ok(id)
      }
    }

    fn clear_current_module(&mut self) {
      self.current_module_first_uses.clear();
      self.current_module_declarations.clear();
      let mut t = HashMap::with_capacity(self.current_module_names.len());
      std::mem::swap(&mut t, &mut self.current_module_names);
      self.names.push(t);
    }

    pub fn finalize_current_module(&mut self) -> Result<(), Vec<CompilerError>> {
      let mut errs = Vec::new();
      let first_module_id = self.last_global_id as usize - self.current_module_names.len();
      for id in self.current_module_names.values() {
        let module_relative_id = *id as usize - first_module_id;
        if !self.current_module_declarations[module_relative_id] {
          let first_use_sr = self.current_module_first_uses[module_relative_id as usize];
          errs.push(ge_err::undeclared_global(first_use_sr));
        }
      }
      self.clear_current_module();
      if !errs.is_empty() {
        Err(errs)
      } else {
        Ok(())
      }
    }

    pub fn set_type_if_global(&mut self, id: Identifier, type_id: TypeId) {
      if let Identifier::Global(id) = id {
        self.types[id as usize] = type_id;
      }
    }


  */
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
