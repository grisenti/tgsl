use std::collections::{HashMap, HashSet};
use std::slice::SliceIndex;

use crate::errors::{SourceError, SourceInfo};

use super::error_from_source_info;
use super::identifier::{GlobalId, Identifier, ModuleId};
use super::modules::Module;
use super::types::TypeId;

pub type NameQueryResult = Result<Option<Identifier>, ()>;
pub type ModuleGlobalNames = HashMap<String, GlobalId>;

#[derive(Default)]
pub struct GlobalEnv {
  names: Vec<ModuleGlobalNames>,
  pub types: Vec<TypeId>,
  last_global_id: GlobalId,

  current_module_declarations: Vec<bool>,
  current_module_names: ModuleGlobalNames,
  current_module_first_uses: Vec<SourceInfo>,
}

impl GlobalEnv {
  fn add_name(&mut self, name: String, source_info: SourceInfo) -> Identifier {
    debug_assert!(!self.current_module_names.contains_key(&name));
    let id = self.last_global_id;
    self.current_module_names.insert(name, id);
    self.last_global_id += 1;
    self.types.push(TypeId::NOTHING);
    self.current_module_first_uses.push(source_info);
    self.current_module_declarations.push(false);
    Identifier::Global(id)
  }

  /// returns:
  /// - `Ok(Some(id))` if the name exists in one of the provided modules or in the current module
  /// - `Ok(None)` if it doesn't exist
  /// - `Err(())` if there are multiple
  pub fn get_name(
    &self,
    modules: &[ModuleId],
    name: &str,
    source_info: SourceInfo,
  ) -> Result<Option<Identifier>, SourceError> {
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
        Some(_) => Err(error_from_source_info(
          &source_info,
          format!("identifier '{name}' declared in multiple imported modules"),
        )),
      })
  }

  pub fn get_id(&self, module_id: ModuleId, name: &str) -> Option<GlobalId> {
    self.names[module_id.0 as usize].get(name).copied()
  }

  pub fn get_globals_count(&self, module_id: ModuleId) -> usize {
    self.names[module_id.0 as usize].len()
  }

  pub fn get_or_add(
    &mut self,
    imported_modules: &[ModuleId],
    name: &str,
    source_info: SourceInfo,
  ) -> Result<Identifier, SourceError> {
    self
      .get_name(imported_modules, name, source_info)
      .and_then(|id| Ok(id.unwrap_or_else(|| self.add_name(name.to_owned(), source_info))))
  }

  pub fn declare_name(
    &mut self,
    name: &str,
    source_info: SourceInfo,
  ) -> Result<Identifier, SourceError> {
    // FIXME: copy paste from finalize_current_module
    let first_module_id = self.last_global_id as usize - self.current_module_names.len();
    if let Some(&id) = self.current_module_names.get(name) {
      let module_relative_id = id as usize - first_module_id;
      if self.current_module_declarations[module_relative_id] {
        Err(error_from_source_info(
          &source_info,
          format!("redeclaration of global identifier '{name}'"),
        ))
      } else {
        self.current_module_declarations[module_relative_id] = true;
        Ok(Identifier::Global(id))
      }
    } else {
      let id = self.add_name(name.to_string(), source_info);
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

  pub fn finalize_current_module(&mut self) -> Result<(), SourceError> {
    let mut errs = Vec::new();
    let first_module_id = self.last_global_id as usize - self.current_module_names.len();
    for (_, id) in &self.current_module_names {
      let module_relative_id = *id as usize - first_module_id;
      if !self.current_module_declarations[module_relative_id] {
        errs.push(error_from_source_info(
          &self.current_module_first_uses[module_relative_id],
          "identifier was not declared in the current module".to_string(),
        ));
      }
    }
    self.clear_current_module();
    if errs.len() > 0 {
      Err(SourceError::from_err_vec(errs))
    } else {
      Ok(())
    }
  }

  pub fn set_type_if_global(&mut self, id: Identifier, type_id: TypeId) {
    if let Identifier::Global(id) = id {
      self.types[id as usize] = type_id;
    }
  }

  pub fn new() -> Self {
    Default::default()
  }
}
