use std::collections::HashMap;

use super::{
  bytecode::Chunk,
  identifier::{GlobalId, ModuleId},
  types::TypeId,
};

pub type GlobalNames = HashMap<String, GlobalId>;

#[derive(Default, Clone)]
pub struct Module {
  pub extern_functions: Vec<GlobalId>,
  pub imports: Vec<ModuleId>,
  pub code: Chunk,
  pub names: GlobalNames,
}

#[derive(Default)]
pub struct ModuleNames {
  names: Vec<GlobalNames>,
  pub last_global_id: u16,
}

impl ModuleNames {
  pub fn last_global_id(&self) -> u16 {
    self.last_global_id
  }

  pub fn update(&mut self, module_names: GlobalNames) -> ModuleId {
    let id = self.names.len() as u16;
    self.names.push(module_names);
    ModuleId(id)
  }

  pub fn get_id(&self, imports: &[ModuleId], name: &str) -> Result<Option<GlobalId>, ()> {
    imports
      .iter()
      .filter_map(|id| self.names[id.0 as usize].get(name))
      .fold(Ok(None), |acc, id| match acc {
        Err(_) => Err(()),
        Ok(None) => Ok(Some(*id)),
        Ok(Some(_)) => Err(()),
      })
  }
}

#[derive(Default)]
pub struct LoadedModules {
  pub module_names: ModuleNames,
  pub extern_functions: Vec<GlobalId>,
  pub module_ids: HashMap<String, ModuleId>,
}
