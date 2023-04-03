use std::collections::HashMap;

use super::{
  bytecode::Chunk,
  identifier::{GlobalId, ModuleId},
};

pub type GlobalNames = HashMap<String, GlobalId>;

#[derive(Default, Clone)]
pub struct Module {
  pub id: ModuleId,
  pub extern_functions: Vec<GlobalId>,
  pub imports: Vec<ModuleId>,
  pub code: Chunk,
}

#[derive(Default)]
pub struct LoadedModules {
  pub extern_functions: Vec<GlobalId>,
  pub module_ids: HashMap<String, ModuleId>,
}
