use std::collections::HashMap;

use super::{
  codegen::BytecodeBuilder,
  identifier::{GlobalId, ModuleId},
};

pub type GlobalNames = HashMap<String, GlobalId>;

#[derive(Default, Clone)]
pub struct Module {
  pub id: ModuleId,
  pub extern_functions: Vec<GlobalId>,
  pub imports: Vec<ModuleId>,
  pub code: BytecodeBuilder,
  pub global_identifiers: usize,
}

#[derive(Default)]
pub struct LoadedModules {
  pub extern_functions: Vec<GlobalId>,
  pub module_ids: HashMap<String, ModuleId>,
}
