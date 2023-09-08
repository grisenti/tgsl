use crate::compiler::overload_set::OverloadSet;
use core::panic;
use std::collections::HashMap;

use super::identifier::{ExternId, GlobalIdentifier, GlobalVarId, ModuleId, StructId};
use super::lexer::SourceRange;
use super::types::Type;

pub struct Module {
  pub global_names: HashMap<String, GlobalIdentifier>,
  pub overloads: Vec<OverloadSet>,
}

pub struct Struct {
  name: String,
  member_names: Vec<String>,
  member_types: Vec<Type>,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub struct MemberIndex(usize);

impl MemberIndex {
  pub fn get_index(self) -> usize {
    self.0
  }
}

impl Struct {
  pub fn new(name: String, member_names: Vec<String>, member_types: Vec<Type>) -> Self {
    assert_eq!(member_types.len(), member_types.len());
    Self {
      name,
      member_names,
      member_types,
    }
  }

  pub fn get_member_index(&self, member_name: &str) -> Option<MemberIndex> {
    self
      .member_names
      .iter()
      .position(|name| name == member_name)
      .map(MemberIndex)
  }
  pub fn member_info(&self, index: MemberIndex) -> (&str, &Type) {
    assert!(index.0 < self.member_types.len());
    (&self.member_names[index.0], &self.member_types[index.0])
  }

  pub fn get_name(&self) -> &str {
    &self.name
  }

  pub fn get_member_types(&self) -> &[Type] {
    &self.member_types
  }
}

#[derive(Default)]
pub struct GlobalEnv {
  variable_types: Vec<Type>,
  extern_functions_types: Vec<Type>,
  structs: Vec<Struct>,
  module_names: HashMap<String, ModuleId>,
  modules: Vec<Module>,
  global_variables_count: u32,
  extern_functions_count: u32,
  structs_count: u32,
  exported_functions: u32,
}

pub struct ParsedModule {}

impl GlobalEnv {
  pub fn get_module(&self, module_name: &str) -> Option<&Module> {
    let id = self.module_names.get(module_name)?;
    Some(&self.modules[id.0 as usize])
  }

  pub fn get_variable_type(&self, id: GlobalVarId) -> &Type {
    assert!(!id.is_relative());
    &self.variable_types[id.get_id() as usize]
  }

  pub fn get_extern_function_type(&self, id: ExternId) -> &Type {
    assert!(!id.is_relative());
    &self.extern_functions_types[id.get_id() as usize]
  }

  pub fn get_struct(&self, id: StructId) -> &Struct {
    assert!(!id.is_relative());
    &self.structs[id.get_id() as usize]
  }

  pub fn export_module(&mut self, parsed_module: ParsedModule) -> Option<ModuleId> {
    todo!();
    /*
    if let Some(module_name) = parsed_module.module_name {
      debug_assert!(
        !self.module_names.contains_key(&module_name),
        "this error should have been detected earlier by calling `is_module_name_available`"
      );

      let module_id = self.modules.len() as u16;
      self.module_names.insert(module_name, ModuleId(module_id));

      self
        .variable_types
        .extend(parsed_module.module_global_variable_types.into_iter());
      self
        .extern_functions_types
        .extend(parsed_module.module_extern_functions_types.into_iter());

      let mut module_names = HashMap::new();
      let mut exported_variables = 0;
      let mut exported_extern_functions = 0;
      let mut exported_structs = 0;
      for (name, id) in parsed_module.global_names {
        match id {
          GlobalIdentifier::Variable(var_id) => {
            if var_id.is_relative() && var_id.is_public() {
              let absolute_id =
                GlobalVarId::absolute(var_id.get_id() + self.global_variables_count);
              module_names.insert(name, GlobalIdentifier::Variable(absolute_id));
              exported_variables += 1;
            }
          }
          GlobalIdentifier::ExternFunction(ext_id) => {
            if ext_id.is_relative() && ext_id.is_public() {
              let absolute_id = ExternId::absolute(ext_id.get_id() + self.extern_functions_count);
              module_names.insert(name, GlobalIdentifier::ExternFunction(absolute_id));
              exported_extern_functions += 1;
            }
          }
          GlobalIdentifier::Struct(struct_id) => {
            if struct_id.is_relative() && struct_id.is_public() {
              let absolute_id = GlobalVarId::absolute(struct_id.get_id() + self.structs_count);
              module_names.insert(name, GlobalIdentifier::Variable(absolute_id));
              exported_structs += 1;
            }
          }
          GlobalIdentifier::OverloadId(overload_id) => {
            module_names.insert(name, GlobalIdentifier::OverloadId(overload_id));
          }
          _ => panic!(),
        }
      }
      let mut exported_functions = 0;
      let overloads = parsed_module
        .module_overloads
        .into_iter()
        .map(|overload_set| {
          let (set, count) = overload_set.export_set(self.exported_functions);
          exported_functions += count;
          set
        })
        .collect();

      self.global_variables_count += exported_variables;
      self.extern_functions_count += exported_extern_functions;
      self.structs_count += exported_structs;
      self.exported_functions += exported_functions as u32;
      self.modules.push(Module {
        global_names: module_names,
        overloads,
      });
      Some(ModuleId(module_id))
    } else {
      None
    }*/
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
pub mod test {
  use super::{GlobalEnv, Module};
  use lazy_static::lazy_static;

  lazy_static! {
    pub static ref EMPTY_GLOBAL_ENV: GlobalEnv = GlobalEnv::new();
  }

  impl GlobalEnv {
    pub fn add_fake_module(&mut self, _module: Module) {}
  }
}
