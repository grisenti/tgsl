use std::collections::hash_map::Entry;

use crate::compiler::identifier::GlobalIdentifier;
use crate::compiler::semantics::environment::Environment;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportError {
  NameRedefinition(String),
  OverloadRedefinition(String),
  NotAValidModule,
}

pub type ImportResult = Result<(), ImportError>;

impl<'src> Environment<'src> {
  fn import_name(&mut self, name: &str, global_id: &GlobalIdentifier) -> ImportResult {
    match global_id {
      other => match self.global_names.entry(name.to_owned()) {
        Entry::Occupied(_) => return Err(ImportError::NameRedefinition(name.to_owned())),
        Entry::Vacant(entry) => {
          entry.insert(*other);
        }
      },
    }
    Ok(())
  }

  pub fn import_module(&mut self, name: &str) -> ImportResult {
    let module = if let Some(module_id) = self.global_env.get_module(name) {
      module_id
    } else {
      return Err(ImportError::NotAValidModule);
    };
    for (name, global_id) in &module.global_names {
      self.import_name(name, global_id)?;
    }
    self.global_functions.import(&module.functions);
    self
      .global_structs
      .import(&module.structs)
      .expect("todo: consider errors");
    Ok(())
  }
}
