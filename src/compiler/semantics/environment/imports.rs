use crate::compiler::global_env::Module;
use crate::compiler::identifier::{GlobalIdentifier, OverloadId};
use crate::compiler::semantics::environment::Environment;
use std::collections::hash_map::Entry;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportError {
  NameRedefinition(String),
  OverloadRedefinition(String),
  NotAValidModule,
}

pub type ImportResult = Result<(), ImportError>;

impl<'src> Environment<'src> {
  fn import_overload(
    &mut self,
    module: &Module,
    name: &str,
    module_overload_id: &OverloadId,
  ) -> ImportResult {
    if let Some(global_id) = self.global_names.get(name).copied() {
      if let GlobalIdentifier::OverloadId(overload_id) = global_id {
        if !self.overloads[overload_id as usize]
          .merge(&module.overloads[*module_overload_id as usize])
        {
          return Err(ImportError::OverloadRedefinition(name.to_owned()));
        }
        self.global_names.insert(name.to_owned(), global_id);
      } else {
        return Err(ImportError::NameRedefinition(name.to_owned()));
      }
    } else {
      let overload_id = self.overloads.len();
      self
        .overloads
        .push(module.overloads[*module_overload_id as usize].clone());
      self.global_names.insert(
        name.to_owned(),
        GlobalIdentifier::OverloadId(overload_id as u32),
      );
    }
    Ok(())
  }

  fn import_name(
    &mut self,
    module: &Module,
    name: &str,
    global_id: &GlobalIdentifier,
  ) -> ImportResult {
    match global_id {
      GlobalIdentifier::OverloadId(module_overload_id) => {
        self.import_overload(&module, name, module_overload_id)?
      }
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
      self.import_name(&module, name, global_id)?;
    }
    Ok(())
  }
}
