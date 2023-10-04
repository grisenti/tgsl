use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::rc::Rc;

use crate::compiler::types::Type;

pub type LocalAddress = u8;
pub type CaptureAddress = u8;
pub type RelativeGlobalVarAddress = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GlobalVarAddress {
  AbsoluteNative(u32),
  RelativeNative(u32),
}

impl GlobalVarAddress {
  fn from_address_and_kind(address: RelativeGlobalVarAddress, kind: GlobalVariableKind) -> Self {
    match kind {
      GlobalVariableKind::ImportedNative => GlobalVarAddress::AbsoluteNative(address),
      GlobalVariableKind::RelativeNative => GlobalVarAddress::RelativeNative(address),
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GlobalVarDeclarationError {
  MultipleDeclarations,
  RedeclaredImport,
}

pub type GlobalVarDeclarationResult<T> = Result<T, GlobalVarDeclarationError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LocalVarDeclarationError {
  TooManyLocalNames,
  AlreadyDefined,
}

pub type LocalVarDeclarationResult<T> = Result<T, LocalVarDeclarationError>;

#[derive(Clone, Copy, PartialEq, Eq)]
enum GlobalVariableKind {
  ImportedNative,
  RelativeNative,
}

#[derive(Clone)]
struct GlobalVariable {
  type_: Type,
  address: u32,
  kind: GlobalVariableKind,
}

#[derive(Clone, Default)]
pub struct GlobalVariables {
  variables: HashMap<Rc<str>, GlobalVariable>,
  last_relative_address: u32,
}

pub struct ExportedGlobalVariables {
  variables: HashMap<Rc<str>, GlobalVariable>,
}

impl ExportedGlobalVariables {
  pub fn link(mut self, global_address_start: u32) -> LinkedGlobalVariables {
    for (_, var) in &mut self.variables {
      var.kind = GlobalVariableKind::ImportedNative;
      var.address += global_address_start;
    }
    LinkedGlobalVariables {
      variables: self.variables,
    }
  }

  pub fn count(&self) -> u32 {
    self.variables.len() as u32
  }
}

pub struct LinkedGlobalVariables {
  variables: HashMap<Rc<str>, GlobalVariable>,
}

impl GlobalVariables {
  pub fn import(&mut self, module_variable: &LinkedGlobalVariables) -> Result<(), ()> {
    self.variables.reserve(module_variable.variables.len());
    for (name, var) in &module_variable.variables {
      match self.variables.entry(name.clone()) {
        Entry::Occupied(_) => {
          todo!()
        }
        Entry::Vacant(e) => {
          e.insert(var.clone());
        }
      }
    }
    Ok(())
  }

  pub fn export(mut self) -> ExportedGlobalVariables {
    self
      .variables
      .retain(|_, var| var.kind == GlobalVariableKind::RelativeNative);
    ExportedGlobalVariables {
      variables: self.variables,
    }
  }

  pub fn declare(
    &mut self,
    name: &str,
    type_: Type,
  ) -> GlobalVarDeclarationResult<RelativeGlobalVarAddress> {
    let address = self.last_relative_address;
    self.last_relative_address += 1;
    match self.variables.entry(Rc::from(name)) {
      Entry::Occupied(_) => {
        todo!()
      }
      Entry::Vacant(e) => {
        e.insert(GlobalVariable {
          address,
          kind: GlobalVariableKind::RelativeNative,
          type_,
        });
      }
    }
    Ok(address)
  }

  pub fn get(&self, name: &str) -> Option<(GlobalVarAddress, Type)> {
    self.variables.get(name).map(|v| {
      (
        GlobalVarAddress::from_address_and_kind(v.address, v.kind),
        v.type_.clone(),
      )
    })
  }

  pub fn is_global_variable(&self, name: &str) -> bool {
    self.variables.contains_key(name)
  }
}
