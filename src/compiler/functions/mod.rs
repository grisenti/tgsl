use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::rc::Rc;

use overload_set::OverloadSet;

use crate::compiler::functions::overload_set::{FindError, Function, FunctionKind};
use crate::compiler::types::FunctionSignature;

pub mod overload_set;

pub struct ForeignFunction {
  pub name: Rc<str>,
  pub signature: FunctionSignature,
  pub relative_address: RelativeFunctionAddress,
}

pub type RelativeFunctionAddress = u32;

#[derive(Debug, Clone, Copy)]
pub enum FunctionInsertError {
  NameAlreadyAType,
  NameAlreadyAVariable,
  MultipleDeclarations,
  MultipleDefinitions,
  RedefinedImport,
  OverloadOnReturnType,
}

pub type FunctionInsertResult = Result<RelativeFunctionAddress, FunctionInsertError>;

#[derive(Default)]
pub struct GlobalFunctions {
  function_names: HashMap<Rc<str>, OverloadSet>,
  last_native_function: u32,
  last_foreign_function: u32,
}

pub struct ExportedFunctions {
  function_names: HashMap<Rc<str>, OverloadSet>,
  native_count: u32,
  foreign_count: u32,
}

impl ExportedFunctions {
  pub fn link(
    mut self,
    last_native_function_address: u32,
    last_foreign_function_address: u32,
  ) -> LinkedFunctions {
    for (_, overload_set) in &mut self.function_names {
      overload_set.link(last_native_function_address, last_foreign_function_address)
    }
    LinkedFunctions {
      function_names: self.function_names,
    }
  }

  pub fn native_count(&self) -> u32 {
    self.native_count
  }

  pub fn foreign_count(&self) -> u32 {
    self.foreign_count
  }
}

pub struct LinkedFunctions {
  function_names: HashMap<Rc<str>, OverloadSet>,
}

impl GlobalFunctions {
  fn declare(
    &mut self,
    address: RelativeFunctionAddress,
    kind: FunctionKind,
    name: &str,
    signature: FunctionSignature,
  ) -> FunctionInsertResult {
    if let Some(overload_set) = self.function_names.get_mut(name) {
      match overload_set.find(signature.get_parameters()) {
        Ok(_) => todo!(),
        Err(FindError::MultipleDeclarations) => Err(FunctionInsertError::MultipleDeclarations),
        Err(FindError::RedefinedImport) => todo!(),
        Err(FindError::MultipleDefinitions) => todo!(),
        Err(FindError::NotFound) => {
          overload_set.insert(Function {
            address,
            kind,
            signature,
          });
          Ok(address)
        }
      }
    } else {
      self.function_names.insert(
        Rc::from(name),
        OverloadSet::new(Function {
          address,
          kind,
          signature,
        }),
      );
      Ok(address)
    }
  }

  pub fn declare_native(
    &mut self,
    name: &str,
    signature: FunctionSignature,
  ) -> FunctionInsertResult {
    let address = self.new_function_address();
    self.declare(address, FunctionKind::Undefined, name, signature)
  }

  pub fn declare_foreign(
    &mut self,
    name: &str,
    signature: FunctionSignature,
  ) -> FunctionInsertResult {
    let address = self.last_foreign_function;
    let ret = self.declare(address, FunctionKind::RelativeForeign, name, signature);
    self.last_foreign_function += 1;
    ret
  }

  pub fn define_native(
    &mut self,
    name: &str,
    signature: FunctionSignature,
  ) -> FunctionInsertResult {
    if let Some(overload_set) = self.function_names.get_mut(name) {
      match overload_set.find_mut(signature.get_parameters()) {
        Ok(f) => {
          if f.kind == FunctionKind::Undefined {
            f.kind = FunctionKind::RelativeNative;
            Ok(f.address)
          } else {
            todo!()
          }
        }
        Err(FindError::MultipleDeclarations) => Err(FunctionInsertError::MultipleDeclarations),
        Err(FindError::RedefinedImport) => todo!(),
        Err(FindError::MultipleDefinitions) => todo!(),
        Err(FindError::NotFound) => {
          let address = self.last_native_function;
          overload_set.insert(Function {
            address,
            kind: FunctionKind::RelativeNative,
            signature,
          });
          self.last_native_function += 1;
          Ok(address)
        }
      }
    } else {
      let address = self.last_native_function;
      self.function_names.insert(
        Rc::from(name),
        OverloadSet::new(Function {
          address,
          kind: FunctionKind::RelativeNative,
          signature,
        }),
      );
      self.last_native_function += 1;
      Ok(address)
    }
  }

  pub fn new_function_address(&mut self) -> RelativeFunctionAddress {
    let address = self.last_native_function;
    self.last_native_function += 1;
    address
  }

  pub fn get_overload_set(&self, name: &str) -> Option<&OverloadSet> {
    self.function_names.get(name)
  }

  pub fn is_overload_set(&self, name: &str) -> bool {
    self.function_names.contains_key(name)
  }

  pub fn export(mut self) -> (ExportedFunctions, Vec<ForeignFunction>) {
    let mut foreign_functions = Vec::with_capacity(self.last_foreign_function as usize);

    self.function_names.retain(|name, overload_set| {
      if overload_set.export() {
        foreign_functions.extend(
          overload_set
            .relative_foreign_iter()
            .map(|f| ForeignFunction {
              name: name.clone(),
              relative_address: f.address,
              signature: f.signature,
            }),
        );
        true
      } else {
        false
      }
    });
    (
      ExportedFunctions {
        function_names: self.function_names,
        native_count: self.last_native_function,
        foreign_count: self.last_foreign_function,
      },
      foreign_functions,
    )
  }

  pub fn import(&mut self, module_functions: &LinkedFunctions) {
    self
      .function_names
      .reserve(module_functions.function_names.len());
    for (name, overload_set) in &module_functions.function_names {
      match self.function_names.entry(name.clone()) {
        Entry::Occupied(mut e) => {
          e.get_mut().merge(&overload_set);
        }
        Entry::Vacant(e) => {
          e.insert(overload_set.clone());
        }
      }
    }
  }
}
