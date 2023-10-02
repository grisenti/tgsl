use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::rc::Rc;

use overload_set::OverloadSet;

use crate::compiler::functions::overload_set::{FindError, Function, FunctionKind};
use crate::compiler::types::FunctionSignature;

pub mod overload_set;

pub struct ExternFunction {
  pub name: Rc<str>,
  pub signature: FunctionSignature,
  pub relative_address: RelativeFunctionAddress,
}

pub type RelativeFunctionAddress = u32;

#[derive(Debug, Clone, Copy)]
pub enum FunctionInsertError {
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
  last_extern_function: u32,
}

pub struct ExportedFunctions {
  function_names: HashMap<Rc<str>, OverloadSet>,
  pub native_count: u32,
  pub extern_count: u32,
}

impl ExportedFunctions {
  pub fn link(
    mut self,
    last_native_function_address: u32,
    last_extern_function_address: u32,
  ) -> LinkedFunctions {
    for (_, overload_set) in &mut self.function_names {
      overload_set.link(last_native_function_address, last_extern_function_address)
    }
    LinkedFunctions {
      function_names: self.function_names,
    }
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
    let address = self.last_native_function;
    let res = self.declare(address, FunctionKind::Undefined, name, signature);
    self.last_native_function += 1;
    res
  }

  pub fn declare_extern(
    &mut self,
    name: &str,
    signature: FunctionSignature,
  ) -> FunctionInsertResult {
    let address = self.last_extern_function;
    let res = self.declare(address, FunctionKind::RelativeExtern, name, signature);
    self.last_extern_function += 1;
    res
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

  pub fn create_lambda(&mut self) -> RelativeFunctionAddress {
    let address = self.last_native_function;
    self.last_native_function += 1;
    address
  }

  pub fn get_overload_set(&self, name: &str) -> Option<&OverloadSet> {
    self.function_names.get(name)
  }

  pub fn export(mut self) -> (ExportedFunctions, Vec<ExternFunction>) {
    let mut extern_functions = Vec::with_capacity(self.last_extern_function as usize);

    self.function_names.retain(|name, overload_set| {
      if overload_set.export() {
        extern_functions.extend(overload_set.relative_extern_iter().map(|f| ExternFunction {
          name: name.clone(),
          relative_address: f.address,
          signature: f.signature,
        }));
        true
      } else {
        false
      }
    });
    (
      ExportedFunctions {
        function_names: self.function_names,
        native_count: self.last_native_function,
        extern_count: self.last_extern_function,
      },
      extern_functions,
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
