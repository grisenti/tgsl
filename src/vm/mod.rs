use std::collections::HashMap;

use crate::{
  compiler::{errors::ErrorPrinter, identifier::ExternId, Compiler},
  standard_library::load_standard_library,
};

mod address_table;
mod chunk;

pub mod extern_function;
pub mod runtime;
pub mod value;

use crate::vm::extern_function::ExternFunction;
use chunk::*;

use self::{address_table::AddressTable, runtime::RunTime};

pub struct VM {
  compiler: Compiler,
  address_table: AddressTable,
  run_time: RunTime,
}

impl VM {
  fn process_extern_functions(
    &mut self,
    declared_extern_functions: &HashMap<String, ExternId>,
    extern_functions: Vec<ExternFunction>,
  ) -> Result<Vec<(ExternId, ExternFunction)>, String> {
    let mut vec = vec![];
    for func in extern_functions {
      if let Some(id) = declared_extern_functions.get(func.get_name()) {
        vec.push((*id, func));
      } else {
        return Err("invalid extern function".to_string());
      }
    }
    Ok(vec)
  }

  pub fn load_module(
    &mut self,
    source: &str,
    extern_functions: Vec<ExternFunction>,
  ) -> Result<(), String> {
    let compiled_module = match self.compiler.compile(source) {
      Err(errs) => return Err(ErrorPrinter::to_string(&errs, source)),
      Ok(module) => module,
    };
    let globals_count = compiled_module.globals_count;
    let functions_count = compiled_module.code.functions.len();
    let external_functions =
      self.process_extern_functions(&compiled_module.extern_functions, extern_functions)?;
    self.run_time.interpret(
      GlobalChunk::new(compiled_module.code, &self.address_table),
      external_functions,
      globals_count as u32,
    );
    self.address_table.update_table(
      globals_count as u32,
      compiled_module.extern_functions.len() as u32,
      functions_count as u32,
    );
    Ok(())
  }

  pub fn new() -> Self {
    Default::default()
  }
}

impl Default for VM {
  fn default() -> Self {
    let mut vm = Self {
      compiler: Compiler::new(),
      address_table: AddressTable::default(),
      run_time: RunTime::default(),
    };
    load_standard_library(&mut vm);
    vm
  }
}
