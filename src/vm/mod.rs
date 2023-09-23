use crate::{
  compiler::{errors::ErrorPrinter, Compiler},
  standard_library::load_standard_library,
};

mod address_table;
mod chunk;

pub mod extern_function;
pub mod runtime;
pub mod value;

use crate::compiler::ModuleExternFunctions;
use crate::vm::extern_function::{ExternFunction, ExternFunctionInfo};
use chunk::*;

use self::{address_table::AddressTable, runtime::RunTime};

pub struct VM {
  compiler: Compiler,
  address_table: AddressTable,
  run_time: RunTime,
}

fn process_extern_functions(
  runtime_extern_functions: &mut Vec<ExternFunction>,
  declared_extern_functions: &ModuleExternFunctions,
  extern_functions: Vec<ExternFunctionInfo>,
) -> Result<(), String> {
  // ensure all declared functions are provided
  // ensure all provided functions exist

  let mut functions = Vec::with_capacity(extern_functions.len());
  for extern_func in extern_functions {
    if let Some(func_info) = declared_extern_functions.get(extern_func.get_name()) {
      if func_info.signature.get_parameters() != extern_func.get_parameters() {
        return Err(format!(
          "inconsistent parameters for function '{}'",
          extern_func.get_name()
        ));
      }
      if func_info.signature.get_return_type() != extern_func.get_return_type() {
        return Err(format!(
          "inconsistent return types for function '{}'",
          extern_func.get_name()
        ));
      }
      functions.push((func_info.id, extern_func.get_extern_function()))
    } else {
      return Err(format!(
        "no extern function named {} in module",
        extern_func.get_name()
      ));
    }
  }
  assert!(functions.len() <= declared_extern_functions.len());
  if functions.len() < declared_extern_functions.len() {
    return Err("missing extern function".to_string());
  }
  functions.sort_by_key(|(key, _)| key.get_id());
  for (_, func) in functions {
    runtime_extern_functions.push(func);
  }
  Ok(())
}

impl VM {
  pub fn load_module(
    &mut self,
    source: &str,
    extern_functions: Vec<ExternFunctionInfo>,
  ) -> Result<(), String> {
    let compiled_module = match self.compiler.compile(source) {
      Err(errs) => return Err(ErrorPrinter::to_string(&errs, source)),
      Ok(module) => module,
    };
    let globals_count = compiled_module.globals_count;
    let functions_count = compiled_module.code.functions.len();
    process_extern_functions(
      &mut self.run_time.extern_functions,
      &compiled_module.extern_functions,
      extern_functions,
    )?;
    unsafe {
      self.run_time.interpret(
        GlobalChunk::new(compiled_module.code, &self.address_table),
        globals_count as u32,
      )
    };
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
