use chunk::*;

use crate::compiler::functions;
use crate::compiler::types as comp_types;
use crate::compiler::CompiledModule;
use crate::foreign_function::{ForeignFunction, ForeignFunctionInfo};
use crate::value as api_types;

use self::{address_table::AddressTable, runtime::RunTime};

mod address_table;
mod chunk;

pub mod runtime;
pub mod value;

#[derive(Default)]
pub struct VM {
  address_table: AddressTable,
  run_time: RunTime,
}

fn compare_types(provided_type: &api_types::Type, required_type: &comp_types::Type) -> bool {
  match provided_type {
    api_types::Type::Num => *required_type == comp_types::Type::Num,
    api_types::Type::Str => *required_type == comp_types::Type::Str,
    api_types::Type::Bool => *required_type == comp_types::Type::Bool,
    api_types::Type::Unit => *required_type == comp_types::Type::Nothing,
    _ => unimplemented!(),
  }
}

fn compare_parameters(
  provided_fn_params: &[api_types::Type],
  required_fn_params: &[comp_types::Type],
) -> bool {
  provided_fn_params
    .iter()
    .zip(required_fn_params.iter())
    .all(|(p, r)| compare_types(p, r))
}

fn process_foreign_functions(
  runtime_foreign_functions: &mut Vec<ForeignFunction>,
  declared_foreign_functions: &[functions::ForeignFunction],
  foreign_functions: Vec<ForeignFunctionInfo>,
) -> Result<(), String> {
  // ensure all declared functions are provided
  // ensure all provided functions exist

  let mut functions = Vec::with_capacity(foreign_functions.len());
  for foreign_func in foreign_functions {
    if let Some(func) = declared_foreign_functions.iter().find(|f| {
      f.name.as_ref() == foreign_func.get_name()
        && compare_parameters(foreign_func.get_parameters(), f.signature.get_parameters())
    }) {
      if !compare_types(
        foreign_func.get_return_type(),
        func.signature.get_return_type(),
      ) {
        return Err(format!(
          "inconsistent return types for function '{}'",
          foreign_func.get_name()
        ));
      }
      functions.push((func.relative_address, foreign_func.get_foreign_function()))
    } else {
      return Err(format!(
        "no foreign function named {} in module",
        foreign_func.get_name()
      ));
    }
  }
  assert!(functions.len() <= declared_foreign_functions.len());
  if functions.len() < declared_foreign_functions.len() {
    return Err("missing foreign function".to_string());
  }
  functions.sort_by_key(|(key, _)| *key);
  for (_, func) in functions {
    runtime_foreign_functions.push(func);
  }
  Ok(())
}

impl VM {
  pub fn load_module(
    &mut self,
    compiled_module: CompiledModule,
    foreign_functions: Vec<ForeignFunctionInfo>,
  ) -> Result<(), String> {
    let globals_count = compiled_module.globals_count;
    let functions_count = compiled_module.code.functions.len();
    process_foreign_functions(
      &mut self.run_time.foreign_functions,
      &compiled_module.foreign_functions,
      foreign_functions,
    )?;
    unsafe {
      self
        .run_time
        .interpret(
          GlobalChunk::new(compiled_module.code, &self.address_table),
          globals_count as u32,
        )
        .map_err(|_| "stack overflow")?
    };
    self.address_table.update_table(
      globals_count as u32,
      compiled_module.foreign_functions.len() as u32,
      functions_count as u32,
    );
    Ok(())
  }

  pub fn new() -> Self {
    Default::default()
  }
}
