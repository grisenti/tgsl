use chunk::*;

use crate::compiler::functions;
use crate::compiler::CompiledModule;
use crate::errors::ForeignBindingError::FunctionNameNotInSource;
use crate::errors::{ForeignBindingError, LoadModuleError};
use crate::foreign_function::ForeignFunction;
use crate::gc::Gc;
use crate::vm::interpreter::Interpreter;
use crate::vm::value::TaggedValue;

use self::address_table::AddressTable;

mod address_table;
mod chunk;

mod call_frame;
pub mod gc;
mod interpreter;
pub mod value;

pub type ForeignCallable = Box<dyn Fn(Gc, &[TaggedValue]) -> TaggedValue>;

pub struct VM {
  address_table: AddressTable,
  interpreter: Interpreter,
  functions: Vec<Function>,
  foreign_functions: Vec<ForeignCallable>,
  globals: Vec<TaggedValue>,
}

impl VM {
  pub fn load_module(
    &mut self,
    compiled_module: CompiledModule,
    foreign_functions: Vec<ForeignFunction>,
  ) -> Result<(), LoadModuleError> {
    let globals_count = compiled_module.globals_count;
    let functions_count = compiled_module.code.functions.len();
    process_foreign_functions(
      &mut self.foreign_functions,
      &compiled_module.foreign_functions,
      foreign_functions,
    )
    .map_err(LoadModuleError::ForeignBindingErrors)?;
    let global_chunk = GlobalChunk::new(compiled_module.code, &self.address_table);
    self.functions.extend(global_chunk.functions);
    self.globals.resize(
      self.globals.len() + globals_count as usize,
      TaggedValue::none(),
    );
    self
      .interpreter
      .interpret(
        &global_chunk.global_code,
        &mut self.globals,
        &self.functions,
        &self.foreign_functions,
      )
      .map_err(LoadModuleError::RuntimeError)?;
    self.address_table.update_table(
      globals_count as u32,
      compiled_module.foreign_functions.len() as u32,
      functions_count as u32,
    );
    Ok(())
  }
}

impl Default for VM {
  fn default() -> Self {
    Self {
      address_table: Default::default(),
      interpreter: Default::default(),
      functions: Default::default(),
      foreign_functions: Default::default(),
      globals: vec![],
    }
  }
}

fn process_foreign_functions(
  runtime_foreign_functions: &mut Vec<ForeignCallable>,
  declared_foreign_functions: &[functions::ForeignFunction],
  foreign_functions: Vec<ForeignFunction>,
) -> Result<(), Vec<ForeignBindingError>> {
  // ensure all declared functions are provided
  // ensure all provided functions exist

  let mut functions = Vec::with_capacity(foreign_functions.len());
  let mut errors = Vec::new();

  for foreign_func in foreign_functions {
    if let Some(func) = declared_foreign_functions.iter().find(|f| {
      f.name.as_ref() == foreign_func.get_name()
        && foreign_func.get_parameters() == f.signature.get_parameters()
    }) {
      if foreign_func.get_return_type() != func.signature.get_return_type() {
        errors.push(ForeignBindingError::InconsistentReturnType {
          source_function: func.signature.format_with_name(&func.name),
          provided_return_type: foreign_func.get_return_type().to_string(),
        });
      }
      functions.push((func.relative_address, foreign_func.get_foreign_function()))
    } else {
      errors.push(FunctionNameNotInSource(foreign_func.get_name().to_string()));
    }
  }
  assert!(functions.len() <= declared_foreign_functions.len());
  if functions.len() < declared_foreign_functions.len() {
    return Err(vec![ForeignBindingError::MissingForeignFunction]);
  }
  functions.sort_by_key(|(key, _)| *key);
  for (_, func) in functions {
    runtime_foreign_functions.push(func);
  }
  if errors.is_empty() {
    Ok(())
  } else {
    Err(errors)
  }
}
