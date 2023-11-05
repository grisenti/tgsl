use std::any::{Any, TypeId};

use chunk::*;

use crate::compiler::codegen::ModuleCode;
use crate::errors::RuntimeError;
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

pub struct ForeignCallable {
  function: Box<dyn Fn(&[TaggedValue], Gc, &mut dyn Any) -> TaggedValue>,
  context_type_id: TypeId,
}

impl ForeignCallable {
  pub(crate) fn new<C: 'static>(
    function: Box<dyn Fn(&[TaggedValue], Gc, &mut dyn Any) -> TaggedValue>,
  ) -> Self {
    Self {
      function,
      context_type_id: TypeId::of::<C>(),
    }
  }
  fn call(&self, arguments: &[TaggedValue], context: &mut dyn Any, gc: Gc) -> TaggedValue {
    debug_assert_eq!((context as &dyn Any).type_id(), self.context_type_id);
    (self.function)(arguments, gc, context)
  }
}

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
    module_code: ModuleCode,
    foreign_functions: impl Iterator<Item = ForeignCallable>,
    context: &mut dyn Any,
  ) -> Result<(), RuntimeError> {
    let globals_count = module_code.global_variables_count;
    let foreign_functions_count = module_code.foreign_functions_count;
    let functions_count = module_code.functions.len();
    let global_chunk = GlobalChunk::new(module_code, &self.address_table);
    self.functions.extend(global_chunk.functions);
    self.foreign_functions.extend(foreign_functions);
    self.globals.resize(
      self.globals.len() + globals_count as usize,
      TaggedValue::none(),
    );
    self.address_table.update_table(
      globals_count,
      foreign_functions_count,
      functions_count as u32,
    );
    self.interpreter.interpret(
      &global_chunk.global_code,
      &mut self.globals,
      &self.functions,
      &self.foreign_functions,
      context,
    )
  }

  pub fn add_library_context(&mut self, context: Box<dyn Any>) {
    self.interpreter.library_contexts.push(context);
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
