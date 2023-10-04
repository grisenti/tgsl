use crate::compiler::functions::{FunctionInsertError, FunctionInsertResult};
use crate::compiler::semantics::environment::Environment;
use crate::compiler::types::FunctionSignature;

impl Environment<'_> {
  fn check_fn_name_availability(&mut self, name: &str) -> Result<(), FunctionInsertError> {
    if self.is_type(name) {
      Err(FunctionInsertError::NameAlreadyAType)
    } else if self.global_variables.is_global_variable(name) {
      Err(FunctionInsertError::NameAlreadyAVariable)
    } else {
      Ok(())
    }
  }

  pub fn declare_native_function(
    &mut self,
    name: &str,
    signature: FunctionSignature,
  ) -> FunctionInsertResult {
    self.check_fn_name_availability(name)?;
    self.global_functions.declare_native(name, signature)
  }

  pub fn declare_extern_function(
    &mut self,
    name: &str,
    signature: FunctionSignature,
  ) -> FunctionInsertResult {
    self.check_fn_name_availability(name)?;
    self.global_functions.declare_extern(name, signature)
  }

  pub fn define_native_function(
    &mut self,
    name: &str,
    signature: FunctionSignature,
  ) -> FunctionInsertResult {
    self.check_fn_name_availability(name)?;
    self.global_functions.define_native(name, signature)
  }
}
