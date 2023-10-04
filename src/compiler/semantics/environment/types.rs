use crate::compiler::semantics::environment::Environment;
use crate::compiler::structs::{StructInsertError, StructInsertResult};
use crate::compiler::types::Type;

impl<'a> Environment<'a> {
  fn check_var_name_availability(&mut self, name: &str) -> Result<(), StructInsertError> {
    if self.global_variables.is_global_variable(name) {
      Err(StructInsertError::NameIsAlreadyAVariable)
    } else if self.global_functions.is_overload_set(name) {
      Err(StructInsertError::NameIsAlreadyAFunction)
    } else {
      Ok(())
    }
  }

  pub fn declare_struct(&mut self, name: &str) -> StructInsertResult {
    self.check_var_name_availability(name)?;
    self.global_structs.declare(name)
  }

  pub fn define_struct(
    &mut self,
    name: &str,
    member_names: Vec<String>,
    member_types: Vec<Type>,
  ) -> StructInsertResult {
    self.check_var_name_availability(name)?;
    self.global_structs.define(name, member_names, member_types)
  }

  pub fn is_type(&self, name: &str) -> bool {
    self.global_structs.is_struct(name)
  }
}
