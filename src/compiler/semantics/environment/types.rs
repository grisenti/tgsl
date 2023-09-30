use crate::compiler::semantics::environment::{DeclarationError, DeclarationResult, Environment};
use crate::compiler::structs::Struct;
use crate::compiler::types::Type;

#[derive(Copy, Clone, Debug)]
pub enum StructGetError {
  NotDefined,
  NotAStruct,
}

pub type StructGetResult<'s> = Result<&'s Struct, StructGetError>;

impl<'a> Environment<'a> {
  pub fn get_struct(&self, name: &str) -> StructGetResult {
    let struct_ = self
      .global_structs
      .get(name)
      .ok_or(StructGetError::NotAStruct)?;
    if struct_.is_defined() {
      Ok(struct_)
    } else {
      Err(StructGetError::NotDefined)
    }
  }

  pub fn is_struct(&self, name: &str) -> bool {
    self.global_structs.get(name).is_some()
  }

  pub fn declare_struct(&mut self, name: &str) -> DeclarationResult<()> {
    if !self.global_structs.declare(name) {
      Err(DeclarationError::AlreadyDefined)
    } else {
      Ok(())
    }
  }

  pub fn define_struct(
    &mut self,
    name: &str,
    member_names: Vec<String>,
    member_types: Vec<Type>,
  ) -> DeclarationResult<()> {
    if !self.global_structs.define(name, member_names, member_types) {
      Err(DeclarationError::AlreadyDefined)
    } else {
      Ok(())
    }
  }
}
