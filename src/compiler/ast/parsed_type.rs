use crate::compiler::ast::TypeHandle;

pub struct ParsedFunctionType {
  signature: Vec<TypeHandle>,
}

impl ParsedFunctionType {
  pub fn new(mut parameters: Vec<TypeHandle>, return_type: TypeHandle) -> Self {
    parameters.push(return_type);
    Self {
      signature: parameters,
    }
  }

  pub fn signature(&self) -> &[TypeHandle] {
    &self.signature[0..self.signature.len() - 1]
  }

  pub fn return_type(&self) -> TypeHandle {
    self.signature[self.signature.len() - 1]
  }
}

pub enum ParsedType<'src> {
  Num,
  Str,
  Bool,
  Nothing,
  Any,
  Named(&'src str),
  Function(ParsedFunctionType),
}
