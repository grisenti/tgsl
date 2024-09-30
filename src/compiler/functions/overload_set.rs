use crate::compiler::global_env::{Module, ModuleIndex};
use crate::compiler::ir::{ForeignFunctionIndex, NativeFunctionIndex};
use crate::compiler::types::{FunctionSignature, Type};

pub enum FindError {
  NotFound,
  MultipleDefinitions,
  MultipleDeclarations,
  RedefinedImport,
}

pub type FindResult<T> = Result<T, FindError>;

pub enum AddError {
  AlreadyDefined,
  AlreadyDeclared,
  RedefinesImport,
}

pub type AddResult<T> = Result<T, AddError>;

#[derive(Debug, Clone, Copy)]
pub enum FunctionIndex {
  UndefinedNative {
    index: NativeFunctionIndex,
  },
  Native {
    index: NativeFunctionIndex,
    module_index: ModuleIndex,
  },
  Foreign {
    index: ForeignFunctionIndex,
    module_index: ModuleIndex,
  },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum FunctionKind {
  Native,
  Foreign,
  Undefined,
  ErrorMultipleDefinitions,
  ErrorRedefinedImport,
}

#[derive(Debug, Clone)]
pub struct OverloadedFunction {
  pub signature: FunctionSignature,
  pub index: FunctionIndex,
}

#[derive(Debug, Clone)]
pub struct OverloadSet {
  functions: Vec<OverloadedFunction>,
}

impl OverloadSet {
  pub fn find(&self, arguments: &[Type]) -> FindResult<OverloadedFunction> {
    if let Some(f) = self
      .functions
      .iter()
      .find(|f| f.signature.get_parameters() == arguments)
    {
      Ok(f.clone())
    } else {
      Err(FindError::NotFound)
    }
  }

  pub fn auto_resolve(&self) -> FindResult<ResolvedOverload> {
    if self.functions.len() == 1 {
      let f = &self.functions[0];
      Ok(ResolvedOverload {
        address: FunctionAddress::from_address_and_kind(f.address, f.kind)?,
        signature: f.signature.clone(),
      })
    } else {
      Err(FindError::NotFound)
    }
  }

  pub(super) fn merge(&mut self, other: &OverloadSet) -> bool {
    todo!()
  }

  pub(super) fn export(&mut self) -> bool {
    self.functions.retain(|f| {
      f.kind != FunctionKind::ImportedNative && f.kind != FunctionKind::ImportedForeign
    });
    !self.functions.is_empty()
  }

  pub(super) fn relative_foreign_iter(&self) -> impl Iterator<Item = OverloadedFunction> + '_ {
    self
      .functions
      .iter()
      .filter(|f| f.kind == FunctionKind::RelativeForeign)
      .cloned()
  }

  pub fn add(&mut self, function: OverloadedFunction) {
    self.functions.push(function);
  }

  pub fn new(function: OverloadedFunction) -> Self {
    Self {
      functions: vec![function],
    }
  }

  pub(super) fn find_mut(&mut self, arguments: &[Type]) -> FindResult<&mut OverloadedFunction> {
    if let Some(f) = self
      .functions
      .iter_mut()
      .find(|f| f.signature.get_parameters() == arguments)
    {
      Ok(f)
    } else {
      Err(FindError::NotFound)
    }
  }
}
