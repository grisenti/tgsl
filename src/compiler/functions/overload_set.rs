use crate::compiler::types::{FunctionSignature, Type};

pub enum FindError {
  NotFound,
  MultipleDefinitions,
  MultipleDeclarations,
  RedefinedImport,
}

#[derive(Debug, Clone, Copy)]
pub enum FunctionAddress {
  RelativeNative(u32),
  RelativeForeign(u32),
  AbsoluteNative(u32),
  AbsoluteForeign(u32),
}

pub type FindResult<T> = Result<T, FindError>;

impl FunctionAddress {
  fn from_address_and_kind(address: u32, kind: FunctionKind) -> FindResult<FunctionAddress> {
    match kind {
      FunctionKind::ImportedNative => Ok(FunctionAddress::AbsoluteNative(address)),
      FunctionKind::ImportedForeign => Ok(FunctionAddress::AbsoluteForeign(address)),
      FunctionKind::RelativeNative | FunctionKind::Undefined => {
        Ok(FunctionAddress::RelativeNative(address))
      }
      FunctionKind::RelativeForeign => Ok(FunctionAddress::RelativeForeign(address)),
      FunctionKind::ErrorMultipleDefinitions => Err(FindError::MultipleDefinitions),
      FunctionKind::ErrorRedefinedImport => Err(FindError::RedefinedImport),
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum FunctionKind {
  ImportedNative,
  ImportedForeign,
  RelativeNative,
  RelativeForeign,
  Undefined, // implicitly native and relative
  ErrorMultipleDefinitions,
  ErrorRedefinedImport,
}

#[derive(Debug, Clone)]
pub(super) struct Function {
  pub(super) signature: FunctionSignature,
  pub(super) address: u32,
  pub(super) kind: FunctionKind,
}

#[derive(Debug, Clone)]
pub struct OverloadSet {
  functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct ExportedOverloadSet {
  functions: Vec<Function>,
}

impl ExportedOverloadSet {
  pub fn link(mut self, last_function_address: u32) -> LinkedOverloadSet {
    for f in &mut self.functions {
      assert!(f.kind == FunctionKind::ImportedNative || f.kind == FunctionKind::ImportedForeign);
      f.address += last_function_address;
    }
    LinkedOverloadSet {
      functions: self.functions,
    }
  }
}

#[derive(Debug, Clone)]
pub struct LinkedOverloadSet {
  functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct ResolvedOverload {
  pub function_signature: FunctionSignature,
  pub function_address: FunctionAddress,
}

impl OverloadSet {
  pub fn find(&self, arguments: &[Type]) -> FindResult<ResolvedOverload> {
    if let Some(f) = self
      .functions
      .iter()
      .find(|f| f.signature.get_parameters() == arguments)
    {
      Ok(ResolvedOverload {
        function_signature: f.signature.clone(),
        function_address: FunctionAddress::from_address_and_kind(f.address, f.kind)?,
      })
    } else {
      Err(FindError::NotFound)
    }
  }

  pub fn auto_resolve(&self) -> FindResult<ResolvedOverload> {
    if self.functions.len() == 1 {
      let f = &self.functions[0];
      Ok(ResolvedOverload {
        function_address: FunctionAddress::from_address_and_kind(f.address, f.kind)?,
        function_signature: f.signature.clone(),
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

  pub(super) fn relative_foreign_iter(&self) -> impl Iterator<Item = Function> + '_ {
    self
      .functions
      .iter()
      .filter(|f| f.kind == FunctionKind::RelativeForeign)
      .cloned()
  }

  pub(super) fn link(
    &mut self,
    last_native_function_address: u32,
    last_foreign_function_address: u32,
  ) {
    for f in &mut self.functions {
      match f.kind {
        FunctionKind::RelativeNative => {
          f.address += last_native_function_address;
          f.kind = FunctionKind::ImportedNative
        }
        FunctionKind::RelativeForeign => {
          f.address += last_foreign_function_address;
          f.kind = FunctionKind::ImportedForeign
        }
        other => panic!("cannot link function of type {other:?}"),
      }
    }
  }

  pub(super) fn insert(&mut self, function: Function) {
    debug_assert!(!self.find(function.signature.get_parameters()).is_ok());

    self.functions.push(function);
  }

  pub(super) fn new(function: Function) -> Self {
    Self {
      functions: vec![function],
    }
  }

  pub(super) fn find_mut(&mut self, arguments: &[Type]) -> FindResult<&mut Function> {
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
