use crate::compiler::identifier::FunctionId;
use crate::compiler::types::{FunctionSignature, Type};

#[derive(Default, Debug, Clone)]
pub struct OverloadSet {
  functions: Vec<(FunctionSignature, FunctionId)>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct ResolvedOverload<'a> {
  pub function_signature: &'a FunctionSignature,
  pub function_id: FunctionId,
}

impl OverloadSet {
  pub fn find(&self, arguments: &[Type]) -> Option<ResolvedOverload> {
    self
      .functions
      .iter()
      .find(|(signature, _)| signature.get_parameters() == arguments)
      .map(|(signature, id)| ResolvedOverload {
        function_signature: &signature,
        function_id: *id,
      })
  }

  pub fn insert(&mut self, signature: FunctionSignature, function_id: FunctionId) {
    debug_assert!(!self.find(signature.get_parameters()).is_some());

    self.functions.push((signature, function_id));
  }

  pub fn merge(&mut self, other: &Self) -> bool {
    for (signature, function_id) in &other.functions {
      if self.find(signature.get_parameters()).is_some() {
        return false;
      } else {
        self.functions.push((signature.clone(), *function_id));
      }
    }
    true
  }

  pub fn auto_resolve(&self) -> Option<&(FunctionSignature, FunctionId)> {
    if self.functions.len() == 1 {
      Some(&self.functions[0])
    } else {
      None
    }
  }

  pub fn export_set(self, function_id_start: u32) -> (Self, usize) {
    let mut exported = Vec::new();
    for (signature, func_id) in self.functions {
      assert!(func_id.is_relative());
      if func_id.is_public() {
        exported.push((
          signature,
          FunctionId::absolute(func_id.get_id() + function_id_start),
        ));
      }
    }
    let count = exported.len();
    (
      Self {
        functions: exported,
      },
      count,
    )
  }
}

#[cfg(test)]
mod test {
  use crate::compiler::identifier::FunctionId;
  use crate::compiler::overload_set::{OverloadSet, ResolvedOverload};
  use crate::compiler::types::{FunctionSignature, Type};

  #[test]
  fn insert_overload_into_empty_set() {
    let mut overload_set = OverloadSet::default();
    overload_set.insert(
      FunctionSignature::new(vec![], Type::Nothing),
      FunctionId::relative(0),
    );
  }

  #[test]
  fn find_empty_function_in_siglet_set() {
    let mut overload_set = OverloadSet::default();
    overload_set.insert(
      FunctionSignature::new(vec![], Type::Nothing),
      FunctionId::relative(0),
    );
    assert_eq!(
      overload_set.find(&vec![]),
      Some(ResolvedOverload {
        function_id: FunctionId::relative(0),
        function_signature: &FunctionSignature::new(vec![], Type::Nothing)
      })
    );
  }

  #[test]
  fn find_non_empty_function_in_siglet_set() {
    let mut overload_set = OverloadSet::default();
    overload_set.insert(
      FunctionSignature::new(vec![Type::Bool, Type::Num], Type::Str),
      FunctionId::relative(0),
    );
    assert_eq!(
      overload_set.find(&vec![Type::Bool, Type::Num]),
      Some(ResolvedOverload {
        function_id: FunctionId::relative(0),
        function_signature: &FunctionSignature::new(vec![Type::Bool, Type::Num], Type::Str)
      })
    );
  }

  #[test]
  fn find_function_in_set() {
    let mut overload_set = OverloadSet::default();
    overload_set.insert(
      FunctionSignature::new(vec![Type::Num, Type::Num], Type::Str),
      FunctionId::relative(0),
    );
    overload_set.insert(
      FunctionSignature::new(vec![Type::Str, Type::Bool], Type::Bool),
      FunctionId::relative(1),
    );
    overload_set.insert(
      FunctionSignature::new(vec![Type::Bool, Type::Num], Type::Nothing),
      FunctionId::relative(2),
    );
    assert_eq!(
      overload_set.find(&vec![Type::Str, Type::Bool]),
      Some(ResolvedOverload {
        function_id: FunctionId::relative(1),
        function_signature: &FunctionSignature::new(vec![Type::Str, Type::Bool], Type::Bool)
      })
    );
  }
}
