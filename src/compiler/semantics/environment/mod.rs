pub mod imports;

use std::collections::hash_map::Entry;
use std::collections::HashMap;

use crate::compiler::codegen::function_code::FunctionCode;

use crate::compiler::identifier::{FunctionId, OverloadId};
use crate::compiler::overload_set::{OverloadSet, ResolvedOverload};
use crate::compiler::types::{FunctionSignature, Type};
use crate::compiler::{
  global_env::{GlobalEnv, Struct},
  identifier::{ExternId, GlobalIdentifier, GlobalVarId, StructId, VariableIdentifier},
};

#[derive(Debug, Eq, PartialEq)]
pub enum ResolvedIdentifier<'a> {
  UnresolvedOverload(OverloadId),
  ResolvedFunction {
    id: FunctionId,
    signature: &'a FunctionSignature,
  },
  ResolvedVariable {
    id: VariableIdentifier,
    type_: &'a Type,
  },
  Struct(StructId),
  ExternFunction {
    id: ExternId,
    type_: &'a Type, // TODO: maybe make this FunctionSignature
  },
  Error,
}

#[derive(Debug, Clone)]
struct Local<'src> {
  name: &'src str,
  id: u8,
  // local id used to refer to any local variable reachable from this scope
  scope_local_id: u8,
  // resets to 0 for any new scope
  function_depth: u8,
  type_: Type,
}

#[derive(Debug, Clone, Copy)]
struct CaptureId {
  id: VariableIdentifier,
  local_id: u8,
}

struct Function {
  captures: Vec<CaptureId>,
  return_type: Type,
  code: FunctionCode,
}

pub struct FinalizedFunction {
  pub captures: Vec<VariableIdentifier>,
  pub code: FunctionCode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeclarationError {
  AlreadyDefined,
  TooManyLocalNames,
}

pub type DeclarationResult<T> = Result<T, DeclarationError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NameError {
  UndeclaredName,
}

pub type NameResult<T> = Result<T, NameError>;

pub struct Environment<'src> {
  global_env: &'src GlobalEnv,

  locals: Vec<Local<'src>>,
  scope_depth: u8,
  last_local_id: u8,
  names_in_current_scope: u8,
  functions_declaration_stack: Vec<Function>,
  declared_functions: u32,

  pub global_names: HashMap<String, GlobalIdentifier>,
  pub module_global_variables_types: Vec<Type>,
  pub extern_function_types: Vec<Type>,
  pub module_structs: Vec<Option<Struct>>,
  pub overloads: Vec<OverloadSet>,
}

impl<'src> Environment<'src> {
  pub fn in_global_scope(&self) -> bool {
    self.scope_depth == 0
  }

  pub fn new_function_id(&mut self) -> FunctionId {
    let id = self.declared_functions;
    self.declared_functions += 1;
    FunctionId::relative(id).into_public()
  }

  fn find_local(&self, local_name: &str) -> Option<usize> {
    self
      .locals
      .iter()
      .rposition(|Local { name, .. }| *name == local_name)
  }

  fn is_local_in_current_scope(&self, local_name: &str) -> bool {
    self
      .locals
      .iter()
      .rev()
      .take(self.names_in_current_scope as usize)
      .any(|local| local.name == local_name)
  }

  fn capture(&mut self, local_index: usize) -> (VariableIdentifier, &Type) {
    let local = &self.locals[local_index];
    let start_depth = self.functions_declaration_stack.len() as u8;
    let target_depth = local.function_depth;

    assert!(start_depth >= target_depth);

    let mut capture_id = VariableIdentifier::Local(local.id);
    let functions = self
      .functions_declaration_stack
      .iter_mut()
      .rev()
      .take((start_depth - target_depth) as usize)
      .rev();
    for func in functions {
      // TODO: very slow, think of something different
      if let Some(capture_index) = func.captures.iter().position(|c| c.local_id == local.id) {
        capture_id = VariableIdentifier::Capture(capture_index as u8);
      } else {
        let tmp = capture_id;
        capture_id = VariableIdentifier::Capture(func.captures.len() as u8);
        func.captures.push(CaptureId {
          id: tmp,
          local_id: local.id,
        });
      }
    }
    (capture_id, &local.type_)
  }

  fn get_global(&mut self, name: &str) -> NameResult<ResolvedIdentifier> {
    if let Some(&global_variable) = self.global_names.get(name) {
      match global_variable {
        GlobalIdentifier::Variable(var_id) => {
          let type_ = if var_id.is_relative() {
            &self.module_global_variables_types[var_id.get_id() as usize]
          } else {
            self.global_env.get_variable_type(var_id)
          };
          Ok(ResolvedIdentifier::ResolvedVariable {
            id: var_id.into(),
            type_,
          })
        }
        GlobalIdentifier::ExternFunction(extern_id) => {
          let type_ = if extern_id.is_relative() {
            &self.extern_function_types[extern_id.get_id() as usize]
          } else {
            self.global_env.get_extern_function_type(extern_id)
          };
          Ok(ResolvedIdentifier::ExternFunction {
            id: extern_id,
            type_,
          })
        }
        GlobalIdentifier::Struct(struct_id) => Ok(ResolvedIdentifier::Struct(struct_id)),
        GlobalIdentifier::OverloadId(overload_id) => {
          if let Some((signature, id)) = self.overloads[overload_id as usize].auto_resolve() {
            Ok(ResolvedIdentifier::ResolvedFunction { signature, id: *id })
          } else {
            Ok(ResolvedIdentifier::UnresolvedOverload(overload_id))
          }
        }
        GlobalIdentifier::Invalid => panic!(),
      }
    } else {
      Err(NameError::UndeclaredName)
    }
  }

  fn declare_global(&mut self, name: &str, global_id: GlobalIdentifier) -> DeclarationResult<()> {
    if let Entry::Vacant(e) = self.global_names.entry(name.to_string()) {
      e.insert(global_id);
      Ok(())
    } else {
      Err(DeclarationError::AlreadyDefined)
    }
  }

  pub fn get_id(&mut self, name: &str) -> NameResult<ResolvedIdentifier> {
    if let Some(local) = self.find_local(name) {
      let (local_id, type_) = self.capture(local);
      Ok(ResolvedIdentifier::ResolvedVariable {
        id: local_id.into(),
        type_,
      })
    } else {
      self.get_global(name)
    }
  }

  pub fn get_struct_id(&mut self, name: &str) -> NameResult<StructId> {
    let id = self.global_names.get(name).copied();
    if let Some(GlobalIdentifier::Struct(id)) = id {
      Ok(id)
    } else {
      panic!();
    }
  }

  pub fn define_global_function(
    &mut self,
    name: &str,
    signature: FunctionSignature,
  ) -> DeclarationResult<FunctionId> {
    self.declare_global_function(name, signature)
  }

  fn get_or_create_overload_id(&mut self, name: &str) -> DeclarationResult<OverloadId> {
    if let Some(id) = self.global_names.get(name) {
      if let GlobalIdentifier::OverloadId(overload_id) = id {
        Ok(*overload_id)
      } else {
        panic!()
      }
    } else {
      let overload_id = self.overloads.len() as u32;
      self.overloads.push(OverloadSet::default());
      self
        .global_names
        .insert(name.to_string(), GlobalIdentifier::OverloadId(overload_id));
      Ok(overload_id as OverloadId)
    }
  }

  pub fn declare_global_function(
    &mut self,
    name: &str,
    signature: FunctionSignature,
  ) -> DeclarationResult<FunctionId> {
    let overload_id = self.get_or_create_overload_id(name)?;
    if let Some(overload) = &self.overloads[overload_id as usize].find(signature.get_parameters()) {
      if overload.function_signature.get_return_type() != signature.get_return_type() {
        panic!()
      } else {
        Ok(overload.function_id)
      }
    } else {
      let function_id = self.new_function_id();
      self.overloads[overload_id as usize].insert(signature, function_id);
      Ok(function_id)
    }
  }

  pub fn resolve_overload(
    &self,
    overload_id: OverloadId,
    parameters: &[Type],
  ) -> Option<ResolvedOverload> {
    self.overloads[overload_id as usize].find(parameters)
  }

  pub fn get_struct(&self, id: StructId) -> Option<&Struct> {
    if id.is_relative() {
      self.module_structs[id.get_id() as usize].as_ref()
    } else {
      Some(self.global_env.get_struct(id))
    }
  }

  pub fn declare_struct(&mut self, name: &str) -> DeclarationResult<StructId> {
    let id = StructId::relative(self.module_structs.len() as u32).into_public();
    self.declare_global(name, id.into())?;
    self.module_structs.push(None);
    Ok(id)
  }

  pub fn define_struct(
    &mut self,
    name: &str,
    member_names: Vec<String>,
    member_types: Vec<Type>,
  ) -> DeclarationResult<StructId> {
    assert_eq!(member_types.len(), member_names.len());

    if let Some(GlobalIdentifier::Struct(struct_id)) = self.global_names.get(name) {
      if !struct_id.is_relative() {
        return Err(DeclarationError::AlreadyDefined);
      }
      let struct_ = &mut self.module_structs[struct_id.get_id() as usize];
      if struct_.is_some() {
        Err(DeclarationError::AlreadyDefined)
      } else {
        *struct_ = Some(Struct::new(name.to_string(), member_names, member_types));
        Ok(*struct_id)
      }
    } else {
      let id = StructId::relative(self.module_structs.len() as u32).into_public();
      self.declare_global(name, id.into())?;
      self.module_structs.push(Some(Struct::new(
        name.to_string(),
        member_names,
        member_types,
      )));
      Ok(id)
    }
  }

  fn define_local_variable(
    &mut self,
    name: &'src str,
    var_type: Type,
  ) -> DeclarationResult<VariableIdentifier> {
    if self.last_local_id == u8::MAX {
      return Err(DeclarationError::TooManyLocalNames);
    }
    let id = self.last_local_id;
    let local = Local {
      name,
      id,
      scope_local_id: self.names_in_current_scope,
      function_depth: self.functions_declaration_stack.len() as u8,
      type_: var_type,
    };
    self.last_local_id += 1;
    self.names_in_current_scope += 1;
    self.locals.push(local);
    Ok(VariableIdentifier::Local(id))
  }

  pub fn get_current_function_return_type(&self) -> Option<&Type> {
    self
      .functions_declaration_stack
      .last()
      .map(|func| &func.return_type)
  }

  pub fn get_current_function_code(&mut self) -> Option<&mut FunctionCode> {
    self
      .functions_declaration_stack
      .last_mut()
      .map(|func| &mut func.code)
  }

  pub fn define_variable(
    &mut self,
    name: &'src str,
    var_type: Type,
  ) -> DeclarationResult<VariableIdentifier> {
    if self.is_local_in_current_scope(name) {
      Err(DeclarationError::AlreadyDefined)
    } else if self.in_global_scope() {
      let id = GlobalVarId::relative(self.module_global_variables_types.len() as u32).into_public();
      self.declare_global(name, id.into())?;
      self.module_global_variables_types.push(var_type);
      Ok(VariableIdentifier::Global(id))
    } else {
      self.define_local_variable(name, var_type)
    }
  }

  pub fn pop_scope(&mut self) -> u8 {
    debug_assert!(self.scope_depth > 0);

    let names_in_current_scope = self.names_in_current_scope;
    self
      .locals
      .truncate(self.locals.len() - names_in_current_scope as usize);
    if let Some(local) = self.locals.last() {
      self.names_in_current_scope = local.scope_local_id + 1;
    } else {
      self.names_in_current_scope = 0;
    }
    self.scope_depth -= 1;
    self.last_local_id -= names_in_current_scope;
    names_in_current_scope
  }

  pub fn push_scope(&mut self) {
    self.scope_depth += 1;
    self.names_in_current_scope = 0;
  }

  pub fn push_function(&mut self, name: String, return_type: Type) {
    self.names_in_current_scope = 0;
    self.last_local_id = 0;
    self.push_scope();
    self.functions_declaration_stack.push(Function {
      captures: Vec::new(),
      return_type,
      code: FunctionCode::new(name),
    })
  }

  pub fn pop_function(&mut self) -> FinalizedFunction {
    assert!(!self.functions_declaration_stack.is_empty());

    self.pop_scope();
    if let Some(last_local) = self.locals.last() {
      self.last_local_id = last_local.id + 1;
      self.names_in_current_scope = last_local.scope_local_id;
    } else {
      self.last_local_id = 0;
      self.names_in_current_scope = 0;
    }
    let function = self
      .functions_declaration_stack
      .pop()
      .expect("no functions to pop");
    let captures = function.captures.iter().map(|c| c.id).collect();
    FinalizedFunction {
      captures,
      code: function.code,
    }
  }

  pub fn ensure_module_name_available(&self, name: &str) -> bool {
    self.global_env.is_module_name_available(name)
  }

  pub fn declare_extern_function(
    &mut self,
    name: &str,
    function_signature: FunctionSignature,
  ) -> DeclarationResult<ExternId> {
    let id = ExternId::relative(self.extern_function_types.len() as u32).into_public();
    self.declare_global(name, GlobalIdentifier::ExternFunction(id))?;
    self
      .extern_function_types
      .push(Type::Function(function_signature));
    Ok(id)
  }

  pub fn new(global_env: &'src GlobalEnv) -> Self {
    Self {
      global_env,

      locals: Vec::new(),
      scope_depth: 0,
      last_local_id: 0,
      names_in_current_scope: 0,
      functions_declaration_stack: Vec::new(),
      declared_functions: 0,

      global_names: HashMap::new(),
      module_global_variables_types: Vec::new(),
      extern_function_types: Vec::new(),
      module_structs: Vec::new(),
      overloads: Vec::new(),
    }
  }
}

#[cfg(test)]
mod test {
  use crate::compiler::identifier::FunctionId;

  use crate::compiler::semantics::environment::ResolvedIdentifier::{
    ResolvedFunction, ResolvedVariable,
  };
  use crate::compiler::types::{FunctionSignature, Type};
  use crate::compiler::{
    global_env::GlobalEnv,
    identifier::{GlobalVarId, VariableIdentifier},
  };

  use super::Environment;

  #[test]
  fn access_global_variable_from_global_scope() {
    let global_env = GlobalEnv::new();
    let mut env = Environment::new(&global_env);
    let var_id = env
      .define_variable("x", Type::Any)
      .expect("could not define variable");
    assert!(matches!(
      VariableIdentifier::Global(GlobalVarId::relative(0)),
      _var_id
    ));
    assert_eq!(
      env.get_id("x"),
      Ok(ResolvedVariable {
        id: var_id,
        type_: &Type::Any
      })
    );
  }

  #[test]
  fn access_global_variable_from_local_scope() {
    let global_env = GlobalEnv::new();
    let mut env = Environment::new(&global_env);
    let var_id = env
      .define_variable("x", Type::Str)
      .expect("counld not define global variable");
    env.push_scope();
    assert_eq!(
      env.get_id("x"),
      Ok(ResolvedVariable {
        id: var_id.into(),
        type_: &Type::Str
      })
    );
  }

  #[test]
  fn capture_once() {
    let global_env = GlobalEnv::new();
    let mut env = Environment::new(&global_env);
    env.push_function("outer".to_string(), Type::Nothing);
    let var_id = env
      .define_variable("x", Type::Bool)
      .expect("could not declare name");
    env.push_function("inner".to_string(), Type::Nothing);
    assert_eq!(
      env.get_id("x"),
      Ok(ResolvedVariable {
        id: VariableIdentifier::Capture(0).into(),
        type_: &Type::Bool
      })
    );
    assert_eq!(
      env.get_id("x"),
      Ok(ResolvedVariable {
        id: VariableIdentifier::Capture(0).into(),
        type_: &Type::Bool
      })
    );
    assert_eq!(
      env.get_id("x"),
      Ok(ResolvedVariable {
        id: VariableIdentifier::Capture(0).into(),
        type_: &Type::Bool
      })
    );
    let function = env.pop_function();
    assert_eq!(function.captures, vec![var_id]);
  }

  #[test]
  fn multilevel_capture() {
    let global_env = GlobalEnv::new();
    let mut env = Environment::new(&global_env);
    env.push_function("1".to_string(), Type::Nothing);
    env
      .define_variable("x", Type::Num)
      .expect("could not declare name");
    env.push_function("2".to_string(), Type::Nothing);
    env.push_function("3".to_string(), Type::Nothing);
    assert_eq!(
      env.get_id("x"),
      Ok(ResolvedVariable {
        id: VariableIdentifier::Capture(0).into(),
        type_: &Type::Num
      })
    );
    let captures = env.pop_function().captures;
    assert_eq!(captures, vec![VariableIdentifier::Capture(0)]);
    let captures = env.pop_function().captures;
    assert_eq!(captures, vec![VariableIdentifier::Local(0)]);
  }

  #[test]
  fn no_capture_for_globals() {
    let global_env = GlobalEnv::new();
    let mut env = Environment::new(&global_env);
    env
      .define_variable("x", Type::Bool)
      .expect("could not declare name");
    env.push_function("func".to_string(), Type::Nothing);
    env.get_id("x").expect("could not get variable");
    assert_eq!(env.pop_function().captures, vec![]);
  }

  #[test]
  fn no_capture_for_globals_in_inner_scope() {
    let global_env = GlobalEnv::new();
    let mut env = Environment::new(&global_env);
    env
      .define_variable("x", Type::Str)
      .expect("could not declare name");
    env.push_function("h".to_string(), Type::Bool);
    env.push_scope();
    assert_eq!(
      env.get_id("x"),
      Ok(ResolvedVariable {
        id: GlobalVarId::relative(0).into_public().into(),
        type_: &Type::Str
      })
    );
  }

  #[test]
  fn no_capture_same_function_different_scope() {
    let global_env = GlobalEnv::new();
    let mut env = Environment::new(&global_env);
    env.push_function("f".to_string(), Type::Nothing);
    env
      .define_variable("x", Type::Str)
      .expect("could not declare name");
    env.push_scope();
    assert_eq!(
      env.get_id("x"),
      Ok(ResolvedVariable {
        id: VariableIdentifier::Local(0).into(),
        type_: &Type::Str
      })
    );
  }

  #[test]
  fn overload_set_with_one_element_returns_the_element() {
    let global_env = GlobalEnv::new();
    let mut env = Environment::new(&global_env);
    let signature = FunctionSignature::new(vec![Type::Str], Type::Num);
    env
      .define_global_function("get_object", signature.clone())
      .expect("could not define function");
    assert_eq!(
      env.get_id("get_object"),
      Ok(ResolvedFunction {
        id: FunctionId::relative(0).into_public().into(),
        signature: &signature
      })
    );
  }
}
