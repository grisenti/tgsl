use std::collections::hash_map::Entry;
use std::collections::HashMap;

use crate::compiler::codegen::function_code::FunctionCode;
use crate::compiler::functions::overload_set::OverloadSet;
use crate::compiler::functions::GlobalFunctions;
use crate::compiler::structs::GlobalStructs;
use crate::compiler::types::Type;
use crate::compiler::{
  global_env::GlobalEnv,
  identifier::{GlobalIdentifier, GlobalVarId, VariableIdentifier},
};

pub mod imports;
pub mod types;

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
  pub global_structs: GlobalStructs<'src>,
  pub global_functions: GlobalFunctions,
  pub module_global_variables_types: Vec<Type>,
  pub overloads: Vec<OverloadSet>,
}

impl<'src> Environment<'src> {
  pub fn in_global_scope(&self) -> bool {
    self.scope_depth == 0
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

  fn get_global(&mut self, name: &str) -> Option<(VariableIdentifier, Type)> {
    if let Some(&global_variable) = self.global_names.get(name) {
      if let GlobalIdentifier::Variable(var_id) = global_variable {
        let type_ = if var_id.is_relative() {
          &self.module_global_variables_types[var_id.get_id() as usize]
        } else {
          self.global_env.get_variable_type(var_id)
        };
        Some((var_id.into(), type_.clone()))
      } else {
        panic!()
      }
    } else {
      None
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

  pub fn get_variable(&mut self, name: &str) -> Option<(VariableIdentifier, Type)> {
    if let Some(local) = self.find_local(name) {
      let (local_id, type_) = self.capture(local);
      Some((local_id.into(), type_.clone()))
    } else {
      self.get_global(name)
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
      global_structs: Default::default(),
      global_functions: Default::default(),
      module_global_variables_types: Vec::new(),
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
