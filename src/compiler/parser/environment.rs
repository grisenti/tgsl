use std::collections::hash_map::Entry;

use crate::compiler::identifier::{FunctionId, OverloadId};
use crate::compiler::overload_set::{OverloadSet, ResolvedOverload};
use crate::compiler::{
  errors::ge_err,
  global_env::{GlobalEnv, Struct},
  identifier::{
    ExternId, GlobalIdentifier, GlobalVarId, Identifier, ModuleId, StructId, VariableIdentifier,
  },
};

use super::*;

pub enum ResolvedIdentifier {
  UnresolvedOverload(OverloadId),
  ResolvedIdentifier { id: Identifier, type_: Type },
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
  return_type: Option<Type>,
}

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

  fn get_global(&mut self, name: &str, name_sr: SourceRange) -> CompilerResult<ResolvedIdentifier> {
    if let Some(&global_variable) = self.global_names.get(name) {
      let type_ = match global_variable {
        GlobalIdentifier::Variable(var_id) => {
          if var_id.is_relative() {
            self.module_global_variables_types[var_id.get_id() as usize].clone()
          } else {
            self.global_env.get_variable_type(var_id).clone()
          }
        }
        GlobalIdentifier::ExternFunction(extern_id) => {
          if extern_id.is_relative() {
            self.extern_function_types[extern_id.get_id() as usize].clone()
          } else {
            self.global_env.get_extern_function_type(extern_id).clone()
          }
        }
        GlobalIdentifier::Struct(struct_id) => Type::Struct(struct_id),
        GlobalIdentifier::OverloadId(overload_id) => {
          return Ok(ResolvedIdentifier::UnresolvedOverload(overload_id));
        }
        GlobalIdentifier::Invalid => panic!(),
      };
      Ok(ResolvedIdentifier::ResolvedIdentifier {
        id: global_variable.into(),
        type_: type_.clone(),
      })
    } else {
      Err(ge_err::undeclared_global(name_sr))
    }
  }

  fn declare_global(
    &mut self,
    name: &str,
    name_sr: SourceRange,
    global_id: GlobalIdentifier,
  ) -> CompilerResult<()> {
    if let Entry::Vacant(e) = self.global_names.entry(name.to_string()) {
      e.insert(global_id);
      Ok(())
    } else {
      Err(ge_err::identifier_redeclaration(name_sr, name))
    }
  }

  pub fn get_id(&mut self, name: &str, name_sr: SourceRange) -> CompilerResult<ResolvedIdentifier> {
    if let Some(local) = self.find_local(name) {
      let (local_id, type_) = self.capture(local);
      Ok(ResolvedIdentifier::ResolvedIdentifier {
        id: local_id.into(),
        type_: type_.clone(),
      })
    } else {
      self.get_global(name, name_sr)
    }
  }

  pub fn get_variable_id(
    &mut self,
    name: &str,
    name_sr: SourceRange,
  ) -> CompilerResult<(VariableIdentifier, &Type)> {
    if let Some(local) = self.find_local(name) {
      Ok(self.capture(local))
    } else {
      let id = self.global_names.get(name).copied();
      if let Some(GlobalIdentifier::Variable(id)) = id {
        Ok((
          id.into(),
          &self.module_global_variables_types[id.get_id() as usize],
        ))
      } else {
        Err(ge_err::undeclared_global(name_sr))
      }
    }
  }

  pub fn get_struct_id(&mut self, name: &str, _name_sr: SourceRange) -> CompilerResult<StructId> {
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
    name_sr: SourceRange,
    signature: FunctionSignature,
  ) -> CompilerResult<FunctionId> {
    self.declare_global_function(name, name_sr, signature)
  }

  fn get_or_create_overload_id(
    &mut self,
    name: &str,
    name_sr: SourceRange,
  ) -> CompilerResult<OverloadId> {
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
    name_sr: SourceRange,
    signature: FunctionSignature,
  ) -> CompilerResult<FunctionId> {
    let overload_id = self.get_or_create_overload_id(name, name_sr)?;
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

  pub fn define_struct(
    &mut self,
    name: &str,
    name_sr: SourceRange,
    member_names: Vec<String>,
    member_types: Vec<Type>,
  ) -> CompilerResult<StructId> {
    assert_eq!(member_types.len(), member_names.len());

    let id = StructId::relative(self.module_structs.len() as u32).into_public();
    self.declare_global(name, name_sr, id.into())?;
    self.module_structs.push(Some(Struct::new(
      name.to_string(),
      member_names,
      member_types,
    )));
    Ok(id)
  }

  fn define_local_variable(
    &mut self,
    name: &'src str,
    name_sr: SourceRange,
    var_type: Type,
  ) -> CompilerResult<VariableIdentifier> {
    if self.last_local_id == u8::MAX {
      return Err(parser_err::too_many_local_names(name_sr));
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
    self.locals.push(local);
    Ok(VariableIdentifier::Local(id))
  }

  pub fn define_function_return_type(&mut self, type_: Type) {
    self
      .functions_declaration_stack
      .last_mut()
      .unwrap()
      .return_type = Some(type_);
  }

  pub fn get_function_return_type(&self) -> Option<&Type> {
    self
      .functions_declaration_stack
      .last()
      .map(|func| func.return_type.as_ref())
      .flatten()
  }

  pub fn define_variable(
    &mut self,
    name: &'src str,
    name_sr: SourceRange,
    var_type: Type,
  ) -> CompilerResult<VariableIdentifier> {
    if self.is_local_in_current_scope(name) {
      Err(parser_err::same_scope_name_redeclaration(name_sr, name))
    } else if self.in_global_scope() {
      let id = GlobalVarId::relative(self.module_global_variables_types.len() as u32).into_public();
      self.declare_global(name, name_sr, id.into())?;
      self.module_global_variables_types.push(var_type);
      Ok(VariableIdentifier::Global(id))
    } else {
      self.define_local_variable(name, name_sr, var_type)
    }
  }

  pub fn pop_scope(&mut self) -> u8 {
    debug_assert!(self.scope_depth > 0);

    let names_in_current_scope = self.names_in_current_scope;
    self
      .locals
      .truncate(self.locals.len() - names_in_current_scope as usize);
    if let Some(local) = self.locals.last() {
      self.names_in_current_scope = local.scope_local_id;
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

  pub fn push_function(&mut self) {
    self.names_in_current_scope = 0;
    self.last_local_id = 0;
    self.push_scope();
    self.functions_declaration_stack.push(Function {
      captures: Vec::new(),
      return_type: None,
    })
  }

  pub fn pop_function(&mut self) -> Vec<VariableIdentifier> {
    assert!(!self.functions_declaration_stack.is_empty());

    self.pop_scope();
    if let Some(last_local) = self.locals.last() {
      self.last_local_id = last_local.id + 1;
      self.names_in_current_scope = last_local.scope_local_id;
    } else {
      self.last_local_id = 0;
      self.names_in_current_scope = 0;
    }
    self
      .functions_declaration_stack
      .pop()
      .unwrap()
      .captures
      .iter()
      .map(|c| c.id)
      .collect()
  }

  fn ensure_name_available(&self, _name: &str, _name_sr: SourceRange) -> CompilerResult<()> {
    Ok(())
  }

  pub fn declare_extern_function(
    &mut self,
    name: &str,
    name_sr: SourceRange,
    function_signature: FunctionSignature,
  ) -> CompilerResult<ExternId> {
    let id = ExternId::relative(self.extern_function_types.len() as u32).into_public();
    self.declare_global(name, name_sr, GlobalIdentifier::ExternFunction(id))?;
    self
      .extern_function_types
      .push(Type::Function(function_signature));
    Ok(id)
  }

  pub fn import_module(&mut self, name: &str, name_sr: SourceRange) -> CompilerResult<ModuleId> {
    let module_id = self.global_env.get_module_id(name, name_sr)?;
    let module = self.global_env.get_module(module_id);
    // FIXME: does not check for double declarations
    for (name, global_id) in &module.global_names {
      match global_id {
        GlobalIdentifier::OverloadId(module_overload_id) => {
          if let Some(global_id) = self.global_names.get(name).copied() {
            if let GlobalIdentifier::OverloadId(overload_id) = global_id {
              if !self.overloads[overload_id as usize]
                .merge(&module.overloads[*module_overload_id as usize])
              {
                panic!() // cannot merge
              }
              self.global_names.insert(name.clone(), global_id);
            } else {
              panic!() // overload in one name is something else in this module
            }
          } else {
            let overload_id = self.overloads.len();
            self
              .overloads
              .push(module.overloads[*module_overload_id as usize].clone());
            self.global_names.insert(
              name.clone(),
              GlobalIdentifier::OverloadId(overload_id as u32),
            );
          }
        }
        other => {
          self.global_names.insert(name.clone(), *other);
        }
      }
    }
    Ok(module_id)
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

#[cfg(disable)]
mod test {
  use crate::compiler::{
    global_env::GlobalEnv,
    identifier::{GlobalVarId, VariableIdentifier},
    lexer::SourceRange,
  };

  use super::Environment;

  #[test]
  fn access_global_variable_from_global_scope() {
    let global_env = GlobalEnv::new();
    let mut env = Environment::new(&global_env);
    env
      .define_variable("x", SourceRange::EMPTY)
      .expect("could not define variable");
    assert_eq!(
      env.get_variable_id("x", SourceRange::EMPTY),
      Ok(VariableIdentifier::Global(
        GlobalVarId::relative(0).into_public()
      ))
    );
  }

  #[test]
  fn access_global_variable_from_local_scope() {
    let global_env = GlobalEnv::new();
    let mut env = Environment::new(&global_env);
    env
      .define_variable("x", SourceRange::EMPTY)
      .expect("counld not define global variable");
    env.push_scope();
    assert_eq!(
      env.get_variable_id("x", SourceRange::EMPTY),
      Ok(VariableIdentifier::Global(
        GlobalVarId::relative(0).into_public()
      ))
    );
  }

  #[test]
  fn capture_once() {
    let global_env = GlobalEnv::new();
    let mut env = Environment::new(&global_env);
    env.push_function();
    env
      .define_variable("x", SourceRange::EMPTY)
      .expect("could not declare name");
    env.push_function();
    assert_eq!(
      env.get_variable_id("x", SourceRange::EMPTY),
      Ok(VariableIdentifier::Capture(0))
    );
    assert_eq!(
      env.get_variable_id("x", SourceRange::EMPTY),
      Ok(VariableIdentifier::Capture(0))
    );
    assert_eq!(
      env.get_variable_id("x", SourceRange::EMPTY),
      Ok(VariableIdentifier::Capture(0))
    );
    let captures = env.pop_function();
    assert_eq!(captures, vec![VariableIdentifier::Local(0)]);
  }

  #[test]
  fn multilevel_capture() {
    let global_env = GlobalEnv::new();
    let mut env = Environment::new(&global_env);
    env.push_function();
    env
      .define_variable("x", SourceRange::EMPTY)
      .expect("could not declare name");
    env.push_function();
    env.push_function();
    assert_eq!(
      env.get_variable_id("x", SourceRange::EMPTY),
      Ok(VariableIdentifier::Capture(0))
    );
    let captures = env.pop_function();
    assert_eq!(captures, vec![VariableIdentifier::Capture(0)]);
    let captures = env.pop_function();
    assert_eq!(captures, vec![VariableIdentifier::Local(0)]);
  }

  #[test]
  fn no_capture_for_globals() {
    let global_env = GlobalEnv::new();
    let mut env = Environment::new(&global_env);
    env
      .define_variable("x", SourceRange::EMPTY)
      .expect("could not declare name");
    env.push_function();
    assert_eq!(env.pop_function(), vec![]);
  }

  #[test]
  fn no_capture_for_globals_in_inner_scope() {
    let global_env = GlobalEnv::new();
    let mut env = Environment::new(&global_env);
    env
      .define_variable("x", SourceRange::EMPTY)
      .expect("could not declare name");
    env.push_function();
    env.push_scope();
    assert_eq!(
      env.get_variable_id("x", SourceRange::EMPTY),
      Ok(VariableIdentifier::Global(
        GlobalVarId::relative(0).into_public()
      ))
    );
  }

  #[test]
  fn no_capture_same_function_different_scope() {
    let global_env = GlobalEnv::new();
    let mut env = Environment::new(&global_env);
    env.push_function();
    env
      .define_variable("x", SourceRange::EMPTY)
      .expect("could not declare name");
    env.push_scope();
    assert_eq!(
      env.get_variable_id("x", SourceRange::EMPTY),
      Ok(VariableIdentifier::Local(0))
    );
  }

  // #[test]
  // fn can_use_imported_names_in_anonymous_module() {
  //   let global_env = GlobalEnv::new();
  //   let module_id = global_env
  //     .new_module("test", SourceRange::EMPTY)
  //     .expect("could not create test module");
  //   let name_id = global_env.new_global_id();
  //   global_env.export_module(
  //     module_id,
  //     Module {
  //       public_names: HashMap::from([("x".to_string(), name_id)]),
  //     },
  //   );
  //   let mut env = Environment::new(&global_env);
  //   env
  //     .import_module("test", SourceRange::EMPTY)
  //     .expect("could not import test module");
  //   env
  //     .get_name("x", SourceRange::EMPTY)
  //     .expect("could not get id");
  // }
}
