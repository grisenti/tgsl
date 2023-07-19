use std::{collections::hash_map::Entry, hash::Hash};

use crate::compiler::{
  errors::ge_err,
  global_env::GlobalEnv,
  identifier::{ExternId, GlobalId, Identifier, ModuleId},
};

use super::*;

#[derive(Debug, Clone, Copy)]
struct Local<'src> {
  name: &'src str,
  id: u8,             // local id used to refer to any local variable reachable from this scope
  scope_local_id: u8, // resets to 0 for any new scope
  function_depth: u8,
}

#[derive(Debug, Clone, Copy)]
struct Global {
  id: GlobalId,
  private: bool,
  declaration: bool,
}

#[derive(Debug, Clone, Copy)]
struct CaptureId {
  id: Identifier,
  local_id: u8,
}

struct Function {
  captures: Vec<CaptureId>,
}

pub struct Environment<'src> {
  module_id: Option<ModuleId>,
  global_env: &'src mut GlobalEnv,
  globals: HashMap<String, Global>,

  locals: Vec<Local<'src>>,
  scope_depth: u8,
  last_local_id: u8,
  names_in_current_scope: u8,
  functions_declaration_stack: Vec<Function>,

  pub extern_ids: Vec<GlobalId>,
  extern_id_start: u16,
}

impl<'src> Environment<'src> {
  pub fn in_global_scope(&self) -> bool {
    self.scope_depth == 0
  }

  fn find_local(&self, local_name: &str) -> Option<Local<'src>> {
    self
      .locals
      .iter()
      .rev()
      .find(|Local { name, .. }| *name == local_name)
      .copied()
  }

  fn is_local_in_current_scope(&self, local_name: &str) -> bool {
    self
      .locals
      .iter()
      .rev()
      .take(self.names_in_current_scope as usize)
      .find(|local| local.name == local_name)
      .is_some()
  }

  fn capture(&mut self, local: Local) -> Identifier {
    let start_depth = self.functions_declaration_stack.len() as u8;
    let target_depth = local.function_depth;

    debug_assert!(start_depth >= target_depth);

    let mut capture_id = Identifier::Local(local.id);
    let functions = self
      .functions_declaration_stack
      .iter_mut()
      .rev()
      .take((start_depth - target_depth) as usize)
      .rev();
    for func in functions {
      // TODO: very slow, think of something different
      if let Some(capture_index) = func.captures.iter().position(|c| c.local_id == local.id) {
        capture_id = Identifier::Capture(capture_index as u8);
      } else {
        let tmp = capture_id;
        capture_id = Identifier::Capture(func.captures.len() as u8);
        func.captures.push(CaptureId {
          id: tmp,
          local_id: local.id,
        });
      }
    }
    capture_id
  }

  fn get_global(&mut self, name: &str, name_sr: SourceRange) -> CompilerResult<GlobalId> {
    if let Some(global) = self.globals.get(name) {
      Ok(global.id)
    } else {
      Err(ge_err::undeclared_global(name_sr))
    }
  }

  fn declare_global(
    &mut self,
    name: &str,
    name_sr: SourceRange,
    declaration: bool,
  ) -> CompilerResult<GlobalId> {
    match self.globals.entry(name.to_string()) {
      Entry::Vacant(e) => {
        let id = self.global_env.new_global_id();
        e.insert(Global {
          id,
          private: true,
          declaration: true,
        });
        Ok(id)
      }
      Entry::Occupied(_) => Err(ge_err::identifier_redeclaration(name_sr, name)),
    }
  }

  pub fn get_name(&mut self, name: &str, name_sr: SourceRange) -> CompilerResult<Identifier> {
    if let Some(local) = self.find_local(name) {
      Ok(self.capture(local))
    } else {
      self.get_global(name, name_sr).map(Identifier::Global)
    }
  }

  pub fn define_global_function(
    &mut self,
    name: &str,
    name_sr: SourceRange,
  ) -> CompilerResult<Identifier> {
    match self.globals.entry(name.to_string()) {
      Entry::Vacant(e) => {
        let id = self.global_env.new_global_id();
        e.insert(Global {
          id,
          private: true,
          declaration: false,
        });
        Ok(Identifier::Global(id))
      }
      Entry::Occupied(mut e) => {
        let global = e.get_mut();
        if global.declaration {
          global.declaration = false;
          Ok(Identifier::Global(global.id))
        } else {
          panic!();
        }
      }
    }
  }

  pub fn declare_global_function(
    &mut self,
    name: &str,
    name_sr: SourceRange,
  ) -> CompilerResult<Identifier> {
    let id = self.global_env.new_global_id();
    self.globals.insert(
      name.to_string(),
      Global {
        id,
        private: true,
        declaration: true,
      },
    );
    Ok(Identifier::Global(id))
  }

  pub fn define_variable(
    &mut self,
    name: &'src str,
    name_sr: SourceRange,
  ) -> CompilerResult<Identifier> {
    if self.is_local_in_current_scope(name) {
      Err(parser_err::same_scope_name_redeclaration(name_sr, name))
    } else if self.in_global_scope() {
      self
        .declare_global(name, name_sr, false)
        .map(Identifier::Global)
    } else {
      if self.last_local_id == u8::MAX {
        return Err(parser_err::too_many_local_names(name_sr));
      }
      let local = Local {
        name,
        id: self.last_local_id,
        scope_local_id: self.names_in_current_scope,
        function_depth: self.functions_declaration_stack.len() as u8,
      };
      self.last_local_id += 1;
      self.locals.push(local);
      Ok(Identifier::Local(local.id))
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
    })
  }

  pub fn pop_function(&mut self) -> Vec<Identifier> {
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

  // TODO: move this functionality out of here
  pub fn create_extern_id(&mut self, id: Identifier) -> ExternId {
    if let Identifier::Global(id) = id {
      self.extern_ids.push(id);
      ExternId(self.extern_id_start + id)
    } else {
      panic!()
    }
  }

  pub fn set_module(&mut self, name: &str, name_sr: SourceRange) -> CompilerResult<ModuleId> {
    assert!(self.module_id.is_none(), "I don't need this case for now");

    let module_id = self.global_env.new_module(name, name_sr)?;
    self.module_id = Some(module_id);
    Ok(module_id)
  }

  pub fn import_module(&mut self, name: &str, name_sr: SourceRange) -> CompilerResult<ModuleId> {
    let module_id = self.global_env.get_module_id(name, name_sr)?;
    let module = self.global_env.get_module(module_id);
    // FIXME: does not check for double declarations
    self
      .globals
      .extend(module.public_names.iter().map(|(name, id)| {
        (
          name.to_owned(),
          Global {
            id: *id,
            private: true,
            declaration: false,
          },
        )
      }));

    Ok(module_id)
  }

  pub fn new(global_env: &'src mut GlobalEnv) -> Self {
    Self {
      globals: HashMap::new(),
      global_env,
      module_id: None,

      names_in_current_scope: 0,
      last_local_id: 0,
      locals: Vec::new(),
      scope_depth: 0,

      extern_ids: Vec::new(),
      extern_id_start: 0,
      functions_declaration_stack: Vec::new(),
    }
  }

  pub fn finalize(self) -> CompilerResult<()> {
    let module_public_globals = self
      .globals
      .into_iter()
      .filter(|(_, global)| global.private)
      .map(|(name, global)| (name, global.id))
      .collect::<HashMap<_, _>>();
    if let Some(module_id) = self.module_id {
      self.global_env.export_module(
        module_id,
        global_env::Module {
          public_names: module_public_globals,
        },
      );
    }
    Ok(())
  }
}

#[cfg(test)]
mod test {

  use std::collections::HashMap;

  use crate::compiler::{
    global_env::{GlobalEnv, Module},
    identifier::Identifier,
    lexer::SourceRange,
  };

  use super::Environment;

  #[test]
  fn access_global_variable_from_global_scope() {
    let mut global_env = GlobalEnv::new();
    let mut env = Environment::new(&mut global_env);
    env.define_variable("x", SourceRange::EMPTY);
    assert_eq!(
      env.get_name("x", SourceRange::EMPTY),
      Ok(Identifier::Global(0))
    );
  }

  #[test]
  fn access_global_variable_from_local_scope() {
    let mut global_env = GlobalEnv::new();
    let mut env = Environment::new(&mut global_env);
    env.define_variable("x", SourceRange::EMPTY);
    env.push_scope();
    assert_eq!(
      env.get_name("x", SourceRange::EMPTY),
      Ok(Identifier::Global(0))
    );
  }

  #[test]
  fn capture_once() {
    let mut global_env = GlobalEnv::new();
    let mut env = Environment::new(&mut global_env);
    env.push_function();
    env
      .define_variable("x", SourceRange::EMPTY)
      .expect("could not declare name");
    env.push_function();
    assert_eq!(
      env.get_name("x", SourceRange::EMPTY),
      Ok(Identifier::Capture(0))
    );
    assert_eq!(
      env.get_name("x", SourceRange::EMPTY),
      Ok(Identifier::Capture(0))
    );
    assert_eq!(
      env.get_name("x", SourceRange::EMPTY),
      Ok(Identifier::Capture(0))
    );
    let captures = env.pop_function();
    assert_eq!(captures, vec![Identifier::Local(0)]);
  }

  #[test]
  fn multilevel_capture() {
    let mut global_env = GlobalEnv::new();
    let mut env = Environment::new(&mut global_env);
    env.push_function();
    env
      .define_variable("x", SourceRange::EMPTY)
      .expect("could not declare name");
    env.push_function();
    env.push_function();
    assert_eq!(
      env.get_name("x", SourceRange::EMPTY),
      Ok(Identifier::Capture(0))
    );
    let captures = env.pop_function();
    assert_eq!(captures, vec![Identifier::Capture(0)]);
    let captures = env.pop_function();
    assert_eq!(captures, vec![Identifier::Local(0)]);
  }

  #[test]
  fn no_capture_for_globals() {
    let mut global_env = GlobalEnv::new();
    let mut env = Environment::new(&mut global_env);
    env
      .define_variable("x", SourceRange::EMPTY)
      .expect("could not declare name");
    env.push_function();
    assert_eq!(env.pop_function(), vec![]);
  }

  #[test]
  fn no_capture_for_globals_in_inner_scope() {
    let mut global_env = GlobalEnv::new();
    let mut env = Environment::new(&mut global_env);
    env
      .define_variable("x", SourceRange::EMPTY)
      .expect("could not declare name");
    env.push_function();
    env.push_scope();
    assert_eq!(
      env.get_name("x", SourceRange::EMPTY),
      Ok(Identifier::Global(0))
    );
  }

  #[test]
  fn no_capture_same_function_different_scope() {
    let mut global_env = GlobalEnv::new();
    let mut env = Environment::new(&mut global_env);
    env.push_function();
    env
      .define_variable("x", SourceRange::EMPTY)
      .expect("could not declare name");
    env.push_scope();
    assert_eq!(
      env.get_name("x", SourceRange::EMPTY),
      Ok(Identifier::Local(0))
    );
  }

  #[test]
  fn can_use_imported_names_in_anonymous_module() {
    let mut global_env = GlobalEnv::new();
    let module_id = global_env
      .new_module("test", SourceRange::EMPTY)
      .expect("could not create test module");
    let name_id = global_env.new_global_id();
    global_env.export_module(
      module_id,
      Module {
        public_names: HashMap::from([("x".to_string(), name_id)]),
      },
    );
    let mut env = Environment::new(&mut global_env);
    env
      .import_module("test", SourceRange::EMPTY)
      .expect("could not import test module");
    env
      .get_name("x", SourceRange::EMPTY)
      .expect("could not get id");
  }
}
