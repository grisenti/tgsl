use crate::compiler::{
  global_env::GlobalEnv,
  identifier::{ExternId, GlobalId, Identifier, ModuleId},
};

use super::*;

#[derive(Debug, Clone, Copy)]
struct LocalId {
  scope_depth: u8,
  function_depth: u8,
  id: u8,
}

#[derive(Debug, Clone, Copy)]
struct Local<'src> {
  name: &'src str,
  id: u8,             // local id used to refer to any local variable reachable from this scope
  scope_local_id: u8, // resets to 0 for any new scope
  function_depth: u8,
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
  imported_modules: Vec<ModuleId>,
  module_id: ModuleId,
  global: &'src mut GlobalEnv,

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

  pub fn get_name(&mut self, name: &str, name_sr: SourceRange) -> CompilerResult<Identifier> {
    if let Some(local) = self.find_local(name) {
      Ok(self.capture(local))
    } else {
      self
        .global
        .get_name(&self.imported_modules, name, name_sr)
        .map(Identifier::Global)
    }
  }

  pub fn declare_name(
    &mut self,
    name: &'src str,
    name_sr: SourceRange,
  ) -> CompilerResult<Identifier> {
    if self.is_local_in_current_scope(name) {
      Err(parser_err::same_scope_name_redeclaration(name_sr, name))
    } else if self.in_global_scope() {
      self
        .global
        .declare_name(self.module_id, name, name_sr)
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

  pub fn import_module(&mut self, id: ModuleId) {
    self.imported_modules.push(id);
  }

  pub fn new(global_env: &'src mut GlobalEnv) -> Self {
    let module_id = global_env.new_module();
    Self {
      global: global_env,
      module_id,
      imported_modules: vec![module_id],

      names_in_current_scope: 0,
      last_local_id: 0,
      locals: Vec::new(),
      scope_depth: 0,

      extern_ids: Vec::new(),
      extern_id_start: 0,
      functions_declaration_stack: Vec::new(),
    }
  }
}

#[cfg(test)]
mod test {

  use crate::compiler::{global_env::GlobalEnv, identifier::Identifier, lexer::SourceRange};

  use super::Environment;

  #[test]
  fn access_global_from_global_scope() {
    let mut global_env = GlobalEnv::new();
    let mut env = Environment::new(&mut global_env);
    env.declare_name("x", SourceRange::EMPTY);
    assert_eq!(
      env.get_name("x", SourceRange::EMPTY),
      Ok(Identifier::Global(0))
    );
  }

  #[test]
  fn access_global_from_local_scope() {
    let mut global_env = GlobalEnv::new();
    let mut env = Environment::new(&mut global_env);
    env.declare_name("x", SourceRange::EMPTY);
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
      .declare_name("x", SourceRange::EMPTY)
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
      .declare_name("x", SourceRange::EMPTY)
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
      .declare_name("x", SourceRange::EMPTY)
      .expect("could not declare name");
    env.push_function();
    assert_eq!(env.pop_function(), vec![]);
  }

  #[test]
  fn no_capture_for_globals_in_inner_scope() {
    let mut global_env = GlobalEnv::new();
    let mut env = Environment::new(&mut global_env);
    env
      .declare_name("x", SourceRange::EMPTY)
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
      .declare_name("x", SourceRange::EMPTY)
      .expect("could not declare name");
    env.push_scope();
    assert_eq!(
      env.get_name("x", SourceRange::EMPTY),
      Ok(Identifier::Local(0))
    );
  }
}
