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
struct CaptureId {
  id: Identifier,
  local_id: u8,
}

struct Function {
  captures: Vec<CaptureId>,
}

pub struct Environment<'compilation> {
  imported_modules: Vec<ModuleId>,
  global: &'compilation mut GlobalEnv,

  locals: Vec<(&'compilation str, LocalId)>,
  last_local_id: u8,

  extern_ids: Vec<GlobalId>,
  extern_id_start: u16,
  functions_declaration_stack: Vec<Function>,
  scope_depth: u8,
}

impl<'compilation> Environment<'compilation> {
  pub fn in_global_scope(&self) -> bool {
    self.scope_depth == 0
  }

  fn find_local(&self, name: &str) -> Option<LocalId> {
    self
      .locals
      .iter()
      .rev()
      .find(|(local_name, _)| *local_name == name)
      .map(|pair| pair.1)
  }

  fn capture(
    &mut self,
    LocalId {
      function_depth: target_depth,
      id,
      ..
    }: LocalId,
  ) -> Identifier {
    let start_depth = self.functions_declaration_stack.len() as u8;
    debug_assert!(start_depth >= target_depth);
    let mut capture_id = Identifier::Local(id);
    for func in self
      .functions_declaration_stack
      .iter_mut()
      .rev()
      .take((start_depth - target_depth) as usize)
      .rev()
    {
      // TODO: very slow, think of something different
      if let Some(capture_index) = func.captures.iter().position(|c| c.local_id == id) {
        capture_id = Identifier::Capture(capture_index as u8);
      } else {
        let tmp = capture_id;
        capture_id = Identifier::Capture(func.captures.len() as u8);
        func.captures.push(CaptureId {
          id: tmp,
          local_id: id,
        });
      }
    }
    capture_id
  }

  pub fn get_name_or_add_global(
    &mut self,
    name: &str,
    source_info: SourceRange,
  ) -> CompilerResult<Identifier> {
    if let Some(id) = self.find_local(name) {
      Ok(self.capture(id))
    } else {
      self
        .global
        .get_or_add(&self.imported_modules, name, source_info)
    }
  }

  pub fn get_name(
    &mut self,
    name: &str,
    name_sr: SourceRange,
  ) -> CompilerResult<Option<Identifier>> {
    if let Some(id) = self.find_local(name) {
      Ok(Some(self.capture(id)))
    } else {
      self.global.get_name(&self.imported_modules, name, name_sr)
    }
  }

  pub fn set_type_if_global(&mut self, id: Identifier, type_id: TypeId) {
    self.global.set_type_if_global(id, type_id);
  }

  pub fn declare_name_or_err(
    &mut self,
    name: &'compilation str,
    name_sr: SourceRange,
  ) -> CompilerResult<Identifier> {
    if self
      .locals
      .iter()
      .rev()
      .take_while(|(_, LocalId { scope_depth, .. })| self.scope_depth == *scope_depth)
      .any(|(local_name, _)| *local_name == name)
    {
      Err(parser_err::same_scope_name_redeclaration(name_sr, name))
    } else if self.in_global_scope() {
      self.global.declare_name(name, name_sr)
    } else {
      if self.last_local_id == u8::MAX {
        return Err(parser_err::too_many_local_names(name_sr));
      }
      let id = LocalId {
        scope_depth: self.scope_depth,
        function_depth: self.functions_declaration_stack.len() as u8,
        id: self.last_local_id,
      };
      self.last_local_id += 1;
      self.locals.push((name, id));
      Ok(Identifier::Local(id.id))
    }
  }

  pub fn pop_scope(&mut self) -> u8 {
    debug_assert!(self.scope_depth > 0);
    let names_in_local_scope = self
      .locals
      .iter()
      .rev()
      .take_while(|(_, LocalId { scope_depth, .. })| self.scope_depth == *scope_depth)
      .count();
    self
      .locals
      .truncate(self.locals.len() - names_in_local_scope);
    self.scope_depth -= 1;
    self.last_local_id -= names_in_local_scope as u8;
    names_in_local_scope as u8
  }

  pub fn push_scope(&mut self) {
    self.scope_depth += 1;
  }

  pub fn push_function(&mut self) {
    self.last_local_id = 0;
    self.push_scope();
    self.functions_declaration_stack.push(Function {
      captures: Vec::new(),
    })
  }

  pub fn pop_function(&mut self) -> Vec<Identifier> {
    assert!(!self.functions_declaration_stack.is_empty());
    self.pop_scope();
    // all of the functions variables should be out of the stack
    self.last_local_id = if let Some((_, LocalId { id, .. })) = self.locals.last() {
      *id + 1
    } else {
      0
    };
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

  pub fn new(global_env: &'compilation mut GlobalEnv) -> Self {
    Self {
      global: global_env,
      imported_modules: Vec::new(),
      locals: Vec::new(),
      last_local_id: 0,
      extern_ids: Vec::new(),
      extern_id_start: 0,
      functions_declaration_stack: Vec::new(),
      scope_depth: 0,
    }
  }

  pub fn finalize(self) -> Result<Vec<GlobalId>, Vec<CompilerError>> {
    if let Err(err) = self.global.finalize_current_module() {
      Err(err)
    } else {
      Ok(self.extern_ids)
    }
  }
}

#[cfg(test)]
mod test {

  use crate::compiler::{global_env::GlobalEnv, identifier::Identifier, lexer::SourceRange};

  use super::Environment;

  #[test]
  fn capture_once() {
    let mut global_env = GlobalEnv::new();
    let mut env = Environment::new(&mut global_env);
    env.push_function();
    env
      .declare_name_or_err("x", SourceRange::EMPTY)
      .expect("could not declare name");
    env.push_function();
    assert_eq!(
      env.get_name_or_add_global("x", SourceRange::EMPTY),
      Ok(Identifier::Capture(0))
    );
    assert_eq!(
      env.get_name_or_add_global("x", SourceRange::EMPTY),
      Ok(Identifier::Capture(0))
    );
    assert_eq!(
      env.get_name_or_add_global("x", SourceRange::EMPTY),
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
      .declare_name_or_err("x", SourceRange::EMPTY)
      .expect("could not declare name");
    env.push_function();
    env.push_function();
    assert_eq!(
      env.get_name_or_add_global("x", SourceRange::EMPTY),
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
      .declare_name_or_err("x", SourceRange::EMPTY)
      .expect("could not declare name");
    env.push_function();
    assert_eq!(env.pop_function(), vec![]);
  }

  #[test]
  fn no_capture_for_globals_in_inner_scope() {
    let mut global_env = GlobalEnv::new();
    let mut env = Environment::new(&mut global_env);
    env
      .declare_name_or_err("x", SourceRange::EMPTY)
      .expect("could not declare name");
    env.push_function();
    env.push_scope();
    assert_eq!(
      env.get_name_or_add_global("x", SourceRange::EMPTY),
      Ok(Identifier::Global(0))
    );
  }

  #[test]
  fn no_capture_same_function_different_scope() {
    let mut global_env = GlobalEnv::new();
    let mut env = Environment::new(&mut global_env);
    env.push_function();
    env
      .declare_name_or_err("x", SourceRange::EMPTY)
      .expect("could not declare name");
    env.push_scope();
    assert_eq!(
      env.get_name_or_add_global("x", SourceRange::EMPTY),
      Ok(Identifier::Local(0))
    );
  }
}
