use crate::compiler::identifier::Identifier;

use super::*;
use std::collections::{HashMap, HashSet};

struct Function {
  captures: Vec<Identifier>,
}

#[derive(Debug, Clone, Copy)]
struct LocalId {
  scope_depth: u8,
  id: u8,
}

#[derive(Default)]
pub struct Environment {
  locals: Vec<(String, LocalId)>,
  last_local_id: u8,
  globals: HashMap<String, Identifier>,
  global_types: Vec<Type>,
  functions_declaration_stack: Vec<Function>,
  declared: HashSet<Identifier>,
  last_global_id: u16,
  scope_depth: u8,
}

pub struct FinalizedEnvironment {
  pub global_types: Vec<Type>,
}

impl Environment {
  fn add_global_name(&mut self, name: String) -> Identifier {
    let id = Identifier::Global(self.last_global_id);
    self.globals.insert(name, id);
    self.last_global_id += 1;
    id
  }

  fn declare_global_name(
    &mut self,
    name: &str,
    name_src_info: SourceInfo,
  ) -> Result<Identifier, SourceError> {
    let id = self
      .globals
      .get(name)
      .copied()
      .unwrap_or_else(|| self.add_global_name(name.to_string()));
    if self.declared.contains(&id) {
      Err(error_from_source_info(
        &name_src_info,
        format!("redeclaration of global name {name}"),
      ))
    } else {
      self.declared.insert(id);
      Ok(id)
    }
  }

  pub fn in_global_scope(&self) -> bool {
    self.scope_depth == 0
  }

  pub fn set_global_type(&mut self, id: Identifier, new_type: Type) {
    if let Identifier::Global(global_id) = id {
      let index = global_id as usize;
      if self.global_types.len() <= index {
        self.global_types.resize(index + 1, Type::Unknown);
      }
      self.global_types[global_id as usize] = new_type;
    } else {
      panic!("tried to set global type for local variable");
    }
  }

  fn find_local(&self, name: &str) -> Option<LocalId> {
    self
      .locals
      .iter()
      .rev()
      .find(|(local_name, _)| local_name == name)
      .map(|pair| pair.1)
  }

  fn capture(
    &mut self,
    LocalId {
      scope_depth: target_depth,
      id,
    }: LocalId,
  ) -> Identifier {
    let start_depth = self.scope_depth;
    debug_assert!(start_depth >= target_depth);
    let mut capture_id = Identifier::Local(id);
    for func in self
      .functions_declaration_stack
      .iter_mut()
      .rev()
      .take((start_depth - target_depth) as usize)
      .rev()
    {
      let tmp = capture_id;
      capture_id = Identifier::Capture(func.captures.len() as u8);
      func.captures.push(tmp);
    }
    capture_id
  }

  pub fn get_name_or_add_global(&mut self, name: &str) -> Identifier {
    if let Some(id) = self.find_local(name) {
      self.capture(id)
    } else if let Some(global_id) = self.globals.get(name) {
      *global_id
    } else {
      self.add_global_name(name.to_string())
    }
  }

  pub fn declare_name_or_err(
    &mut self,
    name: &str,
    name_src_info: SourceInfo,
  ) -> Result<Identifier, SourceError> {
    if self
      .locals
      .iter()
      .rev()
      .take_while(|(_, LocalId { scope_depth, .. })| self.scope_depth == *scope_depth)
      .any(|(local_name, _)| local_name == name)
    {
      Err(error_from_source_info(
        &name_src_info,
        format!("cannot redeclare name '{name}' in the same scope"),
      ))
    } else if self.in_global_scope() {
      self.declare_global_name(name, name_src_info)
    } else {
      if self.last_local_id == u8::MAX {
        return Err(error_from_source_info(
          &name_src_info,
          "too many local names ".to_string(),
        ));
      }
      let id = LocalId {
        scope_depth: self.scope_depth,
        id: self.last_local_id,
      };
      self.last_local_id += 1;
      self.locals.push((name.to_string(), id));
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
      *id
    } else {
      0
    };
    self.functions_declaration_stack.pop().unwrap().captures
  }

  pub fn new() -> Self {
    Self {
      ..Default::default()
    }
  }

  pub fn finalize(mut self) -> FinalizedEnvironment {
    self
      .global_types
      .resize(self.last_global_id as usize + 1, Type::Unknown);
    FinalizedEnvironment {
      global_types: self.global_types,
    }
  }
}
