use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::rc::Rc;

use crate::compiler::types::Type;

type StructModifierT = u8;

#[derive(Clone, Copy)]
struct StructModifiers(StructModifierT);

impl StructModifiers {
  const DEFINED: StructModifierT = 2;

  fn check(self, bits: StructModifierT) -> bool {
    (self.0 & bits) != 0
  }

  fn set(&mut self, bits: StructModifierT) {
    self.0 |= bits;
  }
}

pub struct Struct {
  name: Rc<str>,
  member_names: Vec<String>,
  member_types: Vec<Type>,
  modifiers: StructModifiers,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub struct MemberIndex(usize);

impl MemberIndex {
  pub fn get_index(self) -> usize {
    self.0
  }
}

impl Struct {
  pub fn new(name: Rc<str>) -> Self {
    Self {
      name,
      member_names: Vec::new(),
      member_types: Vec::new(),
      modifiers: StructModifiers(0),
    }
  }

  pub fn define(&mut self, member_names: Vec<String>, member_types: Vec<Type>) {
    assert_eq!(member_types.len(), member_types.len());
    self.member_names = member_names;
    self.member_types = member_types;
    self.modifiers.set(StructModifiers::DEFINED);
  }

  pub fn is_defined(&self) -> bool {
    self.modifiers.check(StructModifiers::DEFINED)
  }

  pub fn get_member_index(&self, member_name: &str) -> Option<MemberIndex> {
    self
      .member_names
      .iter()
      .position(|name| name == member_name)
      .map(MemberIndex)
  }
  pub fn member_info(&self, index: MemberIndex) -> (&str, &Type) {
    assert!(index.0 < self.member_types.len());
    (&self.member_names[index.0], &self.member_types[index.0])
  }

  pub fn get_name(&self) -> &str {
    &self.name
  }

  pub fn clone_name(&self) -> Rc<str> {
    self.name.clone()
  }

  pub fn get_member_types(&self) -> &[Type] {
    &self.member_types
  }
}

#[derive(Default)]
pub struct GlobalStructs<'s> {
  imported: HashMap<Rc<str>, &'s Struct>,
  module: HashMap<Rc<str>, Struct>,
}

pub struct ExportedGlobalStructs {
  global_structs: HashMap<Rc<str>, Struct>,
}

impl<'s> GlobalStructs<'s> {
  pub fn import(&mut self, module_structs: &'s ExportedGlobalStructs) -> Result<(), ()> {
    for (name, s) in &module_structs.global_structs {
      assert!(!self.module.contains_key(name), "todo: consider errors");
      match self.imported.entry(name.clone()) {
        Entry::Occupied(_) => {
          todo!("error: struct was already imported")
        }
        Entry::Vacant(e) => {
          e.insert(s);
        }
      }
    }
    Ok(())
  }

  pub fn get_mut(&mut self, name: &str) -> Option<&mut Struct> {
    // TODO: what about imported
    self.module.get_mut(name)
  }

  pub fn get(&self, name: &str) -> Option<&Struct> {
    self
      .module
      .get(name)
      .or_else(|| self.imported.get(name).map(|s| *s))
  }

  pub fn export(self) -> ExportedGlobalStructs {
    assert!(self.module.iter().all(|(_, s)| s.is_defined()));
    ExportedGlobalStructs {
      global_structs: self.module,
    }
  }

  pub fn declare(&mut self, name: &str) -> bool {
    if self.imported.contains_key(name) {
      return false;
    }
    let name: Rc<str> = Rc::from(name);
    match self.module.entry(name.clone()) {
      Entry::Vacant(e) => {
        e.insert(Struct::new(name));
        true
      }
      Entry::Occupied(_) => false,
    }
  }

  pub fn define(&mut self, name: &str, member_names: Vec<String>, member_types: Vec<Type>) -> bool {
    assert!(!self.imported.contains_key(name));

    if let Some(s) = self.get_mut(name) {
      if s.is_defined() {
        false
      } else {
        s.define(member_names, member_types);
        true
      }
    } else {
      let name: Rc<str> = Rc::from(name);
      self.module.insert(
        name.clone(),
        Struct {
          name,
          member_names,
          member_types,
          modifiers: StructModifiers(StructModifiers::DEFINED),
        },
      );
      true
    }
  }
}
