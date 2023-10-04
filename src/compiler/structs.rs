use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::rc::Rc;

use crate::compiler::types::Type;

pub struct Struct {
  name: Rc<str>,
  member_names: Vec<String>,
  member_types: Vec<Type>,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub struct MemberIndex(usize);

impl MemberIndex {
  pub fn get_index(self) -> usize {
    self.0
  }
}

impl Struct {
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

#[derive(Copy, Clone, Debug)]
pub enum StructGetError {
  NotDefined,
  NotAStruct,
  MultipleDefinitions,
  RedefinedImport,
}

pub type StructGetResult<'s> = Result<&'s Struct, StructGetError>;

#[derive(Copy, Clone, Debug)]
pub enum StructInsertError {
  NameIsAlreadyAFunction,
  NameIsAlreadyAVariable,
  AlreadyDefined,
  AlreadyDeclared,
  Imported,
  TooManyStructs,
}

pub type StructInsertResult = Result<(), StructInsertError>;

#[derive(Copy, Clone, Eq, PartialEq)]
enum StructIndexKind {
  ImportedNative,
  ModuleNative,
  UndefinedModuleNative,
  ErrorMultipleDefinitions,
  ErrorRedefinedImport,
}

#[derive(Copy, Clone)]
struct StructIndex {
  index: u32,
  kind: StructIndexKind,
}

impl StructIndex {
  fn error_index(kind: StructIndexKind) -> Self {
    Self {
      kind,
      index: u32::MAX,
    }
  }

  fn undefined() -> Self {
    Self {
      kind: StructIndexKind::UndefinedModuleNative,
      index: u32::MAX,
    }
  }
}

#[derive(Default)]
pub struct GlobalStructs<'s> {
  struct_names: HashMap<Rc<str>, StructIndex>,

  imported_structs: Vec<&'s Struct>,
  module_structs: Vec<Struct>,
}

pub struct ExportedGlobalStructs {
  struct_names: HashMap<Rc<str>, StructIndex>,
  module_structs: Vec<Struct>,
}

impl<'s> GlobalStructs<'s> {
  pub fn import(&mut self, module_structs: &'s ExportedGlobalStructs) -> Result<(), ()> {
    self
      .imported_structs
      .reserve(module_structs.module_structs.len());
    self.struct_names.reserve(module_structs.struct_names.len());
    let mut last_index = self.imported_structs.len();

    for (name, index) in &module_structs.struct_names {
      match self.struct_names.entry(name.clone()) {
        Entry::Occupied(_) => todo!("error: struct was already imported"),
        Entry::Vacant(e) => {
          e.insert(StructIndex {
            index: last_index as u32,
            kind: StructIndexKind::ImportedNative,
          });
          self
            .imported_structs
            .push(&module_structs.module_structs[index.index as usize]);
          last_index += 1;
        }
      }
    }
    Ok(())
  }

  pub fn get(&self, name: &str) -> StructGetResult {
    let struct_index = self
      .struct_names
      .get(name)
      .ok_or(StructGetError::NotAStruct)?;

    match struct_index.kind {
      StructIndexKind::ImportedNative => Ok(self.imported_structs[struct_index.index as usize]),
      StructIndexKind::ModuleNative => Ok(&self.module_structs[struct_index.index as usize]),
      StructIndexKind::UndefinedModuleNative => Err(StructGetError::NotDefined),
      StructIndexKind::ErrorMultipleDefinitions => Err(StructGetError::MultipleDefinitions),
      StructIndexKind::ErrorRedefinedImport => Err(StructGetError::RedefinedImport),
    }
  }

  pub fn export(mut self) -> Result<ExportedGlobalStructs, Vec<Rc<str>>> {
    let mut undefined_structs = Vec::new();
    self.struct_names.retain(|name, idx| {
      if idx.kind == StructIndexKind::UndefinedModuleNative {
        undefined_structs.push(name.clone());
      }
      idx.kind == StructIndexKind::ModuleNative
    });
    if undefined_structs.is_empty() {
      Ok(ExportedGlobalStructs {
        struct_names: self.struct_names,
        module_structs: self.module_structs,
      })
    } else {
      Err(undefined_structs)
    }
  }

  pub fn declare(&mut self, name: &str) -> StructInsertResult {
    if self.struct_names.len() > u32::MAX as usize {
      return Err(StructInsertError::TooManyStructs);
    }
    match self.struct_names.entry(Rc::from(name)) {
      Entry::Occupied(e) => match e.get().kind {
        StructIndexKind::ImportedNative => Err(StructInsertError::Imported),
        StructIndexKind::ModuleNative => Err(StructInsertError::AlreadyDefined),
        StructIndexKind::UndefinedModuleNative
        | StructIndexKind::ErrorMultipleDefinitions
        | StructIndexKind::ErrorRedefinedImport => Err(StructInsertError::AlreadyDeclared),
      },
      Entry::Vacant(e) => {
        e.insert(StructIndex::undefined());
        Ok(())
      }
    }
  }

  /// panics if self.module_structs.len() > u32::MAX
  fn add_struct(
    &mut self,
    name: Rc<str>,
    member_names: Vec<String>,
    member_types: Vec<Type>,
  ) -> StructIndex {
    let index = self.module_structs.len() as u32;
    self.module_structs.push(Struct {
      name,
      member_names,
      member_types,
    });
    StructIndex {
      kind: StructIndexKind::ModuleNative,
      index,
    }
  }

  fn define_already_inserted(
    &mut self,
    name: Rc<str>,
    member_names: Vec<String>,
    member_types: Vec<Type>,
    struct_index_kind: StructIndexKind,
  ) -> Result<(), StructInsertError> {
    match struct_index_kind {
      StructIndexKind::ImportedNative => {
        self.struct_names.insert(
          name,
          StructIndex::error_index(StructIndexKind::ErrorRedefinedImport),
        );
        Err(StructInsertError::Imported)
      }
      StructIndexKind::ModuleNative => {
        self.struct_names.insert(
          name,
          StructIndex::error_index(StructIndexKind::ErrorMultipleDefinitions),
        );
        Err(StructInsertError::AlreadyDefined)
      }
      StructIndexKind::UndefinedModuleNative => {
        let new_struct_index = self.add_struct(name.clone(), member_names, member_types);
        self.struct_names.insert(name, new_struct_index);
        Ok(())
      }
      StructIndexKind::ErrorRedefinedImport | StructIndexKind::ErrorMultipleDefinitions => {
        Err(StructInsertError::AlreadyDefined)
      }
    }
  }

  pub fn define(
    &mut self,
    name: &str,
    member_names: Vec<String>,
    member_types: Vec<Type>,
  ) -> StructInsertResult {
    if let Some((name, struct_index)) = self.struct_names.remove_entry(name) {
      self.define_already_inserted(name, member_names, member_types, struct_index.kind)
    } else if self.struct_names.len() > u32::MAX as usize {
      Err(StructInsertError::TooManyStructs)
    } else {
      let name: Rc<str> = Rc::from(name);
      let struct_index = self.add_struct(name.clone(), member_names, member_types);
      self.struct_names.insert(name, struct_index);
      Ok(())
    }
  }

  pub fn is_struct(&self, name: &str) -> bool {
    self.struct_names.contains_key(name)
  }
}
