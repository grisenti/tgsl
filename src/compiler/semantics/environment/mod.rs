use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::compiler::codegen::program_chunk::ProgramChunk;
use crate::compiler::functions::overload_set::{FunctionIndex, OverloadSet, OverloadedFunction};
use crate::compiler::global_env::GlobalEnv;
use crate::compiler::global_symbols::GlobalSymbol;
use crate::compiler::ir::{Capture, CapturedVarIndex, LocalVarIndex, VarIndex};
use crate::compiler::types::{FunctionSignature, Type};

pub mod functions;
pub mod imports;
pub mod types;
pub mod variables;

#[derive(Debug, Clone)]
struct Local {
  name: Rc<str>,
  id: u8,
  // local id used to refer to any local variable reachable from this scope
  scope_local_id: u8,
  // resets to 0 for any new scope
  function_depth: u8,
  type_: Type,
}

struct Function {
  name: Rc<str>,
  captures: Vec<Capture>,
  return_type: Type,
  code: ProgramChunk,
}

impl Function {
  fn get_capture(&self, capture: Capture) -> Option<CapturedVarIndex> {
    self
      .captures
      .iter()
      .position(|captured_variable| *captured_variable == capture)
      .map(|index| index as u8)
  }

  fn get_or_capture_above_capture(&mut self, above_capture: CapturedVarIndex) -> CapturedVarIndex {
    self
      .get_capture(Capture::Capture(above_capture))
      .unwrap_or_else(|| {
        let capture_id = self.captures.len() as u8;
        self.captures.push(Capture::Capture(above_capture));
        capture_id
      })
  }

  fn get_or_capture_above_local(&mut self, above_local: LocalVarIndex) -> CapturedVarIndex {
    self
      .get_capture(Capture::Local(above_local))
      .unwrap_or_else(|| {
        let capture_id = self.captures.len() as u8;
        self.captures.push(Capture::Local(above_local));
        capture_id
      })
  }
}

pub struct FinalizedFunction {
  pub captures: Vec<Capture>,
  pub code: ProgramChunk,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeclarationError {
  AlreadyDeclaredAsVariable,
  AlreadyDeclaredAsType,
  AlreadyDeclaredAsFunction,
  AlreadyDeclaredAsNativeFunction,
  AlreadyDeclaredAsForeignFunction,
  TooManyLocalNames,
}

pub type DeclarationResult<T> = Result<T, DeclarationError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportError {
  NameRedefinition(String),
  OverloadRedefinition(String),
  NotAValidModule,
}

pub type ImportResult = Result<(), ImportError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NameError {
  UndeclaredName,
  NotAVariable,
  NotAFunction,
  NotAType,
}

pub type NameResult<T> = Result<T, NameError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VarGetError {
  NotDeclared,
  NameIsType,
  NameIsFunction,
}

pub struct ExportedEnv {
  pub global_symbols: HashMap<Rc<str>, GlobalSymbol>,
}

pub struct Environment<'src> {
  global_env: &'src GlobalEnv,

  locals: Vec<Local>,
  scope_depth: u8,
  local_names: u8,
  names_in_current_scope: u8,
  functions_declaration_stack: Vec<Function>,
  declared_functions: u32,

  imported_modules: HashSet<Rc<str>>,

  last_global_variable_index: u32,
  last_struct_index: u32,
  last_native_function_index: u32,
  last_foreign_function_index: u32,
  global_symbols: HashMap<Rc<str>, GlobalSymbol>,
}

impl<'src> Environment<'src> {
  pub fn in_global_scope(&self) -> bool {
    self.scope_depth == 0
  }

  pub fn get_global(&self, name: &str) -> Option<&GlobalSymbol> {
    self.global_symbols.get(name)
  }

  fn declare_global(&mut self, name: Rc<str>, symbol: GlobalSymbol) -> DeclarationResult<()> {
    match self.global_symbols.entry(name) {
      Entry::Occupied(e) => {
        return match e.get() {
          GlobalSymbol::GlobalVariable { .. } => Err(DeclarationError::AlreadyDeclaredAsVariable),
          GlobalSymbol::OverloadSet { .. } => Err(DeclarationError::AlreadyDeclaredAsFunction),
          GlobalSymbol::Struct { .. } | GlobalSymbol::UndefinedStruct => {
            Err(DeclarationError::AlreadyDeclaredAsType)
          }
        }
      }
      Entry::Vacant(e) => {
        e.insert(symbol);
        Ok(())
      }
    }
  }

  pub fn declare_variable(&mut self, name: Rc<str>, type_: Type) -> DeclarationResult<VarIndex> {
    if self.in_global_scope() {
      let index = self.last_global_variable_index;
      self.last_global_variable_index += 1;
      self
        .declare_global(
          name,
          GlobalSymbol::GlobalVariable {
            index,
            module_index: 0,
            type_,
          },
        )
        .map(|_| VarIndex::Global {
          index,
          module_index: 0,
        })
    } else {
      let id = self.local_names;
      if id == u8::MAX {
        return Err(DeclarationError::TooManyLocalNames);
      }
      self.local_names += 1;
      self.names_in_current_scope += 1;
      self.locals.push(Local {
        name,
        id,
        scope_local_id: self.names_in_current_scope,
        function_depth: self.scope_depth,
        type_,
      });
      Ok(VarIndex::Local(id))
    }
  }

  fn get_local_var_or_capture(&mut self, name: &str) -> NameResult<(VarIndex, Type)> {
    todo!()
  }

  pub fn get_or_capture_variable(&mut self, name: &str) -> NameResult<(VarIndex, Type)> {
    if self.in_global_scope() {
      let global = self.get_global(name).ok_or(NameError::UndeclaredName)?;
      if let GlobalSymbol::GlobalVariable {
        index,
        module_index,
        type_,
      } = global
      {
        Ok((
          VarIndex::Global {
            index: *index,
            module_index: *module_index,
          },
          type_.clone(),
        ))
      } else {
        Err(NameError::NotAVariable)
      }
    } else {
      todo!()
    }
  }

  fn add_global_function(
    &mut self,
    name: Rc<str>,
    signature: FunctionSignature,
    index: FunctionIndex,
  ) -> DeclarationResult<()> {
    match self.global_symbols.get_mut(&name) {
      Some(GlobalSymbol::OverloadSet(overload_set)) => {
        add_to_overload_set(overload_set, signature, index)
      }
      None => self.declare_global(
        name,
        GlobalSymbol::OverloadSet(OverloadSet::new(OverloadedFunction { signature, index })),
      ),
      Some(GlobalSymbol::GlobalVariable { .. }) => Err(DeclarationError::AlreadyDeclaredAsVariable),
      Some(GlobalSymbol::Struct { .. }) | Some(GlobalSymbol::UndefinedStruct) => {
        Err(DeclarationError::AlreadyDeclaredAsType)
      }
    }
  }

  pub fn declare_native_function(
    &mut self,
    name: Rc<str>,
    signature: FunctionSignature,
  ) -> DeclarationResult<()> {
    let index = self.last_native_function_index;
    let index = FunctionIndex::UndefinedNative { index };
    let result = self.add_global_function(name, signature, index);
    if result.is_ok() {
      self.last_native_function_index += 1;
    }
    result
  }

  pub fn declare_foreign_function(
    &mut self,
    name: Rc<str>,
    signature: FunctionSignature,
  ) -> DeclarationResult<()> {
    let index = self.last_foreign_function_index;
    let index = FunctionIndex::Foreign {
      index,
      module_index: 0,
    };
    let result = self.add_global_function(name, signature, index);
    if result.is_ok() {
      self.last_foreign_function_index += 1;
    }
    result
  }

  fn define_native_function(
    &mut self,
    name: Rc<str>,
    signature: FunctionSignature,
  ) -> DeclarationResult<()> {
    let index = self.last_native_function_index;
    let index = FunctionIndex::Native {
      index,
      module_index: 0,
    };
    let result = self.add_global_function(name, signature, index);
    if result.is_ok() {
      self.last_native_function_index += 1;
    }
    result
  }

  pub fn start_function_definition(
    &mut self,
    name: Rc<str>,
    signature: FunctionSignature,
  ) -> DeclarationResult<()> {
    self.define_native_function(name.clone(), signature)?;
    self.names_in_current_scope = 0;
    self.local_names = 0;
    self.push_scope();
    self.functions_declaration_stack.push(Function {
      name: name.clone(),
      captures: Vec::new(),
      return_type,
      code: ProgramChunk::new(name.to_string()),
    })
  }

  pub fn get_current_function_return_type(&self) -> Option<&Type> {
    self
      .functions_declaration_stack
      .last()
      .map(|func| &func.return_type)
  }

  pub fn end_function_definition(&mut self) -> Vec<Capture> {
    assert!(!self.functions_declaration_stack.is_empty());

    self.pop_scope();
    if let Some(last_local) = self.locals.last() {
      self.local_names = last_local.id + 1;
      self.names_in_current_scope = last_local.scope_local_id + 1;
    } else {
      self.local_names = 0;
      self.names_in_current_scope = 0;
    }
    let function = self
      .functions_declaration_stack
      .pop()
      .expect("no functions to pop");
    function.captures.iter().copied().collect()
  }

  pub fn declare_struct(&mut self, name: Rc<str>) -> DeclarationResult<()> {
    todo!()
  }

  pub fn define_struct(
    &mut self,
    name: Rc<str>,
    member_names: Vec<String>,
    member_types: Vec<Type>,
  ) -> DeclarationResult<()> {
    todo!()
  }

  pub fn import_module(&mut self, module_name: &str) -> ImportResult {
    todo!()
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
    self.local_names -= names_in_current_scope;
    names_in_current_scope
  }

  pub fn push_scope(&mut self) {
    self.scope_depth += 1;
    self.names_in_current_scope = 0;
  }

  pub fn ensure_module_name_available(&self, name: &str) -> bool {
    self.global_env.is_module_name_available(name)
  }

  pub fn function_stack(&self) -> &str {
    self
      .functions_declaration_stack
      .last()
      .map(|func| func.name.as_ref())
      .unwrap_or("")
  }

  pub fn export(self) -> ExportedEnv {
    todo!()
  }

  pub fn new(global_env: &'src GlobalEnv) -> Self {
    Self {
      global_env,

      locals: Vec::new(),
      scope_depth: 0,
      local_names: 0,
      names_in_current_scope: 0,
      functions_declaration_stack: Vec::new(),
      declared_functions: 0,

      imported_modules: HashSet::new(),

      last_native_function_index: 0,
      last_foreign_function_index: 0,
      last_global_variable_index: 0,
      last_struct_index: 0,

      global_symbols: Default::default(),
    }
  }
}

fn add_to_overload_set(
  overload_set: &mut OverloadSet,
  signature: FunctionSignature,
  index: FunctionIndex,
) -> DeclarationResult<()> {
  if let Ok(func) = overload_set.find(signature.get_parameters()) {
    match func.index {
      FunctionIndex::UndefinedNative { .. } => {
        Err(DeclarationError::AlreadyDeclaredAsNativeFunction)
      }
      FunctionIndex::Native { .. } => Err(DeclarationError::AlreadyDeclaredAsNativeFunction),
      FunctionIndex::Foreign { .. } => Err(DeclarationError::AlreadyDeclaredAsForeignFunction),
    }
  } else {
    overload_set.add(OverloadedFunction { signature, index });
    Ok(())
  }
}

#[cfg(disabled)]
mod test {
  use crate::compiler::global_env::GlobalEnv;
  use crate::compiler::semantics::environment::Capture;
  use crate::compiler::types::Type;

  use super::Environment;

  #[test]
  fn capture_once() {
    let global_env = GlobalEnv::new();
    let mut env = Environment::new(&global_env);
    env.start_function_definition("outer".to_string(), Type::Nothing);
    let var_id = env
      .declare_local_var("x", Type::Bool)
      .expect("could not declare name");
    env.start_function_definition("inner".to_string(), Type::Nothing);
    assert_eq!(env.get_capture_or_capture_var("x"), Some((0, Type::Bool)));
    assert_eq!(env.get_capture_or_capture_var("x"), Some((0, Type::Bool)));
    assert_eq!(env.get_capture_or_capture_var("x"), Some((0, Type::Bool)));
    let function = env.end_function_definition();
    assert_eq!(function.captures, vec![Capture::Local(var_id)]);
  }

  #[test]
  fn multilevel_capture() {
    let global_env = GlobalEnv::new();
    let mut env = Environment::new(&global_env);
    env.start_function_definition("1".to_string(), Type::Nothing);
    env
      .declare_local_var("x", Type::Num)
      .expect("could not declare global variable");
    env.start_function_definition("2".to_string(), Type::Nothing);
    env.start_function_definition("3".to_string(), Type::Nothing);
    assert_eq!(env.get_capture_or_capture_var("x"), Some((0, Type::Num)));
    let captures = env.end_function_definition().captures;
    assert_eq!(captures, vec![Capture::Capture(0)]);
    let captures = env.end_function_definition().captures;
    assert_eq!(captures, vec![Capture::Local(0)]);
  }

  #[test]
  fn no_capture_for_globals() {
    let global_env = GlobalEnv::new();
    let mut env = Environment::new(&global_env);
    env
      .declare_global_var("x", Type::Bool)
      .expect("could not declare name");
    env.start_function_definition("func".to_string(), Type::Nothing);
    assert!(env.get_capture_or_capture_var("x").is_none());
    assert_eq!(env.end_function_definition().captures, vec![]);
  }

  #[test]
  fn no_capture_for_globals_in_inner_function_scope() {
    let global_env = GlobalEnv::new();
    let mut env = Environment::new(&global_env);
    env
      .declare_global_var("x", Type::Str)
      .expect("could not declare name");
    env.start_function_definition("h".to_string(), Type::Bool);
    env.push_scope();
    assert!(env.get_capture_or_capture_var("x").is_none());
  }
}
