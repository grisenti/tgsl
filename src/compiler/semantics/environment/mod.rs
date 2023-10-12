use crate::compiler::codegen::function_code::FunctionCode;
use crate::compiler::functions::{ExportedFunctions, ForeignFunction, GlobalFunctions};
use crate::compiler::global_env::GlobalEnv;
use crate::compiler::structs::{ExportedGlobalStructs, GlobalStructs};
use crate::compiler::types::Type;
use crate::compiler::variables::{
  CaptureAddress, ExportedGlobalVariables, GlobalVariables, LocalAddress,
};

pub mod functions;
pub mod imports;
pub mod types;
pub mod variables;

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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Capture {
  Local(LocalAddress),
  Capture(CaptureAddress),
}

struct Function {
  name: String,
  captures: Vec<Capture>,
  return_type: Type,
  code: FunctionCode,
}

impl Function {
  fn get_capture(&self, capture: Capture) -> Option<CaptureAddress> {
    self
      .captures
      .iter()
      .position(|captured_variable| *captured_variable == capture)
      .map(|index| index as u8)
  }

  fn get_or_capture_above_capture(&mut self, above_capture: CaptureAddress) -> CaptureAddress {
    self
      .get_capture(Capture::Capture(above_capture))
      .unwrap_or_else(|| {
        let capture_id = self.captures.len() as u8;
        self.captures.push(Capture::Capture(above_capture));
        capture_id
      })
  }

  fn get_or_capture_above_local(&mut self, above_local: LocalAddress) -> CaptureAddress {
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

pub struct ExportedEnv {
  pub global_variables: ExportedGlobalVariables,
  pub global_structs: ExportedGlobalStructs,
  pub global_functions: ExportedFunctions,
  pub foreign_functions: Vec<ForeignFunction>,
}

pub struct Environment<'src> {
  global_env: &'src GlobalEnv,

  locals: Vec<Local<'src>>,
  scope_depth: u8,
  local_names: u8,
  names_in_current_scope: u8,
  functions_declaration_stack: Vec<Function>,
  declared_functions: u32,

  global_variables: GlobalVariables,
  global_structs: GlobalStructs<'src>,
  global_functions: GlobalFunctions,
}

impl<'src> Environment<'src> {
  pub fn in_global_scope(&self) -> bool {
    self.scope_depth == 0
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

  pub fn push_function(&mut self, name: String, return_type: Type) {
    self.names_in_current_scope = 0;
    self.local_names = 0;
    self.push_scope();
    let prefix = self.generate_location_prefix();
    let debug_name = if prefix.is_empty() {
      name.clone()
    } else {
      format!("{prefix}::{}", &name)
    };
    self.functions_declaration_stack.push(Function {
      name,
      captures: Vec::new(),
      return_type,
      code: FunctionCode::new(debug_name),
    })
  }

  pub fn pop_function(&mut self) -> FinalizedFunction {
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
    let captures = function.captures.iter().copied().collect();
    FinalizedFunction {
      captures,
      code: function.code,
    }
  }

  pub fn ensure_module_name_available(&self, name: &str) -> bool {
    self.global_env.is_module_name_available(name)
  }

  pub fn generate_location_prefix(&self) -> String {
    self
      .functions_declaration_stack
      .iter()
      .map(|f| -> &str { &f.name })
      .collect::<Vec<_>>()
      .join("::")
  }

  pub fn export(self) -> ExportedEnv {
    let (global_functions, foreign_functions) = self.global_functions.export();
    ExportedEnv {
      global_variables: self.global_variables.export(),
      global_structs: self.global_structs.export().unwrap(),
      global_functions,
      foreign_functions,
    }
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

      global_variables: Default::default(),
      global_structs: Default::default(),
      global_functions: Default::default(),
    }
  }
}

#[cfg(test)]
mod test {
  use crate::compiler::global_env::GlobalEnv;
  use crate::compiler::semantics::environment::Capture;
  use crate::compiler::types::Type;

  use super::Environment;

  #[test]
  fn capture_once() {
    let global_env = GlobalEnv::new();
    let mut env = Environment::new(&global_env);
    env.push_function("outer".to_string(), Type::Nothing);
    let var_id = env
      .declare_local_var("x", Type::Bool)
      .expect("could not declare name");
    env.push_function("inner".to_string(), Type::Nothing);
    assert_eq!(env.get_capture_or_capture_var("x"), Some((0, Type::Bool)));
    assert_eq!(env.get_capture_or_capture_var("x"), Some((0, Type::Bool)));
    assert_eq!(env.get_capture_or_capture_var("x"), Some((0, Type::Bool)));
    let function = env.pop_function();
    assert_eq!(function.captures, vec![Capture::Local(var_id)]);
  }

  #[test]
  fn multilevel_capture() {
    let global_env = GlobalEnv::new();
    let mut env = Environment::new(&global_env);
    env.push_function("1".to_string(), Type::Nothing);
    env
      .declare_local_var("x", Type::Num)
      .expect("could not declare global variable");
    env.push_function("2".to_string(), Type::Nothing);
    env.push_function("3".to_string(), Type::Nothing);
    assert_eq!(env.get_capture_or_capture_var("x"), Some((0, Type::Num)));
    let captures = env.pop_function().captures;
    assert_eq!(captures, vec![Capture::Capture(0)]);
    let captures = env.pop_function().captures;
    assert_eq!(captures, vec![Capture::Local(0)]);
  }

  #[test]
  fn no_capture_for_globals() {
    let global_env = GlobalEnv::new();
    let mut env = Environment::new(&global_env);
    env
      .declare_global_var("x", Type::Bool)
      .expect("could not declare name");
    env.push_function("func".to_string(), Type::Nothing);
    assert!(env.get_capture_or_capture_var("x").is_none());
    assert_eq!(env.pop_function().captures, vec![]);
  }

  #[test]
  fn no_capture_for_globals_in_inner_function_scope() {
    let global_env = GlobalEnv::new();
    let mut env = Environment::new(&global_env);
    env
      .declare_global_var("x", Type::Str)
      .expect("could not declare name");
    env.push_function("h".to_string(), Type::Bool);
    env.push_scope();
    assert!(env.get_capture_or_capture_var("x").is_none());
  }
}
