use std::rc::Rc;

use crate::compiler::ast::parsed_type::ParsedType;
use crate::compiler::ast::{StmtHandle, TypeHandle, AST};

use crate::compiler::errors::CompilerError;
use crate::compiler::global_env::GlobalEnv;
use crate::compiler::ir::{Capture, Ir, IrStmtHandle};
use crate::compiler::lexer::SourceRange;
use crate::compiler::semantics::environment::{Environment, ExportedEnv};
use crate::compiler::semantics::statement_semantics::CheckStmtSemantics;
use crate::compiler::types::{FunctionSignature, Type};

mod environment;
mod expression_semantics;
mod statement_semantics;

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum ReturnKind {
  Conditional,
  Unconditional,
  None,
}

impl ReturnKind {
  fn to_conditional(self) -> ReturnKind {
    if self == ReturnKind::Unconditional {
      ReturnKind::Conditional
    } else {
      self
    }
  }
}

fn combine_returns(current: ReturnKind, new: ReturnKind) -> ReturnKind {
  match (current, new) {
    (ReturnKind::None, other) | (other, ReturnKind::None) => other,
    (ReturnKind::Conditional, ReturnKind::Conditional) => ReturnKind::Conditional,
    (ReturnKind::Unconditional, _) | (_, ReturnKind::Unconditional) => ReturnKind::Unconditional,
  }
}

pub struct ModuleExports {
  pub module_name: Rc<str>,
  pub exports: ExportedEnv,
}

pub struct CompiledModule {
  pub exports: Option<ModuleExports>,
  pub ir: Ir,
}

pub fn check_program_semantics(
  ast: &AST,
  global_env: &GlobalEnv,
) -> Result<Ir, Vec<CompilerError>> {
  let mut state = SemanticState::new(global_env);
  let mut ir = Ir::default();

  for stmt in ast.get_program() {
    let checked_stmt = stmt.check(ast, &mut ir, &mut state);
    if let Some(handle) = checked_stmt.handle {
      ir.add_startup_instruction(handle);
    }
  }

  if !state.errors.is_empty() {
    return Err(state.errors);
  }
  Ok(ir)
}

struct SemanticState<'env> {
  errors: Vec<CompilerError>,
  source_range_stack: Vec<SourceRange>,
  env: Environment<'env>,
  module_name: Option<Rc<str>>,
  function_count: u32,
  global_var_count: u32,
}

impl<'a> SemanticState<'a> {
  fn new(global_env: &'a GlobalEnv) -> Self {
    Self {
      errors: Vec::new(),
      source_range_stack: Vec::new(),
      env: Environment::new(global_env),
      module_name: None,
      function_count: 0,
      global_var_count: 0,
    }
  }

  fn push_source_range(&mut self, source_range: SourceRange) {
    self.source_range_stack.push(source_range);
  }

  fn top_source_range(&self) -> SourceRange {
    *self.source_range_stack.last().unwrap()
  }

  fn pop_source_range(&mut self) -> SourceRange {
    self.source_range_stack.pop().unwrap()
  }

  /// Creates a new function scope and adds to it all the function's parameters
  fn open_function(
    &mut self,
    name: Rc<str>,
    parameter_names: &[&str],
    parameter_types: &[Type],
    return_type: Type,
  ) -> u32 {
    self.env.start_function_definition(name, return_type);
    for (name, type_) in parameter_names.iter().zip(parameter_types) {
      self
        .env
        .declare_variable(Rc::from(*name), type_.clone())
        .expect("TODO: error checking");
    }
    let function_id = self.function_count;
    self.function_count += 1;
    function_id
  }

  fn close_function(&mut self) -> Vec<Capture> {
    self.env.end_function_definition()
  }

  fn module_name(&self) -> Rc<str> {
    self
      .module_name
      .clone()
      .unwrap_or(Rc::from(Type::ANONYMOUS_MODULE))
  }
}

fn convert_parsed_type(t: TypeHandle, ast: &AST, state: &SemanticState) -> Type {
  match t.get_type(ast) {
    ParsedType::Num => Type::Num,
    ParsedType::Str => Type::Str,
    ParsedType::Bool => Type::Bool,
    ParsedType::Nothing => Type::Nothing,
    ParsedType::Any => Type::Any,
    ParsedType::Named(name) => {
      todo!()
    }
    ParsedType::Function(function) => {
      let parameters = convert_type_list(function.parameters(), ast, state);
      let return_type = convert_parsed_type(function.return_type(), ast, state);
      FunctionSignature::new(parameters, return_type).into()
    }
  }
}

fn convert_type_list(types: &[TypeHandle], ast: &AST, state: &SemanticState) -> Vec<Type> {
  types
    .iter()
    .map(|t| convert_parsed_type(*t, ast, state))
    .collect()
}

struct CheckedBlock {
  return_kind: ReturnKind,
  block_statements: Vec<IrStmtHandle>,
}

fn check_statement_block(
  statements: &[StmtHandle],
  ast: &AST,
  ir: &mut Ir,
  state: &mut SemanticState,
) -> CheckedBlock {
  let mut return_kind = ReturnKind::None;
  let mut block_statements = Vec::with_capacity(statements.len());
  for stmt in statements {
    let checked_stmt = stmt.check(ast, ir, state);
    if let Some(handle) = checked_stmt.handle {
      block_statements.push(handle);
    }
    return_kind = combine_returns(return_kind, checked_stmt.return_kind);
  }
  CheckedBlock {
    return_kind,
    block_statements,
  }
}
