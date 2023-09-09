use crate::compiler::ast::expression::expr;
use crate::compiler::ast::parsed_type::{ParsedFunctionType, ParsedType};
use crate::compiler::ast::visitor::{ParsedTypeVisitor, StmtVisitor};
use crate::compiler::ast::{ExprHandle, StmtHandle, TypeHandle, AST};
use crate::compiler::codegen::bytecode::ConstantValue;
use crate::compiler::codegen::function_code::FunctionCode;
use crate::compiler::codegen::ModuleCode;
use crate::compiler::errors::{sema_err, CompilerError};
use crate::compiler::global_env::{GlobalEnv, Struct};
use crate::compiler::identifier::{
  FunctionId, GlobalIdentifier, Identifier, ModuleId, StructId, VariableIdentifier,
};
use crate::compiler::lexer::SourceRange;
use crate::compiler::overload_set::OverloadSet;
use crate::compiler::semantics::environment::{
  DeclarationError, Environment, NameError, NameResult, ResolvedIdentifier,
};
use crate::compiler::types::{FunctionSignature, Type};
use std::collections::HashMap;

mod environment;
mod expression_semantics;
mod statement_semantics;

#[derive(PartialEq, Eq)]
pub enum ReturnKind {
  Conditional,
  Unconditional,
  None,
}

fn combine_returns(current: ReturnKind, new: ReturnKind) -> ReturnKind {
  match (current, new) {
    (ReturnKind::None, other) | (other, ReturnKind::None) => other,
    (ReturnKind::Conditional, ReturnKind::Conditional) => ReturnKind::Conditional,
    (ReturnKind::Unconditional, _) | (_, ReturnKind::Unconditional) => ReturnKind::Unconditional,
  }
}

pub struct ModuleExports {
  pub module_name: Option<String>,
  pub global_names: HashMap<String, GlobalIdentifier>,
  pub global_variables_types: Vec<Type>,
  pub extern_function_types: Vec<Type>,
  pub structs: Vec<Option<Struct>>,
  pub overloads: Vec<OverloadSet>,
}

pub struct SemanticChecker<'a> {
  env: Environment<'a>,
  ast: &'a AST<'a>,
  errors: Vec<CompilerError>,
  checked_functions: Vec<FunctionCode>,
  global_code: FunctionCode,
  module_name: Option<String>,
}

impl<'a> SemanticChecker<'a> {
  pub fn check_program(
    ast: &'a AST<'a>,
    global_env: &'a GlobalEnv,
  ) -> Result<(ModuleCode, ModuleExports), Vec<CompilerError>> {
    let mut checker = Self {
      env: Environment::new(global_env),
      ast,
      errors: Vec::new(),
      checked_functions: Vec::new(),
      global_code: FunctionCode::new("<global>".to_string()),
      module_name: None,
    };
    for stmt in ast.get_program() {
      checker.visit_stmt(ast, *stmt);
    }
    assert!(
      checker.env.get_current_function_code().is_none(),
      "some functions have yet to be closed"
    );
    if checker.errors.is_empty() {
      let exported_module = ModuleExports {
        module_name: checker.module_name,
        global_names: checker.env.global_names,
        global_variables_types: checker.env.module_global_variables_types,
        extern_function_types: checker.env.extern_function_types,
        structs: checker.env.module_structs,
        overloads: checker.env.overloads,
      };
      Ok((
        ModuleCode {
          global_code: checker.global_code,
          functions: checker.checked_functions,
        },
        exported_module,
      ))
    } else {
      Err(checker.errors)
    }
  }

  fn emit_error(&mut self, error: CompilerError) {
    self.errors.push(error);
  }

  fn start_function(&mut self, name: &str, signature: FunctionSignature) -> FunctionId {
    self
      .env
      .push_function(name.to_string(), signature.get_return_type().clone());
    self.checked_functions.push(FunctionCode::default());
    match self.env.define_global_function(name, signature) {
      Ok(function_id) => function_id,
      Err(_) => panic!(),
    }
  }

  fn end_function(&mut self, function_id: FunctionId) {
    let index = function_id.get_id() as usize;
    assert!(index < self.checked_functions.len());

    let function = self.env.pop_function();
    self.checked_functions[index] = function.code;
  }

  fn start_lambda(&mut self, return_type: Type) {
    let path = self
      .env
      .get_current_function_code()
      .map(|f| f.get_name())
      .unwrap_or("");
    let name = format!("{path}::<lambda>");
    self.env.push_function(name, return_type);
  }

  fn end_lambda(&mut self) -> (FunctionId, Vec<VariableIdentifier>) {
    let function = self.env.pop_function();
    let function_id = self.env.new_function_id();
    assert_eq!(function_id.get_id() as usize, self.checked_functions.len());
    self.checked_functions.push(function.code);
    (function_id, function.captures)
  }

  fn declare_function(&mut self, name: &str, signature: FunctionSignature) {
    match self.env.declare_global_function(name, signature) {
      Ok(function_id) => {
        let index = function_id.get_id() as usize;
        assert!(index <= self.checked_functions.len());
        if index == self.checked_functions.len() {
          self.checked_functions.push(FunctionCode::default());
        }
      }
      Err(_) => panic!(),
    }
  }

  fn visit_statements(&mut self, statements: &[StmtHandle]) -> ReturnKind {
    let mut return_type = ReturnKind::None;
    for stmt in statements {
      let stmt_return = self.visit_stmt(self.ast, *stmt);
      return_type = combine_returns(return_type, stmt_return);
    }
    return_type
  }

  fn visit_function_body(&mut self, statements: &[StmtHandle], statements_sr: SourceRange) {
    let return_kind = self.visit_statements(statements);
    let return_type = self
      .env
      .get_current_function_return_type()
      .expect("parsing function body in the global scope");
    if !return_type.is_error()
      && *return_type != Type::Nothing
      && return_kind != ReturnKind::Unconditional
    {
      self.emit_error(sema_err::no_unconditional_return(statements_sr));
    }
  }

  fn convert_parameter_types(&mut self, parameter_types: &[TypeHandle]) -> Vec<Type> {
    parameter_types
      .iter()
      .map(|t| self.visit_parsed_type(self.ast, *t))
      .collect::<Vec<_>>()
  }

  fn declare_function_parameters(&mut self, names: &[&'a str], types: &[Type], sr: SourceRange) {
    for (name, type_) in names.iter().zip(types) {
      self.new_variable(name, type_.clone(), sr);
    }
  }

  fn new_variable(&mut self, name: &'a str, type_: Type, sr: SourceRange) -> VariableIdentifier {
    match self.env.define_variable(name, type_) {
      Ok(var_id) => var_id,
      Err(err) => {
        match err {
          DeclarationError::AlreadyDefined => {
            self.emit_error(sema_err::name_already_defined(sr, name))
          }
          DeclarationError::TooManyLocalNames => {
            self.emit_error(sema_err::too_many_local_names(sr))
          }
        }
        VariableIdentifier::Invalid
      }
    }
  }

  fn get_id(&mut self, name: &str, sr: SourceRange) -> ResolvedIdentifier {
    match self.env.get_id(name) {
      Ok(resolved_id) => resolved_id,
      Err(NameError::UndeclaredName) => {
        self.errors.push(sema_err::name_not_found(sr, name));
        ResolvedIdentifier::Error
      }
    }
  }

  fn get_variable(&mut self, name: &str, sr: SourceRange) -> (VariableIdentifier, &Type) {
    match self.env.get_id(name) {
      Ok(ResolvedIdentifier::ResolvedIdentifier {
        id: Identifier::Variable(var_id),
        type_,
      }) => return (var_id, type_),
      Err(NameError::UndeclaredName) => {
        self.errors.push(sema_err::name_not_found(sr, name));
      }
      _ => self.errors.push(sema_err::not_a_variable(sr, name)),
    }

    (VariableIdentifier::Invalid, &Type::Error)
  }

  fn get_struct_id(&mut self, name: &str, sr: SourceRange) -> StructId {
    todo!()
  }

  fn get_struct(&mut self, struct_id: StructId, sr: SourceRange) -> Option<&Struct> {
    todo!()
  }

  unsafe fn generate_identifier_code(&mut self, id: Identifier) {
    match id {
      Identifier::Variable(var_id) => {
        self.code().get_variable(var_id);
      }
      Identifier::ExternFunction(extern_id) => self
        .code()
        .push_constant(ConstantValue::ExternId(extern_id)),
      Identifier::Function(function_id) => self
        .code()
        .push_constant(ConstantValue::FunctionId(function_id)),
      _ => panic!("invalid identifier"),
    }
  }

  fn code(&mut self) -> &mut FunctionCode {
    self
      .env
      .get_current_function_code()
      .unwrap_or(&mut self.global_code)
  }
}

impl ParsedTypeVisitor<Type> for SemanticChecker<'_> {
  fn visit_num(&mut self, ast: &AST) -> Type {
    Type::Num
  }

  fn visit_str(&mut self, ast: &AST) -> Type {
    Type::Str
  }

  fn visit_bool(&mut self, ast: &AST) -> Type {
    Type::Bool
  }

  fn visit_nothing(&mut self, ast: &AST) -> Type {
    Type::Nothing
  }

  fn visit_any(&mut self, ast: &AST) -> Type {
    Type::Any
  }

  fn visit_named(&mut self, ast: &AST, name: &str) -> Type {
    todo!()
  }

  fn visit_function(&mut self, ast: &AST, function: &ParsedFunctionType) -> Type {
    todo!()
  }
}
