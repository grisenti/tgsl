use crate::compiler::ast::expression::expr;
use crate::compiler::ast::parsed_type::ParsedFunctionType;
use crate::compiler::ast::visitor::{ParsedTypeVisitor, StmtVisitor};
use crate::compiler::ast::{ExprHandle, AST};
use crate::compiler::codegen::bytecode::ConstantValue;
use crate::compiler::codegen::function_code::FunctionCode;
use crate::compiler::errors::{sema_err, CompilerError};
use crate::compiler::global_env::{GlobalEnv, Struct};
use crate::compiler::identifier::{Identifier, StructId, VariableIdentifier};
use crate::compiler::lexer::SourceRange;
use crate::compiler::semantics::environment::{DeclarationError, Environment, ResolvedIdentifier};
use crate::compiler::types::Type;

mod environment;
mod expression_semantics;
mod statement_semantics;

pub struct SemanticChecker<'a> {
  env: Environment<'a>,
  ast: &'a AST<'a>,
  errors: Vec<CompilerError>,
  checked_functions: Vec<FunctionCode>,
  current_function_stack: Vec<FunctionCode>,
  global_code: FunctionCode,
}

impl<'a> SemanticChecker<'a> {
  pub fn check_program(
    ast: &'a AST<'a>,
    global_env: &'a GlobalEnv,
  ) -> Result<(), Vec<CompilerError>> {
    let mut checker = Self {
      env: Environment::new(global_env),
      ast,
      errors: Vec::new(),
      checked_functions: Vec::new(),
      current_function_stack: Vec::new(),
      global_code: FunctionCode::new("<global>".to_string()),
    };
    for stmt in ast.get_program() {
      checker.visit_stmt(ast, *stmt);
    }
    if checker.errors.is_empty() {
      Ok(())
    } else {
      Err(checker.errors)
    }
  }

  fn emit_error(&mut self, error: CompilerError) {
    self.errors.push(error);
  }

  fn start_function(&mut self, name: &str) {}

  fn end_function(&mut self) {}

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
    todo!()
  }

  fn get_variable(&mut self, name: &str, sr: SourceRange) -> (VariableIdentifier, &Type) {
    todo!()
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
      .current_function_stack
      .last_mut()
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
