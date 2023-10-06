use std::rc::Rc;

use crate::compiler::ast::parsed_type::ParsedFunctionType;
use crate::compiler::ast::visitor::{ExprVisitor, ParsedTypeVisitor, StmtVisitor};
use crate::compiler::ast::{ExprHandle, StmtHandle, TypeHandle, AST};
use crate::compiler::codegen::bytecode::OpCode;
use crate::compiler::codegen::function_code::FunctionCode;
use crate::compiler::codegen::ModuleCode;
use crate::compiler::errors::{sema_err, ty_err, CompilerError};
use crate::compiler::functions::ExportedFunctions;
use crate::compiler::global_env::GlobalEnv;
use crate::compiler::lexer::SourceRange;
use crate::compiler::semantics::environment::{DeclarationError, DeclarationResult, Environment};
use crate::compiler::structs::ExportedGlobalStructs;
use crate::compiler::types::{FunctionSignature, Type};
use crate::compiler::variables::ExportedGlobalVariables;
use crate::compiler::ExternFunction;

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
  pub module_name: String,
  pub global_variables: ExportedGlobalVariables,
  pub structs: ExportedGlobalStructs,
  pub functions: ExportedFunctions,
}

pub struct CompiledModule {
  pub exports: Option<ModuleExports>,
  pub extern_functions: Vec<ExternFunction>,
  pub globals_count: u32,
  pub module_code: ModuleCode,
}

pub struct SemanticChecker<'a> {
  env: Environment<'a>,
  ast: &'a AST<'a>,
  errors: Vec<CompilerError>,
  checked_functions: Vec<FunctionCode>,
  global_code: FunctionCode,
  module_name: Option<Rc<str>>,
}

impl<'a> SemanticChecker<'a> {
  pub fn check_program(
    ast: &'a AST<'a>,
    global_env: &'a GlobalEnv,
  ) -> Result<CompiledModule, Vec<CompilerError>> {
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
      unsafe { checker.global_code.push_op(OpCode::Return) };
      let exported_env = checker.env.export();
      let global_variable_count = exported_env.global_variables.count();
      let exports = checker.module_name.map(|name| ModuleExports {
        module_name: name.to_string(),
        structs: exported_env.global_structs,
        global_variables: exported_env.global_variables,
        functions: exported_env.global_functions,
      });
      Ok(CompiledModule {
        exports,
        module_code: ModuleCode {
          global_code: checker.global_code,
          functions: checker.checked_functions,
        },
        globals_count: global_variable_count,
        extern_functions: exported_env.extern_functions,
      })
    } else {
      Err(checker.errors)
    }
  }

  fn emit_error(&mut self, error: CompilerError) {
    self.errors.push(error);
  }

  fn check_declaration<T>(
    &mut self,
    name: &str,
    decl: DeclarationResult<T>,
    stmt_sr: SourceRange,
  ) -> Option<T> {
    match decl {
      Ok(value) => Some(value),
      Err(DeclarationError::AlreadyDefined) => {
        self.emit_error(sema_err::name_already_defined(stmt_sr, name));
        None
      }
      Err(DeclarationError::TooManyLocalNames) => {
        assert!(!self.env.in_global_scope());
        self.emit_error(sema_err::too_many_local_names(stmt_sr));
        None
      }
    }
  }

  fn finalize_function_code(&mut self) {
    if *self.env.get_current_function_return_type().unwrap() == Type::Nothing {
      unsafe {
        self.code().push_constant_none();
        self.code().push_op(OpCode::Return);
      }
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

  fn convert_type_list(&mut self, parameter_types: &[TypeHandle]) -> Vec<Type> {
    parameter_types
      .iter()
      .map(|t| self.visit_parsed_type(self.ast, *t))
      .collect::<Vec<_>>()
  }

  fn visit_expr_list(&mut self, ast: &'a AST<'a>, expr_list: &[ExprHandle]) -> Vec<Type> {
    expr_list.iter().map(|e| self.visit_expr(ast, *e)).collect()
  }

  fn declare_local_var(&mut self, name: &'a str, var_type: Type, stmt_sr: SourceRange) {
    if let Err(err) = self.env.declare_local_var(name, var_type) {
      todo!()
    }
  }

  fn declare_function_parameters(&mut self, names: &[&'a str], types: &[Type], sr: SourceRange) {
    for (name, type_) in names.iter().zip(types) {
      self.declare_local_var(name, type_.clone(), sr);
    }
  }

  fn check_condition(&mut self, condition_type: Type, sr: SourceRange) -> bool {
    if condition_type.is_error() {
      false
    } else if condition_type != Type::Bool {
      self.emit_error(ty_err::incorrect_conditional_type(sr, &condition_type));
      false
    } else {
      true
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
  fn visit_num(&mut self, _ast: &AST) -> Type {
    Type::Num
  }

  fn visit_str(&mut self, _ast: &AST) -> Type {
    Type::Str
  }

  fn visit_bool(&mut self, _ast: &AST) -> Type {
    Type::Bool
  }

  fn visit_nothing(&mut self, _ast: &AST) -> Type {
    Type::Nothing
  }

  fn visit_any(&mut self, _ast: &AST) -> Type {
    Type::Any
  }

  fn visit_named(&mut self, _ast: &AST, name: &str) -> Type {
    Type::Struct {
      name: Rc::from(name),
      module_name: self
        .module_name
        .clone()
        .unwrap_or(Rc::from(Type::ANONYMOUS_MODULE)),
    }
  }

  fn visit_function(&mut self, ast: &AST, function: &ParsedFunctionType) -> Type {
    let parameters = self.convert_type_list(function.parameters());
    let return_type = self.visit_parsed_type(ast, function.return_type());
    FunctionSignature::new(parameters, return_type).into()
  }
}
