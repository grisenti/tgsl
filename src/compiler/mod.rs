use crate::compiler::ast::json::ASTJSONPrinter;

use crate::compiler::codegen::ModuleCode;

use crate::compiler::global_env::GlobalEnv;
use crate::compiler::identifier::ModuleId;
use crate::compiler::semantics::SemanticChecker;
use std::collections::HashMap;

use self::{
  errors::CompilerError,
  identifier::{ExternId, GlobalIdentifier},
  parser::Parser,
};

pub mod ast;
pub mod codegen;
pub mod errors;
mod global_env;
pub mod identifier;
mod lexer;
mod operators;
mod overload_set;
mod parser;
mod semantics;
pub mod types;

pub struct CompiledModule {
  pub module_id: Option<ModuleId>,
  pub globals_count: u16,
  pub extern_functions: HashMap<String, ExternId>,
  pub code: ModuleCode,
}

pub fn module_extern_functions(
  names: &HashMap<String, GlobalIdentifier>,
) -> HashMap<String, ExternId> {
  names
    .iter()
    .filter_map(|(name, id)| {
      if let GlobalIdentifier::ExternFunction(id) = id {
        if id.is_relative() {
          Some((name.clone(), *id))
        } else {
          None
        }
      } else {
        None
      }
    })
    .collect()
}

pub struct Compiler {
  global_env: GlobalEnv,
}

impl Compiler {
  pub fn compile(&mut self, source: &str) -> Result<CompiledModule, Vec<CompilerError>> {
    let ast = Parser::parse_program(source)?;
    println!("{}", ASTJSONPrinter::print_to_string(&ast));
    let (code, exports) = SemanticChecker::check_program(&ast, &self.global_env)?;
    println!("{:?}", &code);
    let globals_count = exports.global_variables_types.len() as u16;
    let extern_functions = module_extern_functions(&exports.global_names);
    let module_id = self.global_env.export_module(exports);

    Ok(CompiledModule {
      code,
      module_id,
      extern_functions,
      globals_count,
    })
  }

  pub fn new() -> Self {
    Self {
      global_env: GlobalEnv::new(),
    }
  }
}
