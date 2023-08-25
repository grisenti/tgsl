use std::collections::HashMap;

use self::{
  codegen::FunctionCode,
  errors::CompilerError,
  global_env::GlobalEnv,
  identifier::{ExternId, GlobalIdentifier, ModuleId},
  parser::Parser,
};

pub mod ast;
pub mod bytecode;
pub mod codegen;
pub mod errors;
mod global_env;
pub mod identifier;
mod lexer;
mod operators;
mod parser;
mod types;

use crate::compiler::codegen::BytecodeGenerator;
use ast::json::ASTJSONPrinter;

pub struct CompiledModule {
  pub module_id: Option<ModuleId>,
  pub globals_count: u16,
  pub extern_functions: HashMap<String, ExternId>,
  pub code: FunctionCode,
}

pub struct Compiler {
  global_env: GlobalEnv,
}

impl Compiler {
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

  pub fn compile(&mut self, source: &str) -> Result<CompiledModule, Vec<CompilerError>> {
    let parsed_module = Parser::parse(source, &self.global_env)?;
    println!("{}", ASTJSONPrinter::print_to_string(&parsed_module.ast));
    let code = BytecodeGenerator::generate_program(&parsed_module.ast);
    println!("{:?}", code);

    let extern_functions = Self::module_extern_functions(&parsed_module.global_names);
    let globals_count = parsed_module.module_global_variable_types.len() as u16;
    let module_id = self.global_env.export_module(parsed_module);
    Ok(CompiledModule {
      code,
      module_id,
      globals_count,
      extern_functions,
    })
  }

  pub fn new() -> Self {
    Self {
      global_env: GlobalEnv::new(),
    }
  }
}
