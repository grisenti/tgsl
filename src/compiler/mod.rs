use std::collections::HashMap;

use self::{
  errors::CompilerError,
  global_env::{GlobalEnv, GlobalTypes},
  identifier::{GlobalId, ModuleId},
  modules::Module,
  parser::Parser,
  semantic_analysis::SemanticAnalizer,
  types::type_map::TypeMap,
};

pub mod ast;
pub mod bytecode;
pub mod codegen;
pub mod errors;
mod global_env;
pub mod identifier;
mod lexer;
pub mod modules;
mod parser;
mod semantic_analysis;
mod types;

pub struct Compiler {
  type_map: TypeMap,
  global_env: GlobalEnv,
  last_module: u16,
}

impl Compiler {
  pub fn compile(&mut self, source: &str) -> Result<Module, Vec<CompilerError>> {
    let mut parsed_module = Parser::parse(source, &mut self.type_map, &self.global_env)?;
    let generated_code = SemanticAnalizer::analyze(
      &parsed_module.ast,
      GlobalTypes::new(&self.global_env),
      &mut parsed_module.module_global_types,
      &self.type_map,
    )?;
    let id = ModuleId(self.last_module);
    let globals_count = parsed_module.globals_count;
    let extern_functions = parsed_module.module_extern_functions.clone();
    self.global_env.export_module(parsed_module);
    let ret = Ok(Module {
      id,
      extern_functions,
      imports: vec![],
      code: generated_code,
      globals_count,
    });
    ret
  }

  pub fn get_global(&self, module_id: ModuleId, name: &str) -> Option<GlobalId> {
    self.global_env.get_id(module_id, name)
  }

  pub fn new() -> Self {
    Self {
      type_map: TypeMap::new(),
      global_env: GlobalEnv::new(),
      last_module: 0,
    }
  }
}
