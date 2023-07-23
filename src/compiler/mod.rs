use std::collections::HashMap;

use self::{
  codegen::BytecodeBuilder,
  errors::CompilerError,
  global_env::{GlobalEnv, GlobalTypes},
  identifier::{ExternId, GlobalVarId, ModuleId},
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
mod parser;
mod semantic_analysis;
mod types;

pub struct CompiledModule {
  pub module_id: Option<ModuleId>,
  pub globals_count: u16,
  pub extern_functions: HashMap<String, ExternId>,
  pub code: BytecodeBuilder,
}

pub struct Compiler {
  type_map: TypeMap,
  global_env: GlobalEnv,
}

impl Compiler {
  pub fn compile(&mut self, source: &str) -> Result<CompiledModule, Vec<CompilerError>> {
    let mut parsed_module = Parser::parse(source, &mut self.type_map, &self.global_env)?;
    let generated_code = SemanticAnalizer::analyze(
      &parsed_module.ast,
      GlobalTypes::new(&self.global_env),
      &mut parsed_module.module_global_types,
      &mut parsed_module.module_extern_functions_types,
      &self.type_map,
    )?;
    println!("{:?}", parsed_module.ast);
    let globals_count = parsed_module.globals_count;
    let extern_functions = parsed_module
      .extern_functions
      .iter()
      .filter(|(_, id)| id.is_relative())
      .map(|(name, id)| (name.clone(), *id))
      .collect::<HashMap<_, _>>();
    let module_id = self.global_env.export_module(parsed_module);
    Ok(CompiledModule {
      module_id,
      globals_count,
      code: generated_code,
      extern_functions,
    })
  }

  pub fn get_global(&self, module_id: ModuleId, name: &str) -> Option<GlobalVarId> {
    self.global_env.get_id(module_id, name)
  }

  pub fn new() -> Self {
    Self {
      type_map: TypeMap::new(),
      global_env: GlobalEnv::new(),
    }
  }
}
