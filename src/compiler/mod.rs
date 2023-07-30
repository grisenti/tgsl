use std::collections::HashMap;

use self::{
  codegen::BytecodeBuilder,
  errors::CompilerError,
  global_env::{GlobalEnv, GlobalTypes},
  identifier::{ExternId, GlobalIdentifier, ModuleId},
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
    let mut parsed_module = Parser::parse(source, &mut self.type_map, &self.global_env)?;
    let generated_code = SemanticAnalizer::analyze(
      &parsed_module.ast,
      GlobalTypes::new(&self.global_env),
      &mut parsed_module.module_global_variable_types,
      &mut parsed_module.module_extern_functions_types,
      &mut parsed_module.module_struct_types,
      &self.type_map,
    )?;
    let extern_functions = Self::module_extern_functions(&parsed_module.global_names);
    let globals_count = parsed_module.module_global_variable_types.len();
    let module_id = self.global_env.export_module(parsed_module);
    println!("{:?}", &generated_code);
    Ok(CompiledModule {
      module_id,
      globals_count: globals_count as u16,
      code: generated_code,
      extern_functions,
    })
  }

  pub fn new() -> Self {
    Self {
      type_map: TypeMap::new(),
      global_env: GlobalEnv::new(),
    }
  }
}
