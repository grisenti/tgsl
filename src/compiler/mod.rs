use std::collections::HashMap;

use self::{
  ast::AST,
  codegen::BytecodeBuilder,
  errors::CompilerError,
  global_env::{GlobalEnv, GlobalTypes},
  identifier::{ExternId, GlobalIdentifier, ModuleId},
  parser::Parser,
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
mod types;

use ast::json::ASTJSONPrinter;

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

  pub fn compile(&mut self, source: &str) -> Result<AST, Vec<CompilerError>> {
    let parsed_module = Parser::parse(source, &mut self.type_map, &self.global_env)?;
    println!("{}", ASTJSONPrinter::print_to_string(&parsed_module.ast));
    Ok(parsed_module.ast)

    //let extern_functions = Self::module_extern_functions(&parsed_module.global_names);
    //let globals_count = parsed_module.module_global_variable_types.len();
    //let module_id = self.global_env.export_module(parsed_module);
  }

  pub fn new() -> Self {
    Self {
      type_map: TypeMap::new(),
      global_env: GlobalEnv::new(),
    }
  }
}
