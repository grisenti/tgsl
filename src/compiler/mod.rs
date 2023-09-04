use crate::compiler::ast::json::ASTJSONPrinter;
use crate::compiler::ast::AST;
use crate::compiler::errors::ErrorPrinter;
use std::collections::HashMap;

use self::{
  errors::CompilerError,
  identifier::{ExternId, GlobalIdentifier},
  parser::Parser,
};

pub mod ast;
pub mod errors;
//mod global_env;
pub mod identifier;
mod lexer;

mod parser;
//mod semantics;
//mod types;

/*pub struct CompiledModule {
  pub module_id: Option<ModuleId>,
  pub globals_count: u16,
  pub extern_functions: HashMap<String, ExternId>,
  pub code: ModuleCode,
}*/

pub struct Compiler {
  //global_env: GlobalEnv,
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

  pub fn compile<'src>(&mut self, source: &'src str) -> Result<AST<'src>, String> {
    match Parser::parse_program(source) {
      Ok(ast) => {
        println!("{}", ASTJSONPrinter::print_to_string(&ast));
        Ok(ast)
      }
      Err(errs) => Err(ErrorPrinter::to_string(&errs, source)),
    }

    /*
    let parsed_module = Parser::parse(source, &self.global_env)?;
    println!("{}", ASTJSONPrinter::print_to_string(&parsed_module.ast));
    let code = ModuleCode::generate_program(&parsed_module.ast);
    println!("{:?}", code);

    let extern_functions = Self::module_extern_functions(&parsed_module.global_names);
    let globals_count = parsed_module.module_global_variable_types.len() as u16;
    let module_id = self.global_env.export_module(parsed_module);
    Ok(CompiledModule {
      code,
      module_id,
      globals_count,
      extern_functions,
    })*/
  }

  pub fn new() -> Self {
    Self {
      //global_env: GlobalEnv::new(),
    }
  }
}
