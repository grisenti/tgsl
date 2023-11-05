use crate::compiler::codegen::ModuleCode;
use crate::compiler::functions::ForeignFunction;
use crate::compiler::global_env::GlobalEnv;
use crate::compiler::semantics::SemanticChecker;

use self::{errors::CompilerError, parser::Parser};

pub mod ast;
pub mod codegen;
pub mod errors;
pub mod functions;
mod global_env;
mod lexer;
mod parser;
mod semantics;
mod structs;
pub mod types;
mod variables;

pub struct CompiledModule {
  pub foreign_functions: Vec<ForeignFunction>,
  pub code: ModuleCode,
}

#[derive(Default)]
pub struct Compiler {
  global_env: GlobalEnv,
}

impl Compiler {
  pub fn compile(&mut self, source: &str) -> Result<CompiledModule, Vec<CompilerError>> {
    let ast = Parser::parse_program(source)?;
    //println!("{}", ASTJSONPrinter::print_to_string(&ast));
    let module = SemanticChecker::check_program(&ast, &self.global_env)?;
    //println!("{:?}", &module.module_code);
    if let Some(exports) = module.exports {
      self.global_env.export_module(exports);
    }
    Ok(CompiledModule {
      code: module.module_code,
      foreign_functions: module.foreign_functions,
    })
  }

  pub fn new() -> Self {
    Self {
      global_env: GlobalEnv::new(),
    }
  }
}
