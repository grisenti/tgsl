use crate::compiler::codegen::ModuleProgram;
use crate::compiler::functions::ForeignFunction;
use crate::compiler::global_env::GlobalEnv;
use crate::compiler::semantics::check_program_semantics;

use self::{errors::CompilerError, parser::Parser};

pub mod ast;
pub mod codegen;
mod debug_tree;
pub mod errors;
pub mod functions;
mod global_env;
mod global_symbols;
mod ir;
mod lexer;
mod parser;
mod semantics;
mod structs;
pub mod types;

pub struct CompiledModule {
  pub foreign_functions: Vec<ForeignFunction>,
  pub program: ModuleProgram,
}

#[derive(Default)]
pub struct Compiler {
  global_env: GlobalEnv,
}

impl Compiler {
  pub fn compile(&mut self, source: &str) -> Result<(), Vec<CompilerError>> {
    let ast = Parser::parse_program(source)?;
    let ir = check_program_semantics(&ast, &self.global_env)?;
    println!("{}", ir.to_debug_tree().to_string(0));
    Ok(())
  }

  pub fn new() -> Self {
    Self {
      global_env: GlobalEnv::new(),
    }
  }
}
