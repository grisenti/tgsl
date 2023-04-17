use std::collections::HashMap;

use crate::errors::*;

use self::{
  global_env::GlobalEnv,
  identifier::{GlobalId, ModuleId},
  lexer::Lexer,
  modules::{LoadedModules, Module},
  parser::{ParseResult, Parser},
  semantic_analysis::SemanticAnalizer,
  types::type_map::TypeMap,
};

pub mod ast;
pub mod bytecode;
pub mod codegen;
mod global_env;
pub mod identifier;
mod lexer;
pub mod modules;
mod parser;
mod semantic_analysis;
mod types;

type CompilerResult<T> = Result<T, SourceError>;

pub struct Compiler {
  type_map: TypeMap,
  global_env: GlobalEnv,
  last_module: u16,
}

impl Compiler {
  pub fn compile(&mut self, source: &str, loaded: &LoadedModules) -> CompilerResult<Module> {
    let ParseResult {
      ast,
      module_extern_functions,
      imports,
    } = Parser::parse(
      source,
      &mut self.type_map,
      &mut self.global_env,
      &loaded.module_ids,
    )?;
    let generated_code =
      SemanticAnalizer::analyze(ast, &mut self.global_env.types, &self.type_map)?;
    let id = ModuleId(self.last_module);
    let global_identifiers = self.global_env.get_globals_count(id);
    let ret = Ok(Module {
      id,
      extern_functions: module_extern_functions,
      imports,
      code: generated_code,
      global_identifiers,
    });
    self.last_module += 1;
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

fn error_from_lexer_state(lex: &Lexer, error_msg: String) -> SourceError {
  SourceError::new(
    lex.line_no(),
    lex.prev_token_start(),
    lex.prev_token_end(),
    error_msg,
    SourceErrorType::Compilation,
  )
}

fn error_from_source_info(info: &SourceInfo, error_msg: String) -> SourceError {
  SourceError::from_source_info(info, error_msg, SourceErrorType::Compilation)
}
