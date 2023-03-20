use std::collections::HashMap;

use crate::errors::*;

use self::{
  lexer::Lexer,
  modules::{LoadedModules, Module},
  parser::{ParseResult, Parser},
  semantic_analysis::SemanticAnalizer,
  types::{type_map::TypeMap, TypeId},
};

pub mod ast;
pub mod bytecode;
mod codegen;
pub mod identifier;
pub mod lexer;
pub mod modules;
pub mod parser;
pub mod semantic_analysis;
mod types;

type CompilerResult<T> = Result<T, SourceError>;

pub struct Compiler {
  type_map: TypeMap,
  global_types: Vec<TypeId>,
}

impl Compiler {
  pub fn compile(&mut self, source: &str, loaded: &LoadedModules) -> CompilerResult<Module> {
    let ParseResult {
      ast,
      module_names,
      module_extern_functions,
      imports,
    } = Parser::parse(
      source,
      &mut self.type_map,
      &loaded.module_names,
      &mut self.global_types,
      &loaded.module_ids,
    )?;
    println!("{:?}", ast);
    let generated_code = SemanticAnalizer::analyze(ast, &mut self.global_types, &self.type_map)?;
    println!("{:?}", generated_code);
    Ok(Module {
      extern_functions: module_extern_functions,
      names: module_names,
      imports,
      code: generated_code,
    })
  }

  pub fn new() -> Self {
    Self {
      global_types: Vec::new(),
      type_map: TypeMap::new(),
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
