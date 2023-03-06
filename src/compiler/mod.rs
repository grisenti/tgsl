use std::collections::HashMap;

use crate::errors::*;

use self::{
  bytecode::Chunk,
  identifier::{ExternId, Identifier},
  lexer::Lexer,
  parser::{ParseResult, Parser},
  semantic_analysis::SemanticAnalizer,
};

pub mod ast;
pub mod bytecode;
mod codegen;
pub mod identifier;
pub mod lexer;
pub mod parser;
pub mod semantic_analysis;
mod types;

pub struct Compiler {}

pub struct CompilerResult {
  pub generated_code: Chunk,
  pub name_map: HashMap<String, Identifier>,
  pub extern_map: HashMap<Identifier, ExternId>,
}

impl Compiler {
  pub fn compile(program: &str) -> Result<CompilerResult, SourceError> {
    let parser = Parser::new(Lexer::new(program));
    let ParseResult {
      ast,
      global_types,
      type_map,
      extern_map,
      name_map,
    } = parser.parse()?;
    println!("{:?}", &ast);
    let generated_code = SemanticAnalizer::analyze(ast, global_types, type_map)?;
    Ok(CompilerResult {
      generated_code,
      name_map,
      extern_map,
    })
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
