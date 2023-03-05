use crate::errors::*;

use self::{
  bytecode::Chunk,
  lexer::Lexer,
  parser::{ParseResult, Parser},
  semantic_analysis::SemanticAnalizer,
};

pub mod ast;
pub mod bytecode;
mod codegen;
mod identifier;
pub mod lexer;
pub mod parser;
pub mod semantic_analysis;
mod type_map;
mod types;

pub struct Compiler {}

pub struct CompilerResult {
  pub generated_code: Chunk,
}

impl Compiler {
  pub fn compile(program: &str) -> Result<CompilerResult, SourceError> {
    let parser = Parser::new(Lexer::new(program));
    let ParseResult {
      ast,
      global_types,
      type_map,
    } = parser.parse()?;
    println!("{ast:?}");
    let generated_code = SemanticAnalizer::analyze(ast, global_types, type_map)?;
    Ok(CompilerResult { generated_code })
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
