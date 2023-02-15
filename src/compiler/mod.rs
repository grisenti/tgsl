use std::collections::HashMap;

use crate::errors::*;

use self::{
  ast::{Identifier, AST},
  lexer::Lexer,
  parser::{ParseResult, Parser},
  semantic_analysis::SemanticAnalizer,
};

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod semantic_analysis;

pub struct Compiler {}

pub struct CompilerResult {
  pub ast: AST,
  pub global_environment: HashMap<String, Identifier>,
}

impl Compiler {
  pub fn compile(program: &str) -> Result<CompilerResult, SourceError> {
    let parser = Parser::new(Lexer::new(program));
    let ParseResult { ast, final_env } = parser.parse()?;
    SemanticAnalizer::analyze(&ast, final_env.type_map)?;
    Ok(CompilerResult {
      ast,
      global_environment: final_env.global_scope,
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
