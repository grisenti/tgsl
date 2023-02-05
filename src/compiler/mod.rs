use crate::errors::SourceError;

use self::{ast::AST, lexer::Lexer, parser::Parser, semantic_analysis::SemanticAnalizer};

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod semantic_analysis;

pub struct Compiler {}

impl Compiler {
  pub fn compile(program: &str) -> Result<AST, SourceError> {
    let parser = Parser::new(Lexer::new(program));
    let ast = parser.parse()?;
    SemanticAnalizer::analyze(&ast)?;
    Ok(ast)
  }
}
