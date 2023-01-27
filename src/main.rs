mod ast;
mod errors;
mod interpreter;
mod lexer;
mod parser;

use std::{
  fs,
  process::{ExitCode, Termination},
};

use ast::codegen::desugar;
use errors::SourceError;
use interpreter::*;
use lexer::*;
use parser::*;

fn main() -> Result<(), SourceError> {
  let program = fs::read_to_string("program.pr").unwrap();
  let mut parser = Parser::new(Lexer::new(&program));
  let mut interpreter = Interpreter::new();
  let ast = parser.parse()?;
  print!("{}", desugar(&ast));
  interpreter.interpret(ast)?;
  Ok(())
}
