mod ast;
mod errors;
mod interpreter;
mod lexer;
mod parser;

use std::fs;

use ast::codegen::desugar;
use interpreter::*;
use lexer::*;
use parser::*;

fn main() {
  let program = fs::read_to_string("program.pr").unwrap();
  let mut parser = Parser::new(Lexer::new(&program));
  let mut interpreter = Interpreter::new();
  match parser.parse() {
    Ok(ast) => {
      print!("{}", desugar(&ast));
      if let Err(errs) = interpreter.interpret(ast) {
        print!("{}", errs);
      }
    }
    Err(errs) => {
      print!("{}", errs);
    }
  }
}
