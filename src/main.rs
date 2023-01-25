mod ast;
mod errors;
mod interpreter;
mod lexer;
mod parser;

use std::fs;

use interpreter::*;
use lexer::*;
use parser::*;

fn main() {
  let program = fs::read_to_string("program.pr").unwrap();
  let mut parser = Parser::new(Lexer::new(&program));
  let mut interpreter = Interpreter::new();
  match parser.parse() {
    Ok(ast) => match interpreter.interpret(ast) {
      Err(err) => print!("{}", err),
      _ => {}
    },
    Err(errs) => {
      print!("{}", errs);
    }
  }
}
