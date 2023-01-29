mod ast;
mod errors;
mod interpreter;
mod lexer;
mod parser;

use std::fs;

use ast::codegen::desugar;
use errors::SourceError;
use interpreter::*;
use lexer::*;
use parser::*;
/*
fn add(_: &mut Interpreter, args: Vec<ExprValue>) -> InterpreterFnResult {
  match (args[0].clone(), args[1].clone()) {
    (ExprValue::Num(a), ExprValue::Num(b)) => Ok(ExprValue::Num(a + b)),
    _ => Err(()),
  }
}
*/
fn test(program: &String) -> Result<(), SourceError> {
  let parser = Parser::new(Lexer::new(program));
  let ast = parser.parse()?;
  print!("{}", desugar(&ast));
  let mut int = Interpreter::new();
  int.interpret(&ast)?;
  Ok(())
}

fn main() {
  let program = fs::read_to_string("program.pr").unwrap();
  if let Err(err) = test(&program) {
    println!("{}", err.print_long(&program));
  }
}
