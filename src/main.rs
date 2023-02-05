mod compiler;
mod errors;
mod interpreter;

use std::fs;

//use ast::codegen::desugar;

use compiler::Compiler;
use errors::SourceError;
use interpreter::*;
/*
fn add(_: &mut Interpreter, args: Vec<ExprValue>) -> InterpreterFnResult {
  match (args[0].clone(), args[1].clone()) {
    (ExprValue::Num(a), ExprValue::Num(b)) => Ok(ExprValue::Num(a + b)),
    _ => Err(()),
  }
}
*/
fn test(program: &str) -> Result<(), SourceError> {
  let ast = Compiler::compile(program)?;
  let mut interpreter = Interpreter::new(ast);
  interpreter.interpret()
}

fn main() {
  let program = fs::read_to_string("program.pr").unwrap();
  if let Err(err) = test(&program) {
    println!("{}", err.print_long(&program));
  }
}
