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

fn add(_: &mut Interpreter, args: Vec<ExprValue>) -> InterpreterFnResult {
  match (args[0].clone(), args[1].clone()) {
    (ExprValue::Num(a), ExprValue::Num(b)) => Ok(ExprValue::Num(a + b)),
    _ => Err(()),
  }
}

fn main() -> Result<(), SourceError> {
  let program = fs::read_to_string("program.pr").unwrap();
  let mut parser = Parser::new(Lexer::new(&program));
  let mut interpreter = Interpreter::new();
  let ast = parser.parse()?;
  print!("{}", desugar(&ast));
  interpreter.add_native_function(
    "add",
    InterpreterFn {
      arity: 2,
      name: "add".to_string(),
      callable: Box::new(add),
    },
  );
  interpreter.interpret(ast)?;
  Ok(())
}
