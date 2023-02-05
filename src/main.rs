mod compiler;
mod errors;
mod interpreter;

use std::{fs, rc::Rc};

//use ast::codegen::desugar;

use compiler::Compiler;
use errors::{SourceError, SourceInfo};
use interpreter::*;

fn add(_: &mut Interpreter, args: Vec<ExprValue>) -> InterpreterFnResult {
  match (args[0].clone(), args[1].clone()) {
    (ExprValue::Num(a), ExprValue::Num(b)) => Ok(ExprValue::Num(a + b)),
    _ => Err(SourceError::from_source_info(
      &SourceInfo::temporary(),
      String::new(),
      errors::SourceErrorType::Runtime,
    )),
  }
}

fn test(program: &str) -> Result<(), SourceError> {
  let ast = Compiler::compile(program)?;
  let mut interpreter = Interpreter::new(ast);
  interpreter.add_global_variable(
    "add",
    ExprValue::Func(Rc::new(InterpreterFn::foreign(add, 2))),
  );
  interpreter.interpret()
}

fn main() {
  let program = fs::read_to_string("program.pr").unwrap();
  if let Err(err) = test(&program) {
    println!("{}", err.print_long(&program));
  }
}

#[cfg(test)]
mod test {
  use std::{fs, rc::Rc};

  use crate::{
    compiler::Compiler,
    errors::{self, SourceError, SourceInfo},
    interpreter::*,
  };

  fn assert(_: &mut Interpreter, args: Vec<ExprValue>) -> InterpreterFnResult {
    if let ExprValue::Boolean(true) = args[0] {
      Ok(ExprValue::Null)
    } else {
      Err(SourceError::from_source_info(
        &SourceInfo::temporary(),
        "failed assertion".to_string(),
        errors::SourceErrorType::Runtime,
      ))
    }
  }

  #[test]
  fn compile_and_run() {
    for entry in fs::read_dir("tests/").unwrap() {
      let path = entry.unwrap().path();
      if path.is_file() {
        let res = Compiler::compile(&fs::read_to_string(path).unwrap()).unwrap();
        let mut interpreter = Interpreter::new(res);
        interpreter.add_global_variable(
          "assert",
          ExprValue::Func(Rc::new(InterpreterFn::foreign(assert, 1))),
        );
        interpreter.interpret().unwrap();
      }
    }
  }
}
