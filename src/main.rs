mod compiler;
mod errors;
mod vm;

use std::fs;

//use ast::codegen::desugar;

use compiler::Compiler;
use errors::SourceError;
use vm::*;

fn test(program: &str) -> Result<(), SourceError> {
  let ast = Compiler::compile(program)?;
  let mut vm = VM::new();
  println!("{:?}", ast.generated_code);
  vm.interpret(ast.generated_code);
  Ok(())
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

  macro_rules! test_file {
    ($name:ident) => {
      #[test]
      fn $name() {
        compile_and_run(&format!("tests/{}.wds", stringify!($name)));
      }
    };
  }

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

  fn compile_and_run(filename: &str) {
    let res = Compiler::compile(&fs::read_to_string(filename).unwrap()).unwrap();
    let mut interpreter = Interpreter::new(res);
    interpreter.add_global_variable(
      "assert",
      ExprValue::Func(Rc::new(InterpreterFn::foreign(assert, 1))),
    );
    interpreter.interpret().unwrap();
  }

  test_file!(closure_capture);
  test_file!(function_declaration);
  test_file!(passing_closures);
  test_file!(primitive_operations);
  test_file!(ufc_for_primitive_types);
  test_file!(recursive_function);
  test_file!(struct_declaration);
  test_file!(struct_construction);
  test_file!(struct_member_access);
  test_file!(ufc_for_struct);
  test_file!(mutually_recursive_functions);
  test_file!(nested_structs);
  test_file!(conditional_return_types);
  test_file!(if_condition);
}
