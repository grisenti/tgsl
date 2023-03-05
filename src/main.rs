mod compiler;
mod errors;
mod vm;

use std::fs;

//use ast::codegen::desugar;

use compiler::Compiler;
use errors::SourceError;
use vm::*;

use crate::compiler::bytecode::{TaggedValue, Value, ValueType};

fn sum(values: Vec<TaggedValue>) -> TaggedValue {
  println!("{values:?}");
  let first = values[0];
  let second = values[1];
  TaggedValue {
    kind: ValueType::Number,
    value: Value {
      number: unsafe { first.value.number + second.value.number },
    },
  }
}

fn test(program: &str) -> Result<(), SourceError> {
  let program = Compiler::compile(program)?;
  let mut vm = VM::new(program.name_map, program.extern_map);
  vm.bind_function("", Box::new(sum));
  println!("{:?}", program.generated_code);
  vm.interpret(program.generated_code);
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
  use std::fs;

  use crate::{
    compiler::{bytecode::TaggedValue, Compiler},
    vm::VM,
  };

  macro_rules! test_file {
    ($name:ident) => {
      #[test]
      fn $name() {
        compile_and_run(&format!("tests/{}.wds", stringify!($name)));
      }
    };
  }

  fn assert(args: Vec<TaggedValue>) -> TaggedValue {
    assert!(unsafe { args[0].value.boolean });
    TaggedValue::none()
  }

  fn compile_and_run(filename: &str) {
    let res = Compiler::compile(&fs::read_to_string(filename).unwrap()).unwrap();
    let mut interpreter = VM::new(res.name_map, res.extern_map);
    interpreter.bind_function("assert", Box::new(assert));
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
