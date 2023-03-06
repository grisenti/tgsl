mod compiler;
mod errors;
mod vm;

use std::fs;

use compiler::Compiler;
use errors::SourceError;
use vm::{value::TaggedValue, *};

use crate::vm::value::{Value, ValueType};

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

  use crate::{compiler::Compiler, vm::value::TaggedValue, vm::VM};

  macro_rules! test_file {
    ($name:ident) => {
      #[test]
      #[allow(non_snake_case)]
      fn $name() {
        compile_and_run(&format!(
          "tests/{}.wds",
          stringify!($name).replace("__", "/")
        ));
      }
    };
  }

  fn assert(args: Vec<TaggedValue>) -> TaggedValue {
    assert!(unsafe { args[0].value.boolean });
    TaggedValue::none()
  }

  fn compile_and_run(filename: &str) {
    let source = fs::read_to_string(filename).unwrap();
    let res = Compiler::compile(&source);
    match res {
      Err(err) => println!("{}", err.print_long(&source)),
      Ok(res) => {
        let mut vm = VM::new(res.name_map, res.extern_map);
        println!("{:?}", &res.generated_code);
        vm.bind_function("assert", Box::new(assert));
        vm.interpret(res.generated_code);
      }
    }
  }

  test_file!(closures__capture_single_one_level);
  test_file!(closures__capture_single_multi_level);
  test_file!(closures__capture_copy);

  test_file!(ufc__primitive_types);
  test_file!(ufc__struct);
  test_file!(ufc__nested);

  test_file!(function_declaration);
  test_file!(passing_closures);
  test_file!(primitive_operations);
  test_file!(recursive_function);
  test_file!(struct_declaration);
  test_file!(struct_construction);
  test_file!(struct_member_access);
  test_file!(mutually_recursive_functions);
  test_file!(nested_structs);
  test_file!(conditional_return_types);
  test_file!(if_condition);
}
