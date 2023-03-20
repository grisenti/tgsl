mod compiler;
mod errors;
mod id_hasher;
mod vm;

use std::fs;

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

fn test() -> Result<(), String> {
  let mut vm = VM::new();
  let p1 = fs::read_to_string("program.pr").unwrap();
  let p2 = fs::read_to_string("prog2.pr").unwrap();
  vm.load_module("hello".to_string(), &p1, vec![])?;
  vm.load_module("test".to_string(), &p2, vec![])?;
  Ok(())
}

fn main() {
  if let Err(msg) = test() {
    println!("{}", msg);
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
    let mut vm = VM::new();
    if let Err(msg) = vm.load_module(
      "test".to_string(),
      &source,
      vec![("assert", Box::new(assert))],
    ) {
      println!("{}", msg);
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
