mod compiler;
mod errors;
mod id_hasher;
mod standard_library;
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
  use crate::{vm::value::TaggedValue, vm::VM};

  macro_rules! test_files {
    ($module:ident, $($test:ident),+) => {
      mod $module {
				use super::compile_and_run;
      $(
        #[test]
        #[allow(non_snake_case)]
        fn $test() {
          compile_and_run(include_str!(
            concat!("../tests/", stringify!($module), "/", stringify!($test), ".wds")
          ));
        }
          )+
      }
    };
  }

  fn assert(args: Vec<TaggedValue>) -> TaggedValue {
    assert!(unsafe { args[0].value.boolean });
    TaggedValue::none()
  }

  fn compile_and_run(test_file: &str) {
    let mut vm = VM::new();
    if let Err(msg) = vm.load_module(
      "test".to_string(),
      test_file,
      vec![("assert", Box::new(assert))],
    ) {
      println!("{}", msg);
    }
  }

  test_files!(closures, capture_single_one_level, capture_copy);
  test_files!(ufc, primitive_types, aggregates, nested);
  test_files!(
    misc,
    function_declaration,
    passing_closures,
    primitive_operations,
    recursive_function,
    struct_declaration,
    struct_construction,
    struct_member_access,
    mutually_recursive_functions,
    nested_structs,
    conditional_return_types,
    if_condition
  );
}
