use crate::vm::extern_function::ExternFunction;
use crate::vm::{value::TaggedValue, VM};

fn println(value: TaggedValue) {
  println!("{}", value.to_string());
}

fn print(value: TaggedValue) {
  print!("{}", value.to_string());
}

pub fn load_standard_library(vm: &mut VM) {
  vm.load_module(
    include_str!("../../standard-library/io.wds"),
    vec![
      ExternFunction::create("print", print),
      ExternFunction::create("println", println),
    ],
  )
  .unwrap();
}
