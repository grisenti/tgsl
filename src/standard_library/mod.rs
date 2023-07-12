use crate::vm::{value::TaggedValue, VM};

fn println(args: Vec<TaggedValue>) -> TaggedValue {
  println!("{}", args[0].to_string());
  TaggedValue::none()
}

fn print(args: Vec<TaggedValue>) -> TaggedValue {
  print!("{}", args[0].to_string());
  TaggedValue::none()
}

pub fn load_standard_library(vm: &mut VM) {
  vm.load_module(
    include_str!("../../standard-library/io.wds"),
    vec![("println", Box::new(println)), ("print", Box::new(print))],
  )
  .unwrap();
}
