use crate::foreign_function::ForeignFunctionInfo;
use crate::Tgsl;

fn println(value: &str) {
  println!("{}", value.to_string());
}

fn print(value: &str) {
  print!("{}", value.to_string());
}

pub fn load_standard_library(tgsl: &mut Tgsl) {
  tgsl
    .load_module(
      include_str!("../../standard-library/io.tgsl"),
      vec![
        ForeignFunctionInfo::create("print", print),
        ForeignFunctionInfo::create("println", println),
      ],
    )
    .expect("error in loading the standard library");
}
