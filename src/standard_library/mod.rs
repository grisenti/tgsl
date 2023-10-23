use crate::module::Module;
use crate::Tgsl;

fn println(value: &str) {
  println!("{}", value.to_string());
}

fn print(value: &str) {
  print!("{}", value.to_string());
}

pub fn load_standard_library(tgsl: &mut Tgsl) {
  let mut io = Module::new(include_str!("../../standard-library/io.tgsl"));
  io.add_function("print", print)
    .add_function("println", println);
  tgsl
    .load_module(io)
    .expect("error in loading the standard library");
}
