use crate::library::Library;
use crate::Tgsl;

fn println(value: &str) {
  println!("{}", value.to_string());
}

fn print(value: &str) {
  print!("{}", value.to_string());
}

pub struct StandardLibrary {}

impl Library for StandardLibrary {
  fn load(&mut self, tgls: &mut Tgsl) {
    tgls
      .load_module(include_str!("../../standard-library/io.tgsl"))
      .bind_function("println", println)
      .bind_function("print", print)
      .execute()
      .expect("errors loading the standard library");
  }
}
