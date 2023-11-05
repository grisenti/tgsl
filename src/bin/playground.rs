extern crate core;

use std::str::FromStr;
use std::{fs, io};

use tgsl::standard_library::StandardLibrary;
use tgsl::Tgsl;

fn read_input() -> f64 {
  let mut buf = String::new();
  io::stdin().read_line(&mut buf).expect("");
  buf.pop();
  f64::from_str(&buf).unwrap()
}

struct Context {
  s: String,
}

fn test() -> Result<(), String> {
  let source = fs::read_to_string("src/bin/test_program.tgsl").unwrap();
  let mut tgsl = Tgsl::default();
  tgsl.load_library(StandardLibrary {});
  let mut context = Context {
    s: "hello".to_string(),
  };
  let result = tgsl
    .load_module(&source)
    .bind_function("test", |c: &mut Context, _: f64| println!("{}", c.s))
    .execute(&mut context);
  match result {
    Ok(_) => {}
    Err(err) => println!("{}", err.detailed(&source)),
  }
  Ok(())
}

fn main() {
  if let Err(msg) = test() {
    println!("{}", msg);
  }
}
