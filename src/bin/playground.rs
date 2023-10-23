extern crate core;

use std::str::FromStr;
use std::{fs, io};

use tgsl::module::Module;
use tgsl::Tgsl;

fn read_input() -> f64 {
  let mut buf = String::new();
  io::stdin().read_line(&mut buf).expect("");
  buf.pop();
  f64::from_str(&buf).unwrap()
}

fn test() -> Result<(), String> {
  let source = fs::read_to_string("src/bin/test_program.tgsl").unwrap();
  let mut tgsl = Tgsl::default();
  let module = Module::new(&source);
  tgsl.load_module(module)?;
  Ok(())
}

fn main() {
  if let Err(msg) = test() {
    println!("{}", msg);
  }
}
