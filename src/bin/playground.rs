use std::fs;

use language::compiler::{errors::ErrorPrinter, Compiler};

fn test() -> Result<(), String> {
  let mut compiler = Compiler::new();
  let source = fs::read_to_string("program.pr").unwrap();
  match compiler.compile(&source) {
    Err(errs) => println!("{}", ErrorPrinter::to_string(&errs, &source)),
    _ => {}
  }
  Ok(())
}

fn main() {
  if let Err(msg) = test() {
    println!("{}", msg);
  }
}
