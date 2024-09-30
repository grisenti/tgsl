extern crate core;

use std::fs;

use tgsl::errors::UserError;
use tgsl::standard_library::StandardLibrary;
use tgsl::Tgsl;

struct Context {
  s: String,
}

fn test() -> Result<(), String> {
  let source = fs::read_to_string("src/bin/test_program.tgsl").unwrap();
  let mut tgsl = Tgsl::default();
  //tgsl.load_library(StandardLibrary {});
  let result = tgsl
    .load_module(&source)
    .bind_function(
      "test",
      |context: &mut Context| -> Result<(), Box<dyn UserError>> { Err(Box::new("test error")) },
    )
    .execute_with_context(&mut Context { s: "hello".into() });
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
