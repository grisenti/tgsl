use language::compiler::Compiler;
use std::fs;

fn test() -> Result<(), String> {
  let mut compiler = Compiler::new();
  let source = fs::read_to_string("program.pr").unwrap();
  compiler.compile(&source)?;
  Ok(())
}

fn main() {
  if let Err(msg) = test() {
    println!("{}", msg);
  }
}
