use language::compiler::errors::ErrorPrinter;
use language::compiler::Compiler;
use std::fs;

fn test() -> Result<(), String> {
  let mut compiler = Compiler::new();
  let source = fs::read_to_string("program.pr").unwrap();
  if let Err(err) = compiler.compile(&source) {
    Err(ErrorPrinter::to_string(&err, &source))
  } else {
    Ok(())
  }
}

fn main() {
  if let Err(msg) = test() {
    println!("{}", msg);
  }
}
