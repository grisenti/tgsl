use language::compiler::errors::ErrorPrinter;
use language::compiler::Compiler;
use language::vm::VM;
use std::fs;

fn test() -> Result<(), String> {
  let source = fs::read_to_string("program.pr").unwrap();
  let mut vm = VM::new();
  vm.load_module(&fs::read_to_string("prog2.pr").unwrap(), vec![])?;
  vm.load_module(&source, vec![])?;
  Ok(())
}

fn main() {
  if let Err(msg) = test() {
    println!("{}", msg);
  }
}
