use std::fs;

use language::vm::VM;

fn test() -> Result<(), String> {
  let mut vm = VM::new();
  vm.load_module(&fs::read_to_string("prog2.pr").unwrap(), vec![])?;
  let source = fs::read_to_string("program.pr").unwrap();
  vm.load_module(&source, vec![])?;
  Ok(())
}

fn main() {
  if let Err(msg) = test() {
    println!("{}", msg);
  }
}
