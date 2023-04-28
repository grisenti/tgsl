use std::fs;

use language::vm::VM;

fn test() -> Result<(), String> {
  let mut vm = VM::new();
  let p1 = fs::read_to_string("program.pr").unwrap();
  vm.load_module("hello".to_string(), &p1, vec![])?;
  Ok(())
}

fn main() {
  if let Err(msg) = test() {
    println!("{}", msg);
  }
}
