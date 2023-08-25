use std::fs;


use language::vm::VM;

fn test() -> Result<(), String> {
  let mut vm = VM::new();
  let source = fs::read_to_string("program.pr").unwrap();
  vm.load_module(&source, vec![])?;
  Ok(())
}

fn main() {
  if let Err(msg) = test() {
    println!("{}", msg);
  }
}
