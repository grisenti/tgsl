use language::vm::extern_function::ExternFunction;
use language::vm::VM;
use std::fs;

fn add((a, b): (f64, f64)) -> f64 {
  a + b
}

fn test() -> Result<(), String> {
  let source = fs::read_to_string("program.pr").unwrap();
  let mut vm = VM::new();
  vm.load_module(&fs::read_to_string("prog2.pr").unwrap(), vec![])?;
  vm.load_module(&source, vec![ExternFunction::create("add", add)])?;
  Ok(())
}

fn main() {
  if let Err(msg) = test() {
    println!("{}", msg);
  }
}
