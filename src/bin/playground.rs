use std::fs;
use tgsl::Tgsl;

fn add((a, b): (f64, f64)) -> f64 {
  a + b
}

fn concat((a, b): (String, String)) -> String {
  a + &b
}

fn test() -> Result<(), String> {
  let source = fs::read_to_string("program.pr").unwrap();
  let mut tgsl = Tgsl::default();
  //vm.load_module(&fs::read_to_string("prog2.pr").unwrap(), vec![])?;
  tgsl.load_module(&source, vec![])?;
  Ok(())
}

fn main() {
  if let Err(msg) = test() {
    println!("{}", msg);
  }
}
