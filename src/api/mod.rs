use crate::compiler::errors::ErrorPrinter;
use crate::compiler::Compiler;
use crate::foreign_function::ForeignFunction;
use crate::standard_library::load_standard_library;
use crate::vm::VM;

pub mod foreign_function;
pub mod gc;
pub mod types;
pub mod value;

pub struct Tgsl {
  vm: VM,
  compiler: Compiler,
}

impl Tgsl {
  pub fn load_module<'src>(
    &mut self,
    source: &str,
    foreign_functions: Vec<ForeignFunction>,
  ) -> Result<(), String> {
    match self.compiler.compile(source) {
      Ok(compiled_module) => self.vm.load_module(compiled_module, foreign_functions),
      Err(errors) => Err(ErrorPrinter::to_string(&errors, source)),
    }
  }
}

impl Default for Tgsl {
  fn default() -> Self {
    let mut tgsl = Self {
      vm: Default::default(),
      compiler: Default::default(),
    };
    load_standard_library(&mut tgsl);
    tgsl
  }
}
