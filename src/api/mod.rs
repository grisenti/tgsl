use crate::api::module::Module;
use crate::compiler::errors::ErrorPrinter;
use crate::compiler::Compiler;
use crate::standard_library::load_standard_library;
use crate::vm::VM;

pub mod foreign_function;
pub mod gc;
pub mod module;
pub mod types;
pub mod value;

pub struct Tgsl {
  vm: VM,
  compiler: Compiler,
}

impl Tgsl {
  pub fn load_module(&mut self, module: Module) -> Result<(), String> {
    match self.compiler.compile(module.source) {
      Ok(compiled_module) => self
        .vm
        .load_module(compiled_module, module.foreign_functions),
      Err(errors) => Err(ErrorPrinter::to_string(&errors, module.source)),
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
