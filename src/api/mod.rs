use crate::compiler::errors::ErrorPrinter;
use crate::compiler::Compiler;
use crate::extern_function::ExternFunctionInfo;
use crate::standard_library::load_standard_library;
use crate::vm::VM;

pub mod extern_function;
pub mod value;

pub struct Tgsl {
  vm: VM,
  compiler: Compiler,
}

impl Tgsl {
  pub fn load_module<'src>(
    &mut self,
    source: &str,
    extern_functions: Vec<ExternFunctionInfo>,
  ) -> Result<(), String> {
    match self.compiler.compile(source) {
      Ok(compiled_module) => self.vm.load_module(compiled_module, extern_functions),
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
