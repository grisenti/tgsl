use crate::api::library::Library;
use crate::compiler;
use crate::compiler::errors::{CompilerError, ErrorPrinter};
use crate::compiler::functions::RelativeFunctionAddress;
use crate::compiler::{CompiledModule, Compiler};
use crate::errors::{ForeignBindingError, LoadModuleError};
use crate::foreign_function::{ForeignFunction, ForeignParameters, FunctionSignature};
use crate::types::Type;
use crate::value::ForeignValue;
use crate::vm::{VmForeignFunction, VM};

pub mod errors;
pub mod foreign_function;
pub mod gc;
pub mod library;
pub mod types;
pub mod value;

pub struct Tgsl {
  vm: VM,
  compiler: Compiler,
}

impl Tgsl {
  pub fn load_module(&mut self, source: &str) -> ModuleLoader {
    match self.compiler.compile(source) {
      Ok(_) => panic!(),
      Err(err) => {
        println!("{}", ErrorPrinter::to_string(&err, source));
        panic!();
      }
    }
    panic!()
    // match self.compiler.compile(source) {
    //   Ok(compiled_module) => ModuleLoader {
    //     tgsl: self,
    //     compiler_errors: vec![],
    //     function_binding_errors: vec![],
    //     compiled_module: Some(compiled_module),
    //     bound_foreign_functions: vec![],
    //   },
    //   Err(compiler_errors) => ModuleLoader {
    //     tgsl: self,
    //     compiler_errors,
    //     compiled_module: None,
    //     function_binding_errors: vec![],
    //     bound_foreign_functions: vec![],
    //   },
    // }
  }

  pub fn load_library<L: Library>(&mut self, mut library: L)
  where
    L::Context: 'static,
  {
    library.load(self);
    if let Some(context) = library.context() {
      self.vm.add_library_context(Box::new(context))
    }
  }
}

impl Default for Tgsl {
  fn default() -> Self {
    let mut tgsl = Self {
      vm: Default::default(),
      compiler: Default::default(),
    };
    tgsl
  }
}

pub struct ModuleLoader<'tgsl> {
  tgsl: &'tgsl mut Tgsl,
  compiler_errors: Vec<CompilerError>,
  function_binding_errors: Vec<ForeignBindingError>,
  compiled_module: Option<CompiledModule>,
  bound_foreign_functions: Vec<(RelativeFunctionAddress, VmForeignFunction)>,
}

impl<'tgsl> ModuleLoader<'tgsl> {
  #[must_use]
  pub fn bind_function<Signature, F: ForeignFunction<Signature>>(
    mut self,
    name: &str,
    foreign_function: F,
  ) -> Self
  where
    Signature: FunctionSignature,
  {
    if let Some(compiled_module) = &self.compiled_module {
      let result = bind_function(
        name,
        &Signature::parameter_types(),
        Signature::return_type(),
        &compiled_module.foreign_functions,
      );
      match result {
        Ok(address) => self.bound_foreign_functions.push((
          address,
          VmForeignFunction {
            function: foreign_function.raw_foreign_function(),
            context_type_id: Signature::context_type_id(),
          },
        )),
        Err(err) => self.function_binding_errors.push(err),
      }
    }
    self
  }

  pub fn execute(self) -> Result<(), LoadModuleError> {
    self.execute_with_context(&mut ())
  }

  pub fn execute_with_context<Context: 'static>(
    mut self,
    context: &mut Context,
  ) -> Result<(), LoadModuleError> {
    if !self.compiler_errors.is_empty() {
      Err(LoadModuleError::CompilerErrors(self.compiler_errors))
    } else if !self.function_binding_errors.is_empty() {
      Err(LoadModuleError::ForeignBindingErrors(
        self.function_binding_errors,
      ))
    } else {
      // no errors means that compilation was successful
      let compiled_module = self.compiled_module.unwrap();
      self
        .bound_foreign_functions
        .sort_by_key(|(address, _)| *address);

      for f in compiled_module.foreign_functions {
        if self
          .bound_foreign_functions
          .binary_search_by_key(&f.relative_address, |&(address, _)| address)
          .is_err()
        {
          self
            .function_binding_errors
            .push(ForeignBindingError::MissingForeignFunction);
        }
      }

      if !self.function_binding_errors.is_empty() {
        Err(LoadModuleError::ForeignBindingErrors(
          self.function_binding_errors,
        ))
      } else {
        self
          .tgsl
          .vm
          .load_module(
            compiled_module.program,
            self.bound_foreign_functions.into_iter().map(|(_, f)| f),
            context,
          )
          .map_err(LoadModuleError::RuntimeError)
      }
    }
  }
}

fn bind_function(
  name: &str,
  parameter_types: &[Type],
  return_type: Type,
  module_foreign_functions: &[compiler::functions::ForeignFunction],
) -> Result<RelativeFunctionAddress, ForeignBindingError> {
  for declared_function in module_foreign_functions
    .iter()
    .filter(|f| f.name.as_ref() == name)
  {
    if declared_function.signature.get_parameters() == parameter_types {
      return if *declared_function.signature.get_return_type() == return_type {
        Ok(declared_function.relative_address)
      } else {
        Err(ForeignBindingError::InconsistentReturnType {
          source_function: declared_function.signature.format_with_name(name),
          provided_return_type: return_type.to_string(),
        })
      };
    }
  }
  Err(ForeignBindingError::FunctionNameNotInSource(
    name.to_string(),
  ))
}
