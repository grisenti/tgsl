use crate::compiler::semantics::environment::Environment;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportError {
  NameRedefinition(String),
  OverloadRedefinition(String),
  NotAValidModule,
}

pub type ImportResult = Result<(), ImportError>;

impl<'src> Environment<'src> {
  pub fn import_module(&mut self, name: &str) -> ImportResult {
    let module = if let Some(module_id) = self.global_env.get_module(name) {
      module_id
    } else {
      return Err(ImportError::NotAValidModule);
    };
    self.global_variables.import(&module.global_variables);
    self.global_functions.import(&module.functions);
    self
      .global_structs
      .import(&module.structs)
      .expect("todo: consider errors");
    Ok(())
  }
}
