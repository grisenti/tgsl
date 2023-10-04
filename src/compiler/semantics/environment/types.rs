use crate::compiler::semantics::environment::Environment;

impl<'a> Environment<'a> {
  pub fn is_type(&self, name: &str) -> bool {
    self.global_structs.is_struct(name)
  }
}
