use crate::Tgsl;

pub trait Library {
  type Context;
  fn load(&mut self, tgls: &mut Tgsl);
  fn context(self) -> Option<Self::Context>;
}
