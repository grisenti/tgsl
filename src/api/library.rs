use crate::Tgsl;

pub trait Library {
  fn load(&mut self, tgls: &mut Tgsl);
}
