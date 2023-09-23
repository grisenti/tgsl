use std::hash::{BuildHasherDefault, Hasher};

#[derive(Default)]
pub struct IdHasher(u64);

impl Hasher for IdHasher {
  fn finish(&self) -> u64 {
    self.0
  }

  fn write(&mut self, _: &[u8]) {
    unimplemented!()
  }

  fn write_u16(&mut self, i: u16) {
    *self = IdHasher(i as u64);
  }

  fn write_u32(&mut self, i: u32) {
    *self = IdHasher(i as u64);
  }

  fn write_u64(&mut self, i: u64) {
    *self = IdHasher(i)
  }

  fn write_usize(&mut self, i: usize) {
    *self = IdHasher(i as u64)
  }
}

#[allow(unused)]
pub type IdBuildHasher = BuildHasherDefault<IdHasher>;
