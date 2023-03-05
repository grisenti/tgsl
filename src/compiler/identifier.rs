#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub enum Identifier {
  Global(u16),
  Capture(u8),
  Local(u8),
}
