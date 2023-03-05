pub type GlobalId = u16;
pub type CaptureId = u8;
pub type LocalId = u8;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub enum Identifier {
  Global(GlobalId),
  Capture(CaptureId),
  Local(LocalId),
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub struct ExternId(pub u16);
