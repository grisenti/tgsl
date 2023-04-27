pub type CaptureId = u8;
pub type LocalId = u8;
pub type GlobalId = u16;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub enum Identifier {
  Global(GlobalId),
  Capture(CaptureId),
  Local(LocalId),
  Invalid,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub struct ExternId(pub u16);

#[derive(Default, Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub struct ModuleId(pub u16);
