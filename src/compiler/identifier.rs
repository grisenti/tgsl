pub type CaptureId = u8;
pub type LocalId = u8;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub struct GlobalId {
  relative_id: u16,
  modifiers: u16,
}

impl GlobalId {
  pub const IS_PUBLIC_ID_BIT: u16 = 0x8000;
  pub const ABSOLUTE_ID_MODULE_MASK: u16 = 0x7fff;
  pub const RELATIVE_ID_MASK: u32 = 0x000fffff;

  pub fn relative(id: u16) -> Self {
    Self {
      relative_id: id,
      modifiers: 0,
    }
  }

  pub fn absolute(module: u16, id: u16) -> Self {
    assert!(
      module <= Self::ABSOLUTE_ID_MODULE_MASK,
      "module id not in range"
    );

    Self {
      relative_id: id,
      modifiers: module + 1,
    }
  }

  pub fn into_public(self) -> Self {
    assert!((self.modifiers & Self::ABSOLUTE_ID_MODULE_MASK) == 0);
    Self {
      relative_id: self.relative_id,
      modifiers: self.modifiers | Self::IS_PUBLIC_ID_BIT,
    }
  }

  pub fn get_relative(self) -> u16 {
    self.relative_id
  }

  pub fn split_absolute(self) -> (u16, u16) {
    (self.relative_id, self.modifiers - 1)
  }

  pub fn is_relative(self) -> bool {
    (self.modifiers & Self::ABSOLUTE_ID_MODULE_MASK) == 0
  }

  pub fn is_public(self) -> bool {
    (self.modifiers & Self::IS_PUBLIC_ID_BIT) != 0
  }
}

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
