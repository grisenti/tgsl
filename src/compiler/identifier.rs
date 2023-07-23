pub type CaptureId = u8;
pub type LocalId = u8;

macro_rules! define_identifier {
  ($name:ident) => {
    #[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
    pub struct $name(u32);

    impl $name {
      const MODIFIERS_MASK: u32 = 0xf0000000;
      const IS_RELATIVE_BIT: u32 = 0x80000000;
      const IS_PUBLIC_ID_BIT: u32 = 0x40000000;

      pub fn relative(id: u32) -> Self {
        assert!(id < Self::MODIFIERS_MASK);
        Self(id | Self::IS_RELATIVE_BIT)
      }

      pub fn absolute(id: u32) -> Self {
        assert!(id <= Self::MODIFIERS_MASK, "module id not in range");

        Self(id)
      }

      pub fn into_public(self) -> Self {
        assert!(self.is_relative(), "absolute ids are implicitely public");

        Self(self.0 | Self::IS_PUBLIC_ID_BIT)
      }

      pub fn get_id(self) -> u32 {
        self.0 & !Self::MODIFIERS_MASK
      }

      pub fn is_relative(self) -> bool {
        (self.0 & Self::IS_RELATIVE_BIT) != 0
      }

      pub fn is_public(self) -> bool {
        (self.0 & Self::IS_PUBLIC_ID_BIT) != 0
      }
    }
  };
}

define_identifier!(GlobalVarId);
define_identifier!(ExternId);

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub enum VariableIdentifier {
  Global(GlobalVarId),
  Capture(CaptureId),
  Local(LocalId),
  Invalid,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub enum Identifier {
  Variable(VariableIdentifier),
  ExternFunction(ExternId),
  Invalid,
}

impl Into<Identifier> for VariableIdentifier {
  fn into(self) -> Identifier {
    Identifier::Variable(self)
  }
}

impl Into<Identifier> for GlobalVarId {
  fn into(self) -> Identifier {
    VariableIdentifier::Global(self).into()
  }
}

impl Into<Identifier> for ExternId {
  fn into(self) -> Identifier {
    Identifier::ExternFunction(self)
  }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub struct ModuleId(pub u16);
