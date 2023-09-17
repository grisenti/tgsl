pub type CaptureId = u8;
pub type LocalId = u8;
pub type OverloadId = u32;

macro_rules! define_identifier {
  ($name:ident) => {
    #[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
    pub struct $name(u32);

    impl $name {
      const MODIFIERS_MASK: u32 = 0xf0000000;
      const IS_RELATIVE_BIT: u32 = 0x80000000;
      const IS_PUBLIC_ID_BIT: u32 = 0x40000000;

      pub const INVALID: Self = Self(u32::MAX);

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
        assert!(self != Self::INVALID);
        self.0 & !Self::MODIFIERS_MASK
      }

      pub fn is_relative(self) -> bool {
        assert!(self != Self::INVALID);
        (self.0 & Self::IS_RELATIVE_BIT) != 0
      }

      pub fn is_public(self) -> bool {
        assert!(self != Self::INVALID);
        (self.0 & Self::IS_PUBLIC_ID_BIT) != 0
      }
    }
  };
}

define_identifier!(GlobalVarId);
define_identifier!(ExternId);
define_identifier!(StructId);
define_identifier!(FunctionId);

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub enum VariableIdentifier {
  Global(GlobalVarId),
  Capture(CaptureId),
  Local(LocalId),
  Invalid,
}

impl From<GlobalVarId> for VariableIdentifier {
  fn from(value: GlobalVarId) -> Self {
    VariableIdentifier::Global(value)
  }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub enum Identifier {
  Variable(VariableIdentifier),
  ExternFunction(ExternId),
  Struct(StructId),
  Function(FunctionId),
  Invalid,
}

impl From<VariableIdentifier> for Identifier {
  fn from(val: VariableIdentifier) -> Self {
    Identifier::Variable(val)
  }
}

impl From<GlobalVarId> for Identifier {
  fn from(val: GlobalVarId) -> Self {
    VariableIdentifier::Global(val).into()
  }
}

impl From<ExternId> for Identifier {
  fn from(val: ExternId) -> Self {
    Identifier::ExternFunction(val)
  }
}

impl From<StructId> for Identifier {
  fn from(value: StructId) -> Self {
    Identifier::Struct(value)
  }
}

impl From<FunctionId> for Identifier {
  fn from(value: FunctionId) -> Self {
    Identifier::Function(value)
  }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub enum GlobalIdentifier {
  Variable(GlobalVarId),
  ExternFunction(ExternId),
  Struct(StructId),
  OverloadId(OverloadId),
  Invalid,
}

impl From<GlobalIdentifier> for Identifier {
  fn from(value: GlobalIdentifier) -> Self {
    match value {
      GlobalIdentifier::Variable(id) => VariableIdentifier::Global(id).into(),
      GlobalIdentifier::ExternFunction(id) => Identifier::ExternFunction(id),
      GlobalIdentifier::Struct(id) => Identifier::Struct(id),
      _ => panic!(),
    }
  }
}

impl From<ExternId> for GlobalIdentifier {
  fn from(value: ExternId) -> Self {
    Self::ExternFunction(value)
  }
}

impl From<GlobalVarId> for GlobalIdentifier {
  fn from(value: GlobalVarId) -> Self {
    Self::Variable(value)
  }
}

impl From<StructId> for GlobalIdentifier {
  fn from(value: StructId) -> Self {
    Self::Struct(value)
  }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub struct ModuleId(pub u16);
