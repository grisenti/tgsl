use super::{
  identifier::{ExternId, GlobalVarId},
  operators::UnaryOperator,
};
use crate::compiler::operators::BinaryOperator;
use core::fmt::Debug;

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum OpCode {
  Return,

  Constant,
  ConstantStr,
  Function,

  GetGlobal,
  SetGlobal,
  GetLocal,
  SetLocal,

  MakeClosure,
  Capture,
  GetCapture,
  SetCapture,

  // primitive operations
  // numbers
  NegNum,
  AddNum,
  SubNum,
  MulNum,
  DivNum,
  LeNum,
  GeNum,
  LeqNum,
  GeqNum,
  SameNum,
  DiffNum,

  // strings
  AddStr,
  LeStr,
  GeStr,
  LeqStr,
  GeqStr,
  SameStr,
  DiffStr,

  // bool
  NotBool,
  SameBool,
  DiffBool,

  // functions
  Call,

  // structs
  Construct,
  GetMember,
  SetMember,

  Jump,
  BackJump,
  JumpIfFalsePop,
  JumpIfFalseNoPop,

  Pop,

  Last,
}

impl From<BinaryOperator> for OpCode {
  fn from(value: BinaryOperator) -> Self {
    match value {
      BinaryOperator::AddNum => OpCode::AddNum,
      BinaryOperator::SubNum => OpCode::SubNum,
      BinaryOperator::MulNum => OpCode::MulNum,
      BinaryOperator::DivNum => OpCode::DivNum,
      BinaryOperator::LeNum => OpCode::LeNum,
      BinaryOperator::GeNum => OpCode::GeNum,
      BinaryOperator::LeqNum => OpCode::LeqNum,
      BinaryOperator::GeqNum => OpCode::GeqNum,
      BinaryOperator::SameNum => OpCode::SameNum,
      BinaryOperator::DiffNum => OpCode::DiffNum,
      BinaryOperator::AddStr => OpCode::AddStr,
      BinaryOperator::LeStr => OpCode::LeStr,
      BinaryOperator::GeStr => OpCode::GeStr,
      BinaryOperator::LeqStr => OpCode::LeqStr,
      BinaryOperator::GeqStr => OpCode::GeqStr,
      BinaryOperator::SameStr => OpCode::SameStr,
      BinaryOperator::DiffStr => OpCode::DiffStr,
      BinaryOperator::SameBool => OpCode::SameBool,
      BinaryOperator::DiffBool => OpCode::DiffBool,
      BinaryOperator::Invalid => panic!(),
    }
  }
}

impl From<UnaryOperator> for OpCode {
  fn from(value: UnaryOperator) -> Self {
    match value {
      UnaryOperator::NegNum => OpCode::NegNum,
      UnaryOperator::NotBool => OpCode::NotBool,
      UnaryOperator::Invalid => panic!(),
    }
  }
}

#[derive(Clone, Debug)]
pub enum ConstantValue {
  Number(f64),
  GlobalId(GlobalVarId),
  ExternId(ExternId),
  Bool(bool),
  Str(String),
  None,
}
