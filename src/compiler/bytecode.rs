use super::identifier::{ExternId, GlobalVarId};
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
#[derive(Clone, Debug)]
pub enum ConstantValue {
  Number(f64),
  GlobalId(GlobalVarId),
  ExternId(ExternId),
  Bool(bool),
  Str(String),
  None,
}
