use core::fmt::Debug;

use crate::compiler::functions::overload_set::FunctionAddress;

#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub enum OpCode {
  Return,

  Constant,
  ConstantStr,

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
  CallNative,
  CallForeign,
  CallValue,

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

#[derive(Clone, Debug, PartialEq)]
pub enum ConstantValue {
  Number(f64),
  RelativeNativeGlobalVar(u32),
  AbsoluteNativeGlobalVar(u32),
  RelativeNativeFn(u32),
  RelativeForeignFn(u32),
  AbsoluteNativeFn(u32),
  AbsoluteForeignFn(u32),
  Bool(bool),
  Str(String),
  None,
  Stub,
}

impl From<FunctionAddress> for ConstantValue {
  fn from(value: FunctionAddress) -> Self {
    todo!()
  }
}
