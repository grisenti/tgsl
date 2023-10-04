use core::fmt::Debug;

use crate::compiler::functions::overload_set::FunctionAddress;
use crate::compiler::variables::GlobalVarAddress;

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

#[derive(Clone, Debug, PartialEq)]
pub enum ConstantValue {
  Number(f64),
  RelativeNativeGlobalVar(u32),
  AbsoluteNativeGlobalVar(u32),
  RelativeNativeFn(u32),
  RelativeExternFn(u32),
  AbsoluteNativeFn(u32),
  AbsoluteExternFn(u32),
  Bool(bool),
  Str(String),
  None,
  Stub,
}

impl From<FunctionAddress> for ConstantValue {
  fn from(value: FunctionAddress) -> Self {
    match value {
      FunctionAddress::RelativeNative(a) => Self::RelativeNativeFn(a),
      FunctionAddress::RelativeExtern(a) => Self::RelativeExternFn(a),
      FunctionAddress::AbsoluteNative(a) => Self::AbsoluteNativeFn(a),
      FunctionAddress::AbsoluteExtern(a) => Self::AbsoluteExternFn(a),
    }
  }
}

impl From<GlobalVarAddress> for ConstantValue {
  fn from(value: GlobalVarAddress) -> Self {
    match value {
      GlobalVarAddress::AbsoluteNative(a) => Self::AbsoluteNativeGlobalVar(a),
      GlobalVarAddress::RelativeNative(a) => Self::RelativeNativeGlobalVar(a),
    }
  }
}
