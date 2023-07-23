use super::{
  ast::Operator,
  identifier::{ExternId, GlobalVarId},
};
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

impl OpCode {
  pub fn from_numeric_operator(op: Operator) -> Self {
    match op {
      Operator::Basic('+') => OpCode::AddNum,
      Operator::Basic('-') => OpCode::SubNum,
      Operator::Basic('*') => OpCode::MulNum,
      Operator::Basic('/') => OpCode::DivNum,
      Operator::Basic('<') => OpCode::LeNum,
      Operator::Basic('>') => OpCode::GeNum,
      Operator::Geq => OpCode::GeqNum,
      Operator::Leq => OpCode::LeqNum,
      Operator::Same => OpCode::SameNum,
      Operator::Different => OpCode::DiffNum,
      _ => panic!(),
    }
  }

  pub fn from_string_comp_operator(op: Operator) -> Self {
    match op {
      Operator::Basic('<') => OpCode::LeStr,
      Operator::Basic('>') => OpCode::GeStr,
      Operator::Geq => OpCode::GeqStr,
      Operator::Leq => OpCode::LeqStr,
      Operator::Same => OpCode::SameStr,
      Operator::Different => OpCode::DiffStr,
      _ => panic!(),
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
