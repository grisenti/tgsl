use super::ast::Operator;
use core::fmt::Debug;

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum OpCode {
  Return,

  Constant,
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

  Print,
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

#[derive(Clone)]
pub struct Function {
  pub code: Chunk,
}

#[derive(Clone, Debug)]
pub enum ConstantValue {
  Number(f64),
  GlobalId(u16),
  ExternId(u16),
  Bool(bool),
  Str(String),
  None,
}

#[derive(Clone)]
pub struct Chunk {
  pub code: Vec<u8>,
  pub functions: Vec<Function>,
  pub constants: Vec<ConstantValue>,
}

impl Default for Chunk {
  fn default() -> Self {
    Self {
      code: Vec::new(),
      constants: vec![ConstantValue::None],
      functions: Vec::new(),
    }
  }
}

impl Chunk {
  pub fn get_constant(&self, index: usize) -> &ConstantValue {
    &self.constants[index]
  }

  pub fn get_function(&self, index: usize) -> *const Function {
    std::ptr::addr_of!(self.functions[index])
  }

  pub fn empty() -> Self {
    Default::default()
  }
}

impl Debug for Chunk {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut result = "".to_string();
    for (i, func) in self.functions.iter().enumerate() {
      result += &format!("++ fn {i} ++\n{:?}-- fn {i} --\n", func.code);
    }
    let mut index = 0;
    while index < self.code.len() {
      let code = unsafe { std::mem::transmute::<u8, OpCode>(self.code[index]) };
      result += &format!("{index}: ");
      match code {
        OpCode::Constant => {
          index += 1;
          result += &format!(
            "Constant: {:?}\n",
            self.constants[self.code[index] as usize]
          );
        }
        OpCode::Function => {
          index += 1;
          result += &format!("Function: {}\n", self.code[index]);
        }
        OpCode::Call => {
          index += 1;
          result += &format!("Call: {}\n", self.code[index]);
        }
        OpCode::JumpIfFalsePop | OpCode::Jump | OpCode::JumpIfFalseNoPop => {
          index += 2;
          let jump_point = u16::from_ne_bytes([self.code[index - 1], self.code[index]]);
          result += &format!("{code:?}: {}\n", index + 1 + jump_point as usize);
        }
        OpCode::BackJump => {
          index += 2;
          let jump_point = u16::from_ne_bytes([self.code[index - 1], self.code[index]]);
          result += &format!("{code:?}: {}\n", index + 1 - jump_point as usize);
        }
        OpCode::GetLocal
        | OpCode::SetLocal
        | OpCode::GetCapture
        | OpCode::SetCapture
        | OpCode::GetMember
        | OpCode::SetMember
        | OpCode::Construct => {
          index += 1;
          result += &format!("{code:?}: {}\n", self.code[index])
        }
        OpCode::MakeClosure => {
          index += 1;
          result += &format!("MakeClosure, captures: {}\n", self.code[index])
        }
        code => result += &format!("{code:?}\n"),
      }
      index += 1;
    }
    write!(f, "{result}")
  }
}
