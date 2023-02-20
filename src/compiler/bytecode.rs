use super::ast::{Literal, Operator, AST};
use core::fmt::Debug;

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum OpCode {
  Return,

  Constant,

  GetGlobal,
  SetGlobal,

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

#[derive(Clone, Copy)]
pub enum ValueType {
  Number,
  Bool,
  String,
  GlobalId,
  Object,
}

#[derive(Clone, Copy)]
pub union Value {
  pub number: f64,
  pub id: u32,
  pub boolean: bool,
  pub string: *mut String,
}

#[derive(Clone, Copy)]
pub struct TaggedValue {
  pub kind: ValueType,
  pub value: Value,
}

impl TaggedValue {
  pub fn from_literal(lit: &Literal, ast: &AST) -> Self {
    match lit {
      Literal::Number(x) => Self {
        kind: ValueType::Number,
        value: Value { number: *x },
      },
      Literal::String(s) => Self {
        kind: ValueType::String,
        value: Value {
          string: Box::into_raw(Box::new(s.get(ast).to_string())),
        },
      },
      Literal::False => Self {
        kind: ValueType::Bool,
        value: Value { boolean: false },
      },
      Literal::True => Self {
        kind: ValueType::Bool,
        value: Value { boolean: true },
      },
      _ => unimplemented!(),
    }
  }

  pub fn global_id(id: u32) -> Self {
    Self {
      kind: ValueType::GlobalId,
      value: Value { id },
    }
  }

  pub unsafe fn free(&mut self) {
    match self.kind {
      ValueType::String => unsafe {
        let _ = Box::from_raw(self.value.string);
      },
      _ => {}
    }
  }

  pub fn copy_object(&self) -> Self {
    let value = match self.kind {
      ValueType::String => unsafe {
        Value {
          string: Box::into_raw(Box::new((*self.value.string).clone())),
        }
      },
      _ => self.value,
    };
    Self {
      kind: self.kind,
      value,
    }
  }
}

impl ToString for TaggedValue {
  fn to_string(&self) -> String {
    match self.kind {
      ValueType::Number => unsafe { self.value.number.to_string() },
      ValueType::Bool => unsafe { self.value.boolean.to_string() },
      ValueType::String => unsafe { format!("\"{}\"", *self.value.string) },
      ValueType::GlobalId => unsafe { self.value.id.to_string() },
      _ => todo!(),
    }
  }
}

pub struct Chunk {
  pub code: Vec<u8>,
  constants: Vec<TaggedValue>,
}

impl Chunk {
  pub unsafe fn push_constant(&mut self, val: TaggedValue) {
    let constant_offset = self.constants.len() as u8;
    self.constants.push(val);
    self.code.push(OpCode::Constant as u8);
    self.code.push(constant_offset);
  }

  pub unsafe fn push_op(&mut self, op: OpCode) {
    self.code.push(op as u8);
  }

  pub fn push_jump(&mut self, jump_type: OpCode) -> usize {
    debug_assert!(matches!(
      jump_type,
      OpCode::Jump | OpCode::JumpIfFalsePop | OpCode::JumpIfFalseNoPop
    ));
    unsafe { self.push_op(jump_type) };
    let index = self.code.len();
    self.code.push(0);
    self.code.push(0);
    index
  }

  pub fn push_back_jump(&mut self, to: usize) {
    assert!((self.code.len() - to + 2) <= u16::MAX as usize);
    unsafe { self.push_op(OpCode::BackJump) };
    // 2 added to skip jump point
    let split = ((self.code.len() - to + 2) as u16).to_ne_bytes();
    self.code.push(split[0]);
    self.code.push(split[1]);
  }

  pub fn backpatch_current_instruction(&mut self, jump_point: usize) {
    assert!((self.code.len() - jump_point - 2) <= u16::MAX as usize);
    let split = ((self.code.len() - jump_point - 2) as u16).to_ne_bytes();
    // 2 removed to skip jump point
    self.code[jump_point] = split[0];
    self.code[jump_point + 1] = split[1];
  }

  pub fn get_next_instruction_label(&self) -> usize {
    self.code.len()
  }

  pub fn get_constant(&self, index: usize) -> TaggedValue {
    self.constants[index].copy_object()
  }

  pub fn empty() -> Self {
    Self {
      code: Vec::new(),
      constants: Vec::new(),
    }
  }
}

impl Drop for Chunk {
  fn drop(&mut self) {
    for c in &mut self.constants {
      unsafe { c.free() };
    }
  }
}

impl Debug for Chunk {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut result = String::new();
    let mut index = 0;
    while index < self.code.len() {
      let code = unsafe { std::mem::transmute::<u8, OpCode>(self.code[index]) };
      result += &format!("{index}: ");
      match code {
        OpCode::Constant => {
          index += 1;
          result += &format!(
            "Constant: {}\n",
            self.constants[self.code[index] as usize].to_string()
          );
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
        code => result += &format!("{:?}\n", code),
      }
      index += 1;
    }
    write!(f, "{}", result)
  }
}
