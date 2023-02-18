use super::ast::{Literal, Operator, AST};
use core::fmt::Debug;

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum OpCode {
  Return,

  Constant,

  GetGlobal,
  SetGlobal,
  GetLocal,
  SetLocal,

  // primitive operations
  // numbers
  NegNum,
  AddNum,
  SubNum,
  MulNum,
  DivNum,

  // strings
  AddStr,

  Jump,
  JumpIfFalse,

  Print,
  Pop,

  Last,
}

impl OpCode {
  pub fn from_operator(op: Operator) -> Self {
    match op {
      Operator::Basic('+') => OpCode::AddNum,
      Operator::Basic('-') => OpCode::SubNum,
      Operator::Basic('*') => OpCode::MulNum,
      Operator::Basic('/') => OpCode::DivNum,
      _ => unimplemented!(),
    }
  }
}

#[derive(Clone, Copy)]
pub enum ValueType {
  Number,
  Bool,
  String,
  Object,
}

#[derive(Clone, Copy)]
pub union Value {
  pub number: f64,
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
    debug_assert!(matches!(jump_type, OpCode::Jump | OpCode::JumpIfFalse));
    unsafe { self.push_op(jump_type) };
    let index = self.code.len();
    self.code.push(0);
    self.code.push(0);
    index
  }

  pub fn backpatch_current_instruction(&mut self, jump_point: usize) {
    let split = ((self.code.len() - jump_point - 2) as u16).to_ne_bytes();
    self.code[jump_point] = split[0];
    self.code[jump_point + 1] = split[1];
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
        OpCode::JumpIfFalse | OpCode::Jump => {
          index += 2;
          let jump_point = u16::from_ne_bytes([self.code[index - 1], self.code[index]]);
          result += &format!("{code:?}: {}\n", index + 1 + jump_point as usize);
        }
        code => result += &format!("{:?}\n", code),
      }
      index += 1;
    }
    write!(f, "{}", result)
  }
}