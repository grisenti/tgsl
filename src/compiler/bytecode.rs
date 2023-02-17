use super::ast::{Literal, Operator, AST};
use core::fmt::Debug;
use std::mem::ManuallyDrop;

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum OpCode {
  Return,

  Constant,

  // primitive operations
  // numbers
  NegNum,
  AddNum,
  SubNum,
  MulNum,
  DivNum,

  // strings
  AddStr,

  Last,
}

impl OpCode {
  pub fn from_numeric_op(op: Operator) -> Self {
    match op {
      Operator::Basic('+') => OpCode::AddNum,
      Operator::Basic('-') => OpCode::SubNum,
      Operator::Basic('*') => OpCode::MulNum,
      Operator::Basic('/') => OpCode::DivNum,
      _ => unimplemented!(),
    }
  }
}

pub enum ValueType {
  Number,
  Bool,
  String,
  Object,
}

pub union Value {
  pub number: f64,
  pub boolean: bool,
  pub string: ManuallyDrop<String>,
}

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
          string: ManuallyDrop::new(s.get(ast).to_string()),
        },
      },
      _ => unimplemented!(),
    }
  }

  pub fn free(&mut self) {
    match self.kind {
      ValueType::String => unsafe { ManuallyDrop::drop(&mut self.value.string) },
      _ => {}
    }
  }

  pub fn clone_value(&self) -> Value {
    match self.kind {
      ValueType::Number => Value {
        number: unsafe { self.value.number },
      },
      ValueType::String => Value {
        string: unsafe { self.value.string.clone() },
      },
      _ => todo!(),
    }
  }
}

impl ToString for TaggedValue {
  fn to_string(&self) -> String {
    match self.kind {
      ValueType::Number => unsafe { self.value.number.to_string() },
      ValueType::String => unsafe {
        format!(
          "\"{}\"",
          ManuallyDrop::into_inner(self.value.string.clone())
        )
      },
      _ => todo!(),
    }
  }
}

pub struct Chunk {
  pub code: Vec<u8>,
  pub constants: Vec<TaggedValue>,
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
      c.free();
    }
  }
}

impl Debug for Chunk {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut result = String::new();
    let mut index = 0;
    while index < self.code.len() {
      let code = unsafe { std::mem::transmute::<u8, OpCode>(self.code[index]) };
      match code {
        OpCode::Constant => {
          index += 1;
          result += &format!(
            "constant: {}\n",
            self.constants[self.code[index] as usize].to_string()
          );
        }
        code => result += &format!("{:?}\n", code),
      }
      index += 1;
    }
    write!(f, "{}", result)
  }
}
