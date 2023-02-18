use std::collections::HashMap;

use crate::compiler::{
  ast::Identifier,
  bytecode::{Chunk, OpCode, TaggedValue, Value, ValueType},
};

pub struct VM {
  pc: *const u8,
  program: Chunk,
  bytes_read: usize,
  stack: Vec<TaggedValue>,
}

macro_rules! binary_operation {
  ($s:ident, $t:ident, $kind:expr, $op:tt) => {
    let rhs = unsafe {$s.pop().value.$t};
    let lhs = unsafe {$s.pop().value.$t};
	$s.push(TaggedValue{ kind: $kind, value: Value{ $t: lhs $op rhs }});
  };
}

macro_rules! binary_operation {
  ($s:ident, $t:ident, $kind:expr, $op:tt) => {
    let rhs = unsafe {$s.pop().value.$t};
    let lhs = unsafe {$s.pop().value.$t};
	$s.push(TaggedValue{ kind: $kind, value: Value{ $t: lhs $op rhs }});
  };
}

macro_rules! unary_operation {
  ($s:ident, $t:ident, $kind:expr, $op:tt) => {
	  let rhs = unsafe {$s.pop().value.$t};
	  $s.push(TaggedValue{ kind: $kind, value: Value{ $t: $op rhs }});
	};
}

impl VM {
  fn read_instruction(&mut self) -> OpCode {
    let instruction = self.read_byte();
    assert!(instruction < OpCode::Last as u8);
    unsafe { std::mem::transmute::<u8, OpCode>(instruction) }
  }

  fn pop(&mut self) -> TaggedValue {
    self.stack.pop().unwrap()
  }

  fn push(&mut self, val: TaggedValue) {
    self.stack.push(val)
  }

  fn read_byte(&mut self) -> u8 {
    assert!(!self.pc.is_null());
    assert!(self.bytes_read < self.program.code.len());
    let val = unsafe { *self.pc };
    unsafe { self.pc = self.pc.add(1) };
    self.bytes_read += 1;
    val
  }

  fn read_constant(&mut self) -> TaggedValue {
    let index = self.read_byte();
    self.program.get_constant(index as usize)
  }

  pub fn run(&mut self) {
    loop {
      match self.read_instruction() {
        OpCode::Constant => {
          let c = self.read_constant();
          self.push(c);
        }
        OpCode::AddNum => {
          binary_operation!(self, number, ValueType::Number, +);
        }
        OpCode::SubNum => {
          binary_operation!(self, number, ValueType::Number, -);
        }
        OpCode::MulNum => {
          binary_operation!(self, number, ValueType::Number, *);
        }
        OpCode::DivNum => {
          binary_operation!(self, number, ValueType::Number, /);
        }
        OpCode::NegNum => {
          unary_operation!(self, number, ValueType::Number, -);
        }
        OpCode::AddStr => {
          let mut rhs = self.pop();
          let mut lhs = self.pop();
          let rhs_str = unsafe { rhs.value.string };
          let lhs_str = unsafe { lhs.value.string };
          self.push(TaggedValue {
            kind: ValueType::String,
            value: Value {
              string: Box::into_raw(Box::new(unsafe { (*lhs_str).clone() + &*rhs_str })),
            },
          });
          unsafe { rhs.free() };
          unsafe { lhs.free() };
        }
        OpCode::Print => {
          let mut top = self.pop();
          println!("{}", top.to_string());
          unsafe { top.free() };
        }
        OpCode::Pop => {
          let mut top = self.pop();
          unsafe { top.free() };
        }
        OpCode::Return => {
          return;
        }
        _ => {}
      }
    }
  }

  pub fn interpret(&mut self, chunk: Chunk) {
    self.pc = chunk.code.as_ptr();
    self.program = chunk;
    self.run()
  }

  pub fn new() -> Self {
    Self {
      pc: std::ptr::null(),
      program: Chunk::empty(),
      bytes_read: 0,
      stack: Vec::new(),
    }
  }
}
