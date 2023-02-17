use crate::compiler::bytecode::{Chunk, OpCode, Value};

pub struct VM {
  pc: *const u8,
  program: Chunk,
  bytes_read: usize,
  stack: Vec<Value>,
}

macro_rules! binary_operation {
  ($s:ident, $t:ident, $op:tt) => {
    let lhs = unsafe {$s.pop().$t};
    let rhs = unsafe {$s.pop().$t};
	$s.push(Value{ $t: lhs $op rhs });
  };
}

macro_rules! unary_operation {
	($s:ident, $t:ident, $op:tt) => {
	  let rhs = unsafe {$s.pop().$t};
	  $s.push(Value{ number: $op rhs });
	};
  }

impl VM {
  fn read_instruction(&mut self) -> OpCode {
    let instruction = self.read_byte();
    assert!(instruction < OpCode::Last as u8);
    unsafe { std::mem::transmute::<u8, OpCode>(instruction) }
  }

  fn pop(&mut self) -> Value {
    self.stack.pop().unwrap()
  }

  fn push(&mut self, val: Value) {
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

  fn read_constant(&mut self) -> Value {
    let index = self.read_byte();
    assert!(index < self.program.constants.len() as u8);
    self.program.constants[index as usize].clone_value()
  }

  pub fn run(&mut self) {
    loop {
      match self.read_instruction() {
        OpCode::Constant => {
          let c = self.read_constant();
          self.push(c);
        }
        OpCode::AddNum => {
          binary_operation!(self, number, +);
        }
        OpCode::SubNum => {
          binary_operation!(self, number, -);
        }
        OpCode::MulNum => {
          binary_operation!(self, number, *);
        }
        OpCode::DivNum => {
          binary_operation!(self, number, /);
        }
        OpCode::NegNum => {
          unary_operation!(self, number, -);
        }
        OpCode::Return => {
          println!("{}", unsafe { self.pop().number });
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
