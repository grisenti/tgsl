use std::collections::HashMap;

use crate::compiler::bytecode::{Chunk, Function, OpCode, TaggedValue, Value, ValueType};

const MAX_CALLS: usize = 64;
const MAX_STACK: usize = MAX_CALLS * u8::MAX as usize;

struct CallFrame {
  pc: *const u8,
  function: *const Function,
  bp: usize,
}

pub struct VM {
  pc: *const u8,
  function: *const Function,
  bp: usize,
  bytes_read: usize,
  stack: Vec<TaggedValue>,
  call_stack: Vec<CallFrame>,
  globals: HashMap<u16, TaggedValue>,
}

macro_rules! binary_operation {
  ($s:ident, $operator:ident, $result:ident, $kind:expr, $op:tt) => {
    let rhs = unsafe {$s.pop().value.$operator};
    let lhs = unsafe {$s.pop().value.$operator};
	$s.push(TaggedValue{ kind: $kind, value: Value{ $result: lhs $op rhs }});
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

  fn top(&self) -> TaggedValue {
    *self.stack.last().unwrap()
  }

  fn read_byte(&mut self) -> u8 {
    assert!(!self.pc.is_null());
    let val = unsafe { *self.pc };
    unsafe { self.pc = self.pc.add(1) };
    self.bytes_read += 1;
    val
  }

  fn read_constant(&mut self) -> TaggedValue {
    let index = self.read_byte();
    unsafe { (*self.function).code.get_constant(index as usize) }
  }

  fn read_function(&mut self) -> TaggedValue {
    let index = self.read_byte();
    unsafe { (*self.function).code.get_function(index as usize) }
  }

  pub fn run(&mut self) {
    loop {
      let op = self.read_instruction();
      println!("{:?}: {:?}\n", &op, &self.stack);
      match op {
        OpCode::Constant => {
          let c = self.read_constant();
          self.push(c);
        }
        OpCode::Function => {
          let f = self.read_function();
          self.push(f);
        }
        OpCode::GetGlobal => {
          let id = unsafe { self.pop().value.id };
          if let Some(value) = self.globals.get(&id) {
            self.push(*value);
          } else {
            eprint!("trying to access undefined global variable");
            return;
          }
        }
        OpCode::SetGlobal => {
          let id = unsafe { self.pop().value.id };
          let val = self.top();
          self.globals.insert(id, val);
        }
        OpCode::SetLocal => {
          let id = self.read_byte();
          let val = self.top();
          self.stack[self.bp + id as usize] = val;
        }
        OpCode::GetLocal => {
          let id = self.read_byte();
          self.push(self.stack[self.bp + id as usize]);
        }
        OpCode::AddNum => {
          binary_operation!(self, number,number, ValueType::Number, +);
        }
        OpCode::SubNum => {
          binary_operation!(self, number,number, ValueType::Number, -);
        }
        OpCode::MulNum => {
          binary_operation!(self, number,number, ValueType::Number, *);
        }
        OpCode::DivNum => {
          binary_operation!(self, number,number, ValueType::Number, /);
        }
        OpCode::LeNum => {
          binary_operation!(self, number, boolean, ValueType::Bool, <);
        }
        OpCode::GeNum => {
          binary_operation!(self, number, boolean, ValueType::Bool, >);
        }
        OpCode::LeqNum => {
          binary_operation!(self, number, boolean, ValueType::Bool, <=);
        }
        OpCode::GeqNum => {
          binary_operation!(self, number, boolean, ValueType::Bool, >=);
        }
        OpCode::SameNum => {
          binary_operation!(self, number, boolean, ValueType::Bool, ==);
        }
        OpCode::DiffNum => {
          binary_operation!(self, number, boolean, ValueType::Bool, !=);
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
        OpCode::NotBool => {
          unary_operation!(self, boolean, ValueType::Bool, !);
        }
        OpCode::Call => {
          let arguments = self.read_byte();
          let func = unsafe {
            self.stack[self.stack.len() - 1 - arguments as usize]
              .value
              .function
          };
          self.call_stack.push(CallFrame {
            pc: self.pc,
            function: self.function,
            bp: self.bp,
          });
          self.function = func;
          self.pc = unsafe { (*func).code.code.as_ptr() };
          self.bp = self.stack.len() - arguments as usize;
        }
        OpCode::Print => {
          let mut top = self.pop();
          println!("{}", top.to_string());
          unsafe { top.free() };
        }
        OpCode::Pop => {
          self.pop();
          //unsafe { top.free() };
        }
        OpCode::JumpIfFalsePop => {
          let condition = unsafe { self.pop().value.boolean };
          let target = u16::from_ne_bytes([self.read_byte(), self.read_byte()]) as usize;
          if !condition {
            unsafe { self.pc = self.pc.add(target) }
          }
        }
        OpCode::JumpIfFalseNoPop => {
          let condition = unsafe { self.top().value.boolean };
          let target = u16::from_ne_bytes([self.read_byte(), self.read_byte()]) as usize;
          if !condition {
            unsafe { self.pc = self.pc.add(target) }
          }
        }
        OpCode::Jump => {
          let target = u16::from_ne_bytes([self.read_byte(), self.read_byte()]) as usize;
          unsafe { self.pc = self.pc.add(target) }
          self.bytes_read += target;
        }
        OpCode::BackJump => {
          let target = u16::from_ne_bytes([self.read_byte(), self.read_byte()]) as usize;
          unsafe { self.pc = self.pc.sub(target) }
          self.bytes_read -= target;
        }
        OpCode::Return => {
          if let Some(frame) = self.call_stack.pop() {
            self.pc = frame.pc;
            self.bp = frame.bp;
            self.function = frame.function;
          } else {
            return;
          }
        }
        other => todo!("{other:?} not implemented"),
      }
    }
  }

  pub fn interpret(&mut self, chunk: Chunk) {
    self.pc = chunk.code.as_ptr();
    let func = Function { code: chunk };
    self.function = std::ptr::addr_of!(func);
    self.run()
  }

  pub fn new() -> Self {
    Self {
      pc: std::ptr::null(),
      bp: 0,
      function: std::ptr::null(),
      globals: HashMap::new(),
      bytes_read: 0,
      stack: Vec::new(),
      call_stack: Vec::new(),
    }
  }
}
