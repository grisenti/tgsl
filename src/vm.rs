use std::collections::HashMap;

use crate::compiler::bytecode::{Chunk, Closure, Function, OpCode, TaggedValue, Value, ValueType};

const MAX_CALLS: usize = 64;
const MAX_LOCALS: usize = u8::MAX as usize;
const MAX_STACK: usize = MAX_CALLS * MAX_LOCALS;

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

#[derive(Clone, Copy)]
struct CallFrame {
  bp: *mut TaggedValue,
  sp: *mut TaggedValue,
  pc: *const u8,
  function: *const Function,
  captures: *mut TaggedValue,
}

const EMPTY_CALL_FRAME: CallFrame = CallFrame {
  bp: std::ptr::null_mut(),
  sp: std::ptr::null_mut(),
  pc: std::ptr::null(),
  function: std::ptr::null(),
  captures: std::ptr::null_mut(),
};

impl CallFrame {
  fn read_byte(&mut self) -> u8 {
    debug_assert!(!self.pc.is_null());
    let val = unsafe { *self.pc };
    unsafe { self.pc = self.pc.add(1) };
    val
  }

  fn read_instruction(&mut self) -> Result<OpCode, &'static str> {
    if self.sp as usize - self.bp as usize > MAX_LOCALS {
      Err("STACK OVERFLOW")
    } else {
      let instruction = self.read_byte();
      debug_assert!(instruction < OpCode::Last as u8);
      Ok(unsafe { std::mem::transmute::<u8, OpCode>(instruction) })
    }
  }

  fn pop(&mut self) -> TaggedValue {
    unsafe { self.sp = self.sp.sub(1) }
    unsafe { *self.sp }
  }

  fn push(&mut self, val: TaggedValue) {
    unsafe {
      std::ptr::write(self.sp, val);
      self.sp = self.sp.add(1);
    }
  }

  fn top(&self) -> TaggedValue {
    unsafe { *self.sp.sub(1) }
  }

  fn read_constant(&mut self) -> TaggedValue {
    let index = self.read_byte();
    unsafe { (*self.function).code.get_constant(index as usize) }
  }

  fn read_function(&mut self) -> TaggedValue {
    let index = self.read_byte();
    unsafe { (*self.function).code.get_function(index as usize) }
  }

  fn get_stack_value(&self, offset: u8) -> TaggedValue {
    unsafe { *self.bp.add(offset as usize) }
  }

  fn set_stack_value(&mut self, offset: u8, val: TaggedValue) {
    unsafe { *self.bp.add(offset as usize) = val }
  }
}

pub struct VM {
  stack: [TaggedValue; MAX_STACK],
  call_stack: [CallFrame; MAX_CALLS],
  function_call: usize,
  globals: HashMap<u16, TaggedValue>,
}

impl VM {
  fn run(&mut self, frame: CallFrame) {
    let mut frame = frame;
    loop {
      let op = match frame.read_instruction() {
        Ok(op) => op,
        Err(err) => {
          eprintln!("{}", err);
          return;
        }
      };
      match op {
        OpCode::Constant => {
          let c = frame.read_constant();
          frame.push(c);
        }
        OpCode::Function => {
          let f = frame.read_function();
          frame.push(f);
        }
        OpCode::MakeClosure => {
          let func = frame.pop();
          debug_assert!(func.kind == ValueType::Function);
          let captures = frame.read_byte();
          frame.push(TaggedValue::capture(
            unsafe { func.value.function },
            captures as usize,
          ))
        }
        OpCode::GetGlobal => {
          let id = unsafe { frame.pop().value.id };
          if let Some(value) = self.globals.get(&id) {
            frame.push(*value);
          } else {
            eprint!("trying to access undefined global variable");
            return;
          }
        }
        OpCode::SetGlobal => {
          debug_assert!(frame.top().kind == ValueType::GlobalId);
          let id = unsafe { frame.pop().value.id };
          let val = frame.top();
          self.globals.insert(id, val);
        }
        OpCode::SetLocal => {
          let id = frame.read_byte();
          let val = frame.top();
          frame.set_stack_value(id, val);
        }
        OpCode::GetLocal => {
          let id = frame.read_byte();
          frame.push(frame.get_stack_value(id));
        }
        OpCode::Capture => {
          let val = frame.pop();
          unsafe { (*frame.top().value.closure).captures.push(val) }
        }
        OpCode::GetCapture => {
          debug_assert!(!frame.captures.is_null());
          let id = frame.read_byte();
          let val = unsafe { *frame.captures.offset(id as isize) };
          frame.push(val);
        }
        OpCode::SetCapture => {
          let id = frame.read_byte();
          let val = frame.top();
          unsafe { *frame.captures.offset(id as isize) = val };
        }
        OpCode::AddNum => {
          binary_operation!(frame, number,number, ValueType::Number, +);
        }
        OpCode::SubNum => {
          binary_operation!(frame, number,number, ValueType::Number, -);
        }
        OpCode::MulNum => {
          binary_operation!(frame, number,number, ValueType::Number, *);
        }
        OpCode::DivNum => {
          binary_operation!(frame, number,number, ValueType::Number, /);
        }
        OpCode::LeNum => {
          binary_operation!(frame, number, boolean, ValueType::Bool, <);
        }
        OpCode::GeNum => {
          binary_operation!(frame, number, boolean, ValueType::Bool, >);
        }
        OpCode::LeqNum => {
          binary_operation!(frame, number, boolean, ValueType::Bool, <=);
        }
        OpCode::GeqNum => {
          binary_operation!(frame, number, boolean, ValueType::Bool, >=);
        }
        OpCode::SameNum => {
          binary_operation!(frame, number, boolean, ValueType::Bool, ==);
        }
        OpCode::DiffNum => {
          binary_operation!(frame, number, boolean, ValueType::Bool, !=);
        }
        OpCode::NegNum => {
          unary_operation!(frame, number, ValueType::Number, -);
        }
        OpCode::AddStr => {
          let mut rhs = frame.pop();
          let mut lhs = frame.pop();
          let rhs_str = unsafe { rhs.value.string };
          let lhs_str = unsafe { lhs.value.string };
          frame.push(TaggedValue {
            kind: ValueType::String,
            value: Value {
              string: Box::into_raw(Box::new(unsafe { (*lhs_str).clone() + &*rhs_str })),
            },
          });
          unsafe { rhs.free() };
          unsafe { lhs.free() };
        }
        OpCode::NotBool => {
          unary_operation!(frame, boolean, ValueType::Bool, !);
        }
        OpCode::Call => {
          let arguments = frame.read_byte() as usize;
          let function_value = unsafe { *frame.sp.sub(arguments + 1) };
          let (function, captures) = if function_value.kind == ValueType::Function {
            (
              unsafe { function_value.value.function },
              std::ptr::null_mut(),
            )
          } else {
            (
              unsafe { (*function_value.value.closure).function },
              unsafe { (*function_value.value.closure).captures.as_mut_ptr() },
            )
          };
          let bp = unsafe { frame.sp.sub(arguments) };
          let pc = unsafe { (*function).code.code.as_ptr() };
          let sp = frame.sp;
          frame.sp = unsafe { frame.sp.sub(1 + arguments) };
          self.call_stack[self.function_call] = frame;
          self.function_call += 1;
          if self.function_call >= MAX_CALLS {
            eprintln!("STACK OVERFLOW!");
            return;
          }
          frame = CallFrame {
            bp,
            sp,
            pc,
            function,
            captures,
          };
        }
        OpCode::Print => {
          let mut top = frame.pop();
          println!("{}", top.to_string());
          unsafe { top.free() };
        }
        OpCode::Pop => {
          frame.pop();
          //unsafe { top.free() };
        }
        OpCode::JumpIfFalsePop => {
          let condition = unsafe { frame.pop().value.boolean };
          let target = u16::from_ne_bytes([frame.read_byte(), frame.read_byte()]) as usize;
          if !condition {
            unsafe { frame.pc = frame.pc.add(target) }
          }
        }
        OpCode::JumpIfFalseNoPop => {
          let condition = unsafe { frame.top().value.boolean };
          let target = u16::from_ne_bytes([frame.read_byte(), frame.read_byte()]) as usize;
          if !condition {
            unsafe { frame.pc = frame.pc.add(target) }
          }
        }
        OpCode::Jump => {
          let target = u16::from_ne_bytes([frame.read_byte(), frame.read_byte()]) as usize;
          unsafe { frame.pc = frame.pc.add(target) }
        }
        OpCode::BackJump => {
          let target = u16::from_ne_bytes([frame.read_byte(), frame.read_byte()]) as usize;
          unsafe { frame.pc = frame.pc.sub(target) }
        }
        OpCode::Return => {
          if self.function_call == 0 {
            return;
          } else {
            let ret = frame.pop();
            self.function_call -= 1;
            frame = self.call_stack[self.function_call];
            frame.push(ret);
          }
        }
        other => todo!("{other:?} not implemented"),
      }
    }
  }

  pub fn interpret(&mut self, chunk: Chunk) {
    let mut func = Function { code: chunk };
    let function = std::ptr::addr_of!(func);
    let global_frame = CallFrame {
      bp: self.stack.as_mut_ptr(),
      sp: self.stack.as_mut_ptr(),
      pc: func.code.code.as_mut_ptr(),
      function,
      captures: std::ptr::null_mut(),
    };
    self.run(global_frame);
  }

  pub fn new() -> Self {
    Self {
      globals: HashMap::new(),
      stack: [TaggedValue::none(); MAX_STACK],
      call_stack: [EMPTY_CALL_FRAME; MAX_CALLS],
      function_call: 0,
    }
  }
}
