use crate::compiler::codegen::bytecode::OpCode;
use crate::vm::runtime::RuntimeError;
use crate::vm::runtime::RuntimeError::StackOverflow;
use crate::vm::{chunk::Function, runtime::MAX_LOCALS, value::TaggedValue};

#[derive(Clone, Copy)]
pub struct CallFrame {
  pub bp: *mut TaggedValue,
  pub sp: *mut TaggedValue,
  pub pc: *const u8,
  pub function: *const Function,
  pub captures: *mut TaggedValue,
}

pub const EMPTY_CALL_FRAME: CallFrame = CallFrame {
  bp: std::ptr::null_mut(),
  sp: std::ptr::null_mut(),
  pc: std::ptr::null(),
  function: std::ptr::null(),
  captures: std::ptr::null_mut(),
};

impl CallFrame {
  pub fn read_byte(&mut self) -> u8 {
    debug_assert!(!self.pc.is_null());
    let val = unsafe { *self.pc };
    unsafe { self.pc = self.pc.add(1) };
    val
  }

  pub fn overflowed_stack(&self) -> bool {
    debug_assert!((self.bp as usize) <= (self.sp as usize));
    self.sp as usize - self.bp as usize > MAX_LOCALS
  }

  pub fn read_instruction(&mut self) -> OpCode {
    let instruction = self.read_byte();
    debug_assert!(instruction < OpCode::Last as u8);
    unsafe { std::mem::transmute::<u8, OpCode>(instruction) }
  }

  pub fn pop(&mut self) -> TaggedValue {
    unsafe { self.sp = self.sp.sub(1) }
    unsafe { *self.sp }
  }

  pub fn pop_n(&mut self, n: usize) {
    unsafe { self.sp = self.sp.sub(n) }
  }

  pub unsafe fn push_no_overflow(&mut self, val: TaggedValue) {
    debug_assert!(!self.overflowed_stack());
    std::ptr::write(self.sp, val);
    self.sp = self.sp.add(1);
  }

  pub fn push(&mut self, val: TaggedValue) -> Result<(), RuntimeError> {
    if self.overflowed_stack() {
      Err(StackOverflow)
    } else {
      unsafe { self.push_no_overflow(val) };
      Ok(())
    }
  }

  pub fn top(&self) -> TaggedValue {
    unsafe { *self.sp.sub(1) }
  }

  pub fn read_constant(&mut self) -> TaggedValue {
    let index = self.read_byte();
    unsafe { (*self.function).code.get_constant(index as usize) }
  }

  pub fn read_string_constant(&mut self) -> &String {
    let index = self.read_byte();
    unsafe { (*self.function).code.get_string_constant(index as usize) }
  }

  pub fn get_stack_value(&self, offset: u8) -> TaggedValue {
    unsafe { *self.bp.add(offset as usize) }
  }

  pub fn set_stack_value(&mut self, offset: u8, val: TaggedValue) {
    unsafe { *self.bp.add(offset as usize) = val }
  }
}
