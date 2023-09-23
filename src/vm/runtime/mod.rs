use std::mem::ManuallyDrop;
use std::ptr;

use crate::compiler::codegen::bytecode::OpCode;
use crate::vm::chunk::GlobalChunk;
use crate::vm::extern_function::ExternFunction;
use crate::vm::value::{Value, ValueType};

use self::call_frame::{CallFrame, EMPTY_CALL_FRAME};
use self::gc::GC;

use super::{chunk::Function, value::TaggedValue};

mod call_frame;
pub mod gc;

const MAX_CALLS: usize = 64;
const MAX_LOCALS: usize = u8::MAX as usize;
const MAX_STACK: usize = MAX_CALLS * MAX_LOCALS;

macro_rules! binary_operation {
  ($s:ident, $operand:ident, $result:ident, $kind:expr, $op:tt) => {
    let rhs = unsafe {$s.pop().value.$operand};
    let lhs = unsafe {$s.pop().value.$operand};
	  $s.push(TaggedValue{ kind: $kind, value: Value{ $result: lhs $op rhs }});
  };
}

macro_rules! binary_string_comp {
  ($f:ident, $op:tt) => {
    let rhs = unsafe {&(*$f.pop().value.object).value.string};
    let lhs = unsafe {&(*$f.pop().value.object).value.string};
    $f.push(TaggedValue{ kind: ValueType::Bool, value: Value { boolean: *lhs $op *rhs}});
  };
}

macro_rules! unary_operation {
  ($s:ident, $t:ident, $kind:expr, $op:tt) => {
	  let rhs = unsafe {$s.pop().value.$t};
	  $s.push(TaggedValue{ kind: $kind, value: Value{ $t: $op rhs }});
	};
}

pub struct RunTime {
  stack: [TaggedValue; MAX_STACK],
  call_stack: [CallFrame; MAX_CALLS],
  gc: GC,
  function_call: usize,
  globals: Vec<TaggedValue>,
  functions: Vec<Function>,
  pub extern_functions: Vec<ExternFunction>,
}

impl RunTime {
  fn run(&mut self, global_env: &Function) {
    let function = std::ptr::addr_of!(*global_env);
    let mut frame = CallFrame {
      bp: self.stack.as_mut_ptr(),
      sp: self.stack.as_mut_ptr(),
      pc: global_env.code.code.as_ptr(),
      function,
      captures: ptr::null_mut(),
    };
    loop {
      if self.gc.should_run() {
        self.gc.mark(self.stack.iter());
        self.gc.mark(self.globals.iter());
        unsafe { self.gc.sweep() };
      }
      if frame.overflowed_stack() {
        eprintln!("STACK OVERFLOW");
      }
      let op = frame.read_instruction();
      match op {
        OpCode::Constant => {
          let c = frame.read_constant();
          frame.push(c);
        }
        OpCode::ConstantStr => {
          let const_str = frame.read_string_constant();
          let allocated_string = self.gc.alloc_string(const_str.clone());
          frame.push(TaggedValue::object(allocated_string))
        }
        OpCode::MakeClosure => {
          let func = frame.pop();
          debug_assert!(func.kind == ValueType::FunctionId);
          let captures = frame.read_byte();
          frame.push(TaggedValue::object(self.gc.alloc_closure(
            ptr::addr_of!(self.functions[unsafe { func.value.id }]),
            captures as usize,
          )))
        }
        OpCode::GetGlobal => {
          let id = unsafe { frame.pop().value.id };
          let value = unsafe { self.globals.get_unchecked(id) };
          if value.kind == ValueType::None {
            panic!("trying to access undefined global variable");
          }
          frame.push(*value);
        }
        OpCode::SetGlobal => {
          debug_assert!(frame.top().kind == ValueType::GlobalId);
          let id = unsafe { frame.pop().value.id };
          let val = frame.top();
          self.globals[id as usize] = val;
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
          unsafe {
            (*(*frame.top().value.object).value.closure)
              .captures
              .push(val)
          }
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
          let rhs = unsafe { &(*frame.pop().value.object).value.string };
          let lhs = unsafe { &(*frame.pop().value.object).value.string };
          frame.push(TaggedValue::object(
            self
              .gc
              .alloc_string(ManuallyDrop::into_inner(lhs.clone()) + rhs),
          ));
        }
        OpCode::LeStr => {
          binary_string_comp!(frame, <);
        }
        OpCode::GeStr => {
          binary_string_comp!(frame, >);
        }
        OpCode::LeqStr => {
          binary_string_comp!(frame, <=);
        }
        OpCode::GeqStr => {
          binary_string_comp!(frame, >=);
        }
        OpCode::SameStr => {
          binary_string_comp!(frame, ==);
        }
        OpCode::DiffStr => {
          binary_string_comp!(frame, !=);
        }
        OpCode::SameBool => {
          binary_operation!(frame, boolean, boolean, ValueType::Bool, ==);
        }
        OpCode::DiffBool => {
          binary_operation!(frame, boolean, boolean, ValueType::Bool, !=);
        }
        OpCode::NotBool => {
          unary_operation!(frame, boolean, ValueType::Bool, !);
        }
        OpCode::Call => {
          let arguments = frame.read_byte() as usize;
          let function_value = frame.pop();
          let (function, captures) = match function_value.kind {
            ValueType::FunctionId => (
              ptr::addr_of!(self.functions[unsafe { function_value.value.id }]),
              ptr::null_mut(),
            ),
            ValueType::Object => (
              unsafe { (*function_value.value.object).value.closure.function },
              unsafe {
                (*(*function_value.value.object).value.closure)
                  .captures
                  .as_mut_ptr()
              },
            ),
            ValueType::ExternFunctionId => {
              let args = unsafe { &*ptr::slice_from_raw_parts(frame.sp.sub(arguments), arguments) };
              let id = unsafe { function_value.value.id as usize };
              frame.pop_n(arguments);
              frame.push(self.extern_functions[id](args));
              continue;
            }
            _ => unreachable!(),
          };
          let bp = unsafe { frame.sp.sub(arguments) };
          let pc = unsafe { (*function).code.code.as_ptr() };
          let sp = frame.sp;
          frame.sp = unsafe { frame.sp.sub(arguments) };
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
        OpCode::Construct => {
          let n_members = frame.read_byte() as usize;
          let mut members = Vec::with_capacity(n_members);
          for _ in 0..n_members {
            members.push(frame.pop());
          }
          // FIXME: remove this reverse
          members.reverse();
          frame.push(TaggedValue::object(self.gc.alloc_aggregate(members)));
        }
        OpCode::GetMember => {
          let aggregate = unsafe { &mut (*frame.pop().value.object).value.aggregate };
          let id = frame.read_byte();
          let val = aggregate.members[id as usize];
          frame.push(val);
        }
        OpCode::SetMember => {
          let id = frame.read_byte();
          let val = frame.pop();
          let aggregate = unsafe { &mut (*frame.pop().value.object).value.aggregate };
          aggregate.members[id as usize] = val;
          frame.push(val);
        }
        OpCode::Pop => {
          frame.pop();
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

  pub unsafe fn interpret(&mut self, global_chunk: GlobalChunk, globals_count: u32) {
    self.globals.resize(
      self.globals.len() + globals_count as usize,
      TaggedValue::none(),
    );
    self.functions.extend(global_chunk.functions);
    self.run(&global_chunk.global_code);
  }
}

impl Default for RunTime {
  fn default() -> Self {
    Self {
      stack: [TaggedValue::none(); MAX_STACK],
      call_stack: [EMPTY_CALL_FRAME; MAX_CALLS],
      extern_functions: Default::default(),
      function_call: 0,
      gc: Default::default(),
      globals: Default::default(),
      functions: Default::default(),
    }
  }
}
