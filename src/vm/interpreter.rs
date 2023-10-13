use std::mem::ManuallyDrop;
use std::ptr;

use crate::api;
use crate::compiler::codegen::bytecode::OpCode;
use crate::foreign_function::ForeignFunction;
use crate::vm::call_frame::CallFrame;
use crate::vm::chunk::Function;
use crate::vm::gc::GC;
use crate::vm::value::{TaggedValue, Value, ValueType};
use crate::vm::MAX_CALLS;

macro_rules! binary_operation {
  ($s:ident, $operand:ident, $result:ident, $kind:expr, $op:tt) => {
    let rhs = unsafe {$s.pop().value.$operand};
    let lhs = unsafe {$s.pop().value.$operand};
    // popped two values and pushed only one, the size of the stack is 1 less
    unsafe {$s.push_no_overflow(TaggedValue{ kind: $kind, value: Value{ $result: lhs $op rhs }})};
  };
}

macro_rules! binary_string_comp {
  ($f:ident, $op:tt) => {
    let rhs = unsafe {$f.pop().as_object().as_string()};
    let lhs = unsafe {$f.pop().as_object().as_string()};
    // popped two values and pushed only one, the size of the stack is 1 less
    unsafe {$f.push_no_overflow(TaggedValue{ kind: ValueType::Bool, value: Value { boolean: *lhs $op *rhs}})};
  };
}

macro_rules! unary_operation {
  ($s:ident, $t:ident, $kind:expr, $op:tt) => {
	  let rhs = unsafe {$s.pop().value.$t};
    // popped and pushed a value, the size of the stack does not change
	  unsafe {$s.push_no_overflow(TaggedValue{ kind: $kind, value: Value{ $t: $op rhs }})};
	};
}

pub enum RuntimeError {
  StackOverflow,
}

pub fn interpret(
  function: &Function,
  stack: &mut [TaggedValue],
  call_stack: &mut [CallFrame],
  gc: &mut GC,
  globals: &mut [TaggedValue],
  functions: &[Function],
  foreign_functions: &[ForeignFunction],
) -> Result<(), RuntimeError> {
  let bp = stack.as_mut_ptr();
  let mut frame = CallFrame {
    bp,
    sp: bp,
    pc: function.code.code.as_ptr(),
    function: ptr::addr_of!(*function),
    captures: ptr::null_mut(),
  };
  let mut function_call = 0;
  loop {
    // if gc.should_run() {
    //   gc.mark(stack.iter());
    //   gc.mark(globals.iter());
    //   unsafe { gc.sweep() };
    // }
    let op = frame.read_instruction();
    match op {
      OpCode::Constant => {
        let c = frame.read_constant();
        frame.push(c)?;
      }
      OpCode::ConstantStr => {
        let const_str = frame.read_string_constant();
        let allocated_string = gc.alloc_string(const_str.clone());
        frame.push(TaggedValue::object(allocated_string))?;
      }
      OpCode::MakeClosure => make_closure(&mut frame, functions, gc),
      OpCode::GetGlobal => {
        let id = unsafe { frame.pop().as_id() };
        let value = &globals[id];
        debug_assert!(value.kind != ValueType::None, "undefined global variable");
        // we popped `id`, so the final stack size is the same
        unsafe { frame.push_no_overflow(*value) };
      }
      OpCode::SetGlobal => {
        debug_assert!(frame.top().kind == ValueType::GlobalId);
        let id = unsafe { frame.pop().as_id() };
        let val = frame.top();
        globals[id] = val;
      }
      OpCode::SetLocal => {
        let id = frame.read_byte();
        let val = frame.top();
        frame.set_stack_value(id, val);
      }
      OpCode::GetLocal => {
        let id = frame.read_byte();
        frame.push(frame.get_stack_value(id))?;
      }
      OpCode::Capture => {
        let val = frame.pop();
        unsafe {
          frame.top().as_object().as_closure().captures.push(val);
        }
      }
      OpCode::GetCapture => {
        debug_assert!(!frame.captures.is_null());
        let id = frame.read_byte();
        let val = unsafe { *frame.captures.offset(id as isize) };
        frame.push(val)?;
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
        let rhs = unsafe { frame.pop().as_object().as_string() };
        let lhs = unsafe { frame.pop().as_object().as_string() };
        let string =
          TaggedValue::object(gc.alloc_string(ManuallyDrop::into_inner(lhs.clone()) + rhs));
        // popped two values and pushed only one, the size of the stack is 1 less
        unsafe { frame.push_no_overflow(string) };
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
      OpCode::CallNative => call_native(&mut frame, call_stack, functions, &mut function_call)?,
      OpCode::CallForeign => call_foreign(&mut frame, foreign_functions, gc)?,
      OpCode::CallValue => call_value(
        &mut frame,
        call_stack,
        functions,
        foreign_functions,
        &mut function_call,
        gc,
      )?,
      OpCode::Construct => construct(&mut frame, gc)?,
      OpCode::GetMember => {
        let aggregate = unsafe { frame.pop().as_object().as_aggregate() };
        let id = frame.read_byte();
        let val = aggregate.members[id as usize];
        // we popped `aggregate`, maintaining the stack size
        unsafe { frame.push_no_overflow(val) };
      }
      OpCode::SetMember => {
        let id = frame.read_byte();
        let val = frame.pop();
        let aggregate = unsafe { frame.pop().as_object().as_aggregate() };
        aggregate.members[id as usize] = val;
        frame.push(val)?;
      }
      OpCode::Pop => {
        frame.pop();
      }
      OpCode::JumpIfFalsePop => {
        let condition = unsafe { frame.pop().as_bool() };
        let target = u16::from_ne_bytes([frame.read_byte(), frame.read_byte()]) as usize;
        if !condition {
          unsafe { frame.pc = frame.pc.add(target) }
        }
      }
      OpCode::JumpIfFalseNoPop => {
        let condition = unsafe { frame.top().as_bool() };
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
        if function_call == 0 {
          return Ok(());
        } else {
          let ret = frame.pop();
          function_call -= 1;
          frame = call_stack[function_call];
          frame.push(ret)?;
        }
      }
      other => todo!("{other:?} not implemented"),
    }
  }
}

fn call_native(
  frame: &mut CallFrame,
  call_stack: &mut [CallFrame],
  functions: &[Function],
  function_call: &mut usize,
) -> Result<(), RuntimeError> {
  let arguments = frame.read_byte() as usize;
  let function_value = frame.pop();
  debug_assert!(function_value.kind == ValueType::FunctionId);
  let function_id = unsafe { function_value.as_id() };
  let function = ptr::addr_of!(functions[function_id]);
  let bp = unsafe { frame.sp.sub(arguments) };
  let pc = unsafe { (*function).code.code.as_ptr() };
  let sp = frame.sp;
  frame.sp = unsafe { frame.sp.sub(arguments) };
  call_stack[*function_call] = *frame;
  *function_call += 1;
  if *function_call >= MAX_CALLS {
    eprintln!("STACK OVERFLOW!");
    return Err(RuntimeError::StackOverflow);
  }
  *frame = CallFrame {
    bp,
    sp,
    pc,
    function,
    captures: ptr::null_mut(),
  };
  Ok(())
}

fn make_closure(frame: &mut CallFrame, functions: &[Function], gc: &mut GC) {
  let func = frame.pop();
  debug_assert!(func.kind == ValueType::FunctionId);
  let captures = frame.read_byte();
  let closure = TaggedValue::object(gc.alloc_closure(
    ptr::addr_of!(functions[unsafe { func.as_id() }]),
    captures as usize,
  ));
  // we popped `func`, so the final stack size is the same
  unsafe { frame.push_no_overflow(closure) };
}

fn call_foreign(
  frame: &mut CallFrame,
  foreign_functions: &[ForeignFunction],
  gc: &mut GC,
) -> Result<(), RuntimeError> {
  let arguments = frame.read_byte() as usize;
  let function_value = frame.pop();
  debug_assert!(function_value.kind == ValueType::ForeignFunctionId);
  let args = unsafe { &*ptr::slice_from_raw_parts(frame.sp.sub(arguments), arguments) };
  let id = unsafe { function_value.as_id() };
  frame.pop_n(arguments);
  frame.push(foreign_functions[id](api::gc::Gc(gc), args))?;
  Ok(())
}

fn call_closure(
  frame: &mut CallFrame,
  call_stack: &mut [CallFrame],
  function_call: &mut usize,
) -> Result<(), RuntimeError> {
  let arguments = frame.read_byte() as usize;
  let function_value = frame.pop();
  let closure = unsafe { function_value.as_object().as_closure() };
  let function = closure.function;
  let captures = closure.captures.as_mut_ptr();
  let bp = unsafe { frame.sp.sub(arguments) };
  let pc = unsafe { (*function).code.code.as_ptr() };
  let sp = frame.sp;
  frame.sp = unsafe { frame.sp.sub(arguments) };
  call_stack[*function_call] = *frame;
  *function_call += 1;
  if *function_call >= MAX_CALLS {
    eprintln!("STACK OVERFLOW!");
    return Err(RuntimeError::StackOverflow);
  }
  *frame = CallFrame {
    bp,
    sp,
    pc,
    function,
    captures,
  };
  Ok(())
}

fn call_value(
  frame: &mut CallFrame,
  call_stack: &mut [CallFrame],
  functions: &[Function],
  foreign_functions: &[ForeignFunction],
  function_call: &mut usize,
  gc: &mut GC,
) -> Result<(), RuntimeError> {
  match frame.top().kind {
    ValueType::FunctionId => call_native(frame, call_stack, functions, function_call),
    ValueType::ForeignFunctionId => call_foreign(frame, foreign_functions, gc),
    ValueType::Object => call_closure(frame, call_stack, function_call),
    _ => unreachable!(),
  }
}

fn construct(frame: &mut CallFrame, gc: &mut GC) -> Result<(), RuntimeError> {
  let n_members = frame.read_byte() as usize;
  let mut members = Vec::with_capacity(n_members);
  for _ in 0..n_members {
    members.push(frame.pop());
  }
  // FIXME: remove this reverse
  members.reverse();
  // n_member could be 0, leading to a stack that's bigger than what we had before this operation
  frame.push(TaggedValue::object(gc.alloc_aggregate(members)))
}
