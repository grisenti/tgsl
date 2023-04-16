use std::{collections::HashMap, mem::ManuallyDrop};

use crate::{
  compiler::{
    bytecode::{Chunk, ConstantValue, Function, OpCode},
    identifier::ModuleId,
    modules::{GlobalNames, LoadedModules, Module},
    Compiler,
  },
  id_hasher::IdBuildHasher,
  standard_library::load_standard_library,
};

mod gc;
pub mod value;
use gc::GC;
use value::*;

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

  fn read_constant(&mut self) -> &ConstantValue {
    let index = self.read_byte();
    unsafe { (*self.function).code.get_constant(index as usize) }
  }

  fn read_local_function(&mut self) -> TaggedValue {
    let index = self.read_byte();
    unsafe { TaggedValue::function((*self.function).code.get_function(index as usize)) }
  }

  fn get_stack_value(&self, offset: u8) -> TaggedValue {
    unsafe { *self.bp.add(offset as usize) }
  }

  fn set_stack_value(&mut self, offset: u8, val: TaggedValue) {
    unsafe { *self.bp.add(offset as usize) = val }
  }
}

type ExternFunction = Box<dyn Fn(Vec<TaggedValue>) -> TaggedValue>;

pub struct VM {
  state: LoadedModules,
  compiler: Compiler,
  extern_functions: Vec<ExternFunction>,
  stack: [TaggedValue; MAX_STACK],
  call_stack: [CallFrame; MAX_CALLS],
  gc: GC,
  loaded_functions: Vec<Function>,
  function_call: usize,
  globals: HashMap<u16, TaggedValue, IdBuildHasher>,
}

impl VM {
  fn run_gc(&mut self) {
    if self.gc.should_run() {
      self.gc.mark(self.stack.iter());
      self.gc.mark(self.globals.values());
      unsafe { self.gc.sweep() };
    }
  }

  fn run(&mut self, frame: CallFrame) {
    let mut frame = frame;
    loop {
      self.run_gc();
      let op = match frame.read_instruction() {
        Ok(op) => op,
        Err(err) => {
          eprintln!("{err}");
          return;
        }
      };
      match op {
        OpCode::Constant => {
          let c = frame.read_constant();
          let value = TaggedValue::from_constant(c, &mut self.gc);
          frame.push(value);
        }
        OpCode::Function => {
          let f = frame.read_local_function();
          frame.push(f);
        }
        OpCode::MakeClosure => {
          let func = frame.pop();
          debug_assert!(func.kind == ValueType::Function);
          let captures = frame.read_byte();
          frame.push(TaggedValue::object(
            self
              .gc
              .alloc_closure(unsafe { func.value.function }, captures as usize),
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
          let function_value = unsafe { *frame.sp.sub(arguments + 1) };
          let (function, captures) = match function_value.kind {
            ValueType::Function => (
              unsafe { function_value.value.function },
              std::ptr::null_mut(),
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
              let args = (0..arguments).map(|_| frame.pop()).collect();
              let id = unsafe { function_value.value.id as usize };
              frame.push(self.extern_functions[id](args));
              continue;
            }
            _ => unreachable!(),
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
          let aggregate = unsafe { &mut (*frame.pop().value.object).value.aggregate };
          let val = frame.top();
          aggregate.members[id as usize] = val;
        }
        OpCode::Print => {
          let top = frame.pop();
          println!("{}", top.to_string());
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

  pub fn interpret(&mut self, chunk: Chunk) {
    let func = Function { code: chunk };
    let function = std::ptr::addr_of!(func);
    let global_frame = CallFrame {
      bp: self.stack.as_mut_ptr(),
      sp: self.stack.as_mut_ptr(),
      pc: func.code.code.as_ptr(),
      function,
      captures: std::ptr::null_mut(),
    };
    self.run(global_frame);
    self.loaded_functions.push(func);
  }

  fn bind_functions(
    &mut self,
    external_ids: &[u16],
    module_id: ModuleId,
    extern_functions: Vec<(&str, ExternFunction)>,
  ) -> Result<(), String> {
    let mut extern_pair = extern_functions
      .into_iter()
      .filter_map(|(name, func)| {
        self
          .compiler
          .get_global(module_id, name)
          .map(|id| (id, func))
      })
      .collect::<Vec<_>>();
    if extern_pair.len() < external_ids.len() {
      return Err("missing external function in module loading".to_string());
    }
    extern_pair.sort_by_key(|(id, _)| *id);
    for ((id, func), ext_id) in extern_pair.into_iter().zip(external_ids) {
      if id != *ext_id {
        return Err("trying to bind a function that is not marked extern".to_string());
      }
      self.extern_functions.push(func);
    }
    Ok(())
  }

  pub fn load_module(
    &mut self,
    name: String,
    source: &str,
    extern_functions: Vec<(&str, ExternFunction)>,
  ) -> Result<(), String> {
    let Module {
      extern_functions: ext_ids,
      imports: _,
      id,
      code,
    } = match self.compiler.compile(source, &self.state) {
      Err(err) => return Err(err.print_long(source)),
      Ok(module) => module,
    };
    self.bind_functions(&ext_ids, id, extern_functions)?;
    self.state.extern_functions.extend(ext_ids);
    let mod_id = self.state.module_ids.len() as u16;
    self.state.module_ids.insert(name, ModuleId(mod_id));
    self.interpret(code);
    Ok(())
  }

  pub fn new() -> Self {
    let mut vm = Self {
      gc: GC::new(),
      compiler: Compiler::new(),
      state: Default::default(),
      extern_functions: Vec::new(),
      globals: HashMap::default(),
      loaded_functions: Vec::new(),
      stack: [TaggedValue::none(); MAX_STACK],
      call_stack: [EMPTY_CALL_FRAME; MAX_CALLS],
      function_call: 0,
    };
    load_standard_library(&mut vm);
    vm
  }
}
