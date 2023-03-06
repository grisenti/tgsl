use super::ast::{Literal, Operator, AST};
use core::fmt::Debug;
use std::alloc::Layout;

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum OpCode {
  Return,

  Constant,
  Function,

  GetGlobal,
  SetGlobal,
  GetLocal,
  SetLocal,

  MakeClosure,
  Capture,
  GetCapture,
  SetCapture,

  // primitive operations
  // numbers
  NegNum,
  AddNum,
  SubNum,
  MulNum,
  DivNum,
  LeNum,
  GeNum,
  LeqNum,
  GeqNum,
  SameNum,
  DiffNum,

  // strings
  AddStr,
  LeStr,
  GeStr,
  LeqStr,
  GeqStr,
  SameStr,
  DiffStr,

  // bool
  NotBool,
  SameBool,
  DiffBool,

  // functions
  Call,

  // structs
  Construct,
  GetMember,
  SetMember,

  Jump,
  BackJump,
  JumpIfFalsePop,
  JumpIfFalseNoPop,

  Print,
  Pop,

  Last,
}

impl OpCode {
  pub fn from_numeric_operator(op: Operator) -> Self {
    match op {
      Operator::Basic('+') => OpCode::AddNum,
      Operator::Basic('-') => OpCode::SubNum,
      Operator::Basic('*') => OpCode::MulNum,
      Operator::Basic('/') => OpCode::DivNum,
      Operator::Basic('<') => OpCode::LeNum,
      Operator::Basic('>') => OpCode::GeNum,
      Operator::Geq => OpCode::GeqNum,
      Operator::Leq => OpCode::LeqNum,
      Operator::Same => OpCode::SameNum,
      Operator::Different => OpCode::DiffNum,
      _ => panic!(),
    }
  }

  pub fn from_string_comp_operator(op: Operator) -> Self {
    match op {
      Operator::Basic('<') => OpCode::LeStr,
      Operator::Basic('>') => OpCode::GeStr,
      Operator::Geq => OpCode::GeqStr,
      Operator::Leq => OpCode::LeqStr,
      Operator::Same => OpCode::SameStr,
      Operator::Different => OpCode::DiffStr,
      _ => panic!(),
    }
  }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ValueType {
  Number,
  Bool,
  String,
  GlobalId,
  Function,
  ExternFunctionId,
  Closure,
  Aggregate,
  Object,

  None,
}

#[derive(Clone)]
pub struct Function {
  pub code: Chunk,
}

#[derive(Clone)]
pub struct Closure {
  pub function: *const Function,
  pub captures: Vec<TaggedValue>,
}

pub struct Aggregate {
  pub members: Vec<TaggedValue>,
}

#[derive(Clone, Copy)]
pub union Value {
  pub number: f64,
  pub id: u16,
  pub boolean: bool,
  pub string: *mut String,
  pub function: *const Function,
  pub closure: *mut Closure,
  pub aggregate: *mut Aggregate,
  pub none: (),
}

#[derive(Clone, Copy)]
pub struct TaggedValue {
  pub kind: ValueType,
  pub value: Value,
}

unsafe fn alloc_object<T>(value: T) -> *mut T {
  let ptr = std::alloc::alloc(Layout::new::<T>()) as *mut T;
  std::ptr::write(ptr, value);
  ptr
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
          string: unsafe { alloc_object(s.get(ast).to_string()) },
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

  pub fn global_id(id: u16) -> Self {
    Self {
      kind: ValueType::GlobalId,
      value: Value { id },
    }
  }

  pub fn function(func: *const Function) -> Self {
    Self {
      kind: ValueType::Function,
      value: Value { function: func },
    }
  }

  pub fn closure(function: *const Function, captures: usize) -> Self {
    Self {
      kind: ValueType::Closure,
      value: Value {
        closure: unsafe {
          alloc_object(Closure {
            function,
            captures: Vec::with_capacity(captures),
          })
        },
      },
    }
  }

  pub fn aggregate(members: Vec<TaggedValue>) -> Self {
    Self {
      kind: ValueType::Aggregate,
      value: Value {
        aggregate: unsafe { alloc_object(Aggregate { members }) },
      },
    }
  }

  pub fn none() -> Self {
    Self {
      kind: ValueType::None,
      value: Value { none: () },
    }
  }

  pub unsafe fn free(&mut self) {
    match self.kind {
      ValueType::String => {
        self.value.string.drop_in_place();
        unsafe { std::alloc::dealloc(self.value.string as *mut u8, Layout::new::<String>()) }
      }
      ValueType::Closure => {
        self.value.closure.drop_in_place();
        unsafe { std::alloc::dealloc(self.value.closure as *mut u8, Layout::new::<Closure>()) }
      }
      _ => {}
    }
  }

  pub fn copy_object(&self) -> Self {
    let value = match self.kind {
      ValueType::String => unsafe {
        Value {
          string: alloc_object((*self.value.string).clone()),
        }
      },
      ValueType::Closure => unsafe {
        Value {
          closure: alloc_object((*self.value.closure).clone()),
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
      ValueType::GlobalId => unsafe { format!("<global {}>", self.value.id) },
      ValueType::Function => "<function>".to_string(),
      ValueType::None => "<none>".to_string(),
      ValueType::Aggregate => "<struct>".to_string(),
      ValueType::ExternFunctionId => unsafe { format!("<extern {}>", self.value.id) },
      _ => todo!(),
    }
  }
}

impl Debug for TaggedValue {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.to_string())
  }
}

#[derive(Clone)]
pub struct Chunk {
  pub code: Vec<u8>,
  pub functions: Vec<Function>,
  pub constants: Vec<TaggedValue>,
}

impl Chunk {
  pub fn get_constant(&self, index: usize) -> TaggedValue {
    self.constants[index].copy_object()
  }

  pub fn get_function(&self, index: usize) -> TaggedValue {
    TaggedValue::function(std::ptr::addr_of!(self.functions[index]))
  }

  pub fn empty() -> Self {
    Self {
      code: Vec::new(),
      constants: vec![TaggedValue::none()],
      functions: Vec::new(),
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
    let mut result = "".to_string();
    for (i, func) in self.functions.iter().enumerate() {
      result += &format!("++ fn {i} ++\n{:?}-- fn {i} --\n", func.code);
    }
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
        OpCode::Function => {
          index += 1;
          result += &format!("Function: {}\n", self.code[index]);
        }
        OpCode::Call => {
          index += 1;
          result += &format!("Call: {}\n", self.code[index]);
        }
        OpCode::JumpIfFalsePop | OpCode::Jump | OpCode::JumpIfFalseNoPop => {
          index += 2;
          let jump_point = u16::from_ne_bytes([self.code[index - 1], self.code[index]]);
          result += &format!("{code:?}: {}\n", index + 1 + jump_point as usize);
        }
        OpCode::BackJump => {
          index += 2;
          let jump_point = u16::from_ne_bytes([self.code[index - 1], self.code[index]]);
          result += &format!("{code:?}: {}\n", index + 1 - jump_point as usize);
        }
        OpCode::GetLocal
        | OpCode::SetLocal
        | OpCode::GetCapture
        | OpCode::SetCapture
        | OpCode::GetMember
        | OpCode::SetMember
        | OpCode::Construct => {
          index += 1;
          result += &format!("{code:?}: {}\n", self.code[index])
        }
        OpCode::MakeClosure => {
          index += 1;
          result += &format!("MakeClosure, captures: {}\n", self.code[index])
        }
        code => result += &format!("{code:?}\n"),
      }
      index += 1;
    }
    write!(f, "{result}")
  }
}
