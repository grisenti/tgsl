use std::{collections, mem::ManuallyDrop};

use crate::compiler::{
  bytecode::{ConstantValue, OpCode},
  codegen::BytecodeBuilder,
};

use super::value::{TaggedValue, Value, ValueType};

union Constant {
  stack_val: TaggedValue,
  string: ManuallyDrop<String>,
}

enum ConstantType {
  StackValue,
  String,
}

pub struct TaggedConstant {
  value: Constant,
  tag: ConstantType,
}

impl From<TaggedValue> for TaggedConstant {
  fn from(value: TaggedValue) -> Self {
    Self {
      value: Constant { stack_val: value },
      tag: ConstantType::StackValue,
    }
  }
}

impl From<String> for TaggedConstant {
  fn from(value: String) -> Self {
    Self {
      value: Constant {
        string: ManuallyDrop::new(value),
      },
      tag: ConstantType::String,
    }
  }
}

impl From<ConstantValue> for TaggedConstant {
  fn from(value: ConstantValue) -> Self {
    match value {
      ConstantValue::Bool(boolean) => TaggedValue {
        kind: ValueType::Bool,
        value: Value { boolean },
      }
      .into(),
      ConstantValue::Number(number) => TaggedValue {
        kind: ValueType::Number,
        value: Value { number },
      }
      .into(),
      ConstantValue::GlobalId(id) => TaggedValue {
        kind: ValueType::GlobalId,
        value: Value { id },
      }
      .into(),
      ConstantValue::ExternId(id) => TaggedValue {
        kind: ValueType::ExternFunctionId,
        value: Value { id },
      }
      .into(),
      ConstantValue::Str(s) => s.into(),
      ConstantValue::None => TaggedValue::none().into(),
    }
  }
}

impl Clone for TaggedConstant {
  fn clone(&self) -> Self {
    match self.tag {
      ConstantType::StackValue => Self {
        value: Constant {
          stack_val: unsafe { self.value.stack_val },
        },
        tag: ConstantType::StackValue,
      },
      ConstantType::String => Self {
        value: Constant {
          string: unsafe { self.value.string.clone() },
        },
        tag: ConstantType::String,
      },
    }
  }
}

impl Drop for TaggedConstant {
  fn drop(&mut self) {
    match self.tag {
      ConstantType::String => {
        unsafe { ManuallyDrop::drop(&mut self.value.string) };
      }
      _ => {}
    }
  }
}

#[derive(Clone)]
pub struct Function {
  pub code: Chunk,
}

#[derive(Clone)]
pub struct Chunk {
  pub code: Vec<u8>,
  pub functions: Vec<Function>,
  pub constants: Vec<TaggedConstant>,
}

impl Default for Chunk {
  fn default() -> Self {
    Self {
      code: Vec::new(),
      constants: vec![TaggedValue::none().into()],
      functions: Vec::new(),
    }
  }
}

impl Chunk {
  pub unsafe fn get_constant(&self, index: usize) -> TaggedValue {
    self.constants[index].value.stack_val
  }

  pub unsafe fn get_string_constant(&self, index: usize) -> &String {
    &self.constants[index].value.string
  }

  pub fn get_function(&self, index: usize) -> *const Function {
    std::ptr::addr_of!(self.functions[index])
  }

  pub fn empty() -> Self {
    Default::default()
  }

  pub fn new(builder: BytecodeBuilder) -> Self {
    let (code, function, constants) = builder.into_parts();
    let functions = function
      .into_iter()
      .map(|func_code| Function {
        code: Self::new(func_code),
      })
      .collect::<Vec<_>>();
    let constants = constants
      .into_iter()
      .map(|c| c.into())
      .collect::<Vec<TaggedConstant>>();
    Self {
      code,
      functions,
      constants,
    }
  }
}