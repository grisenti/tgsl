use std::mem::ManuallyDrop;

use crate::compiler::codegen::bytecode::ConstantValue;
use crate::compiler::codegen::function_code::FunctionCode;
use crate::compiler::codegen::ModuleCode;

use super::{
  address_table::AddressTable,
  value::{TaggedValue, Value, ValueType},
};

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
  pub constants: Vec<TaggedConstant>,
}

impl Default for Chunk {
  fn default() -> Self {
    Self {
      code: Vec::new(),
      constants: vec![TaggedValue::none().into()],
    }
  }
}

fn convert_constant(value: ConstantValue, address_table: &AddressTable) -> TaggedConstant {
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
    ConstantValue::RelativeNativeGlobalVar(address) => TaggedValue {
      kind: ValueType::GlobalId,
      value: Value {
        id: address_table.resolve_variable(address) as usize,
      },
    }
    .into(),
    ConstantValue::AbsoluteNativeGlobalVar(address) => TaggedValue {
      kind: ValueType::GlobalId,
      value: Value {
        id: address as usize,
      },
    }
    .into(),
    ConstantValue::RelativeNativeFn(address) => TaggedValue {
      kind: ValueType::FunctionId,
      value: Value {
        id: address_table.resolve_global_function(address) as usize,
      },
    }
    .into(),
    ConstantValue::RelativeForeignFn(address) => TaggedValue {
      kind: ValueType::ForeignFunctionId,
      value: Value {
        id: address_table.resolve_foreign_function(address) as usize,
      },
    }
    .into(),
    ConstantValue::AbsoluteNativeFn(address) => TaggedValue {
      kind: ValueType::FunctionId,
      value: Value {
        id: address as usize,
      },
    }
    .into(),
    ConstantValue::AbsoluteForeignFn(address) => TaggedValue {
      kind: ValueType::ForeignFunctionId,
      value: Value {
        id: address as usize,
      },
    }
    .into(),
    ConstantValue::Str(s) => s.into(),
    ConstantValue::None => TaggedValue::none().into(),
    ConstantValue::Stub => panic!("stub constant passed to vm"),
  }
}

impl Chunk {
  pub unsafe fn get_constant(&self, index: usize) -> TaggedValue {
    self.constants[index].value.stack_val
  }

  pub unsafe fn get_string_constant(&self, index: usize) -> &String {
    &self.constants[index].value.string
  }

  pub fn empty() -> Self {
    Default::default()
  }

  pub fn new(function_code: FunctionCode, address_table: &AddressTable) -> Self {
    let (code, constants) = function_code.into_parts();
    let constants = constants
      .into_iter()
      .map(|c| convert_constant(c, address_table))
      .collect::<Vec<TaggedConstant>>();
    Self { code, constants }
  }
}

pub struct GlobalChunk {
  pub global_code: Function,
  pub functions: Vec<Function>,
}

impl GlobalChunk {
  pub fn new(module_code: ModuleCode, address_table: &AddressTable) -> Self {
    let functions = module_code
      .functions
      .into_iter()
      .map(|func_code| Function {
        code: Chunk::new(func_code, address_table),
      })
      .collect::<Vec<_>>();
    let global_code = Function {
      code: Chunk::new(module_code.global_code, address_table),
    };
    Self {
      functions,
      global_code,
    }
  }
}
