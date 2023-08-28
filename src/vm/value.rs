use std::{fmt::Debug, mem::ManuallyDrop};

use super::chunk::Function;

#[derive(Clone)]
pub struct Closure {
  pub function: *const Function,
  pub captures: Vec<TaggedValue>,
}

pub struct Aggregate {
  pub members: Vec<TaggedValue>,
}

pub union ObjectValue {
  pub string: ManuallyDrop<String>,
  pub closure: ManuallyDrop<Closure>,
  pub aggregate: ManuallyDrop<Aggregate>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ObjectType {
  String,
  Closure,
  Aggregate,
}

pub struct Object {
  pub kind: ObjectType,
  pub value: ObjectValue,
  pub marked: bool,
}

impl ToString for Object {
  fn to_string(&self) -> String {
    unsafe {
      match self {
        Object {
          kind: ObjectType::String,
          value: ObjectValue { string },
          ..
        } => string.to_string(),
        Object {
          kind: ObjectType::Closure,
          value: _,
          marked: _,
        } => "<closure>".to_string(),
        Object {
          kind: ObjectType::Aggregate,
          value: _,
          marked: _,
        } => "<struct>".to_string(),
      }
    }
  }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ValueType {
  Number,
  Bool,
  GlobalId,
  Function,
  FunctionId,
  ExternFunctionId,
  Object,

  None,
}

#[derive(Clone, Copy)]
pub union Value {
  pub number: f64,
  pub id: usize,
  pub boolean: bool,
  pub object: *mut Object,
  pub none: (),
}

#[derive(Clone, Copy)]
pub struct TaggedValue {
  pub kind: ValueType,
  pub value: Value,
}

impl TaggedValue {
  pub fn object(object: *mut Object) -> Self {
    Self {
      kind: ValueType::Object,
      value: Value { object },
    }
  }

  pub fn none() -> Self {
    Self {
      kind: ValueType::None,
      value: Value { none: () },
    }
  }
}

impl ToString for TaggedValue {
  fn to_string(&self) -> String {
    match self.kind {
      ValueType::Number => unsafe { self.value.number.to_string() },
      ValueType::Bool => unsafe { self.value.boolean.to_string() },
      ValueType::GlobalId => unsafe { format!("<global {}>", self.value.id) },
      ValueType::Function => "<function>".to_string(),
      ValueType::None => "<none>".to_string(),
      ValueType::ExternFunctionId => unsafe { format!("<extern {}>", self.value.id) },
      ValueType::Object => unsafe { (*self.value.object).to_string() },
      ValueType::FunctionId => unsafe { format!("<function {}>", self.value.id) },
    }
  }
}

impl Debug for TaggedValue {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.to_string())
  }
}
