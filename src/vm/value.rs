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

impl Object {
  pub unsafe fn as_string(&mut self) -> &mut ManuallyDrop<String> {
    debug_assert_eq!(self.kind, ObjectType::String);
    &mut self.value.string
  }

  pub unsafe fn as_closure(&mut self) -> &mut ManuallyDrop<Closure> {
    debug_assert_eq!(self.kind, ObjectType::Closure);
    &mut self.value.closure
  }

  pub unsafe fn as_aggregate(&mut self) -> &mut ManuallyDrop<Aggregate> {
    debug_assert_eq!(self.kind, ObjectType::Aggregate);
    &mut self.value.aggregate
  }
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
  ForeignFunctionId,
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

  pub unsafe fn as_object<'gc>(self) -> &'gc mut Object {
    debug_assert_eq!(self.kind, ValueType::Object);
    &mut (*self.value.object)
  }

  pub unsafe fn as_id(self) -> usize {
    debug_assert!(matches!(
      self.kind,
      ValueType::ForeignFunctionId | ValueType::FunctionId | ValueType::GlobalId,
    ));
    self.value.id
  }

  pub unsafe fn as_bool(self) -> bool {
    debug_assert_eq!(self.kind, ValueType::Bool);
    self.value.boolean
  }

  pub unsafe fn as_f64(self) -> f64 {
    debug_assert_eq!(self.kind, ValueType::Number);
    self.value.number
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
      ValueType::ForeignFunctionId => unsafe { format!("<foreign {}>", self.value.id) },
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
