use crate::compiler::types::Type;
use crate::vm::value::{TaggedValue, Value, ValueType};

pub trait FromValue {
  unsafe fn from_value(value: TaggedValue) -> Self;
}

pub trait ToType {
  fn to_type() -> Type;
}

pub trait Parameter: FromValue + ToType {}

impl<T> Parameter for T where T: FromValue + ToType {}

pub trait ToValue {
  unsafe fn to_value(self) -> TaggedValue;
}

pub trait Return: ToValue + ToType {}

impl<T> Return for T where T: ToValue + ToType {}

pub trait FunctionParameters {
  unsafe fn from_stack(values: &[TaggedValue]) -> Self;

  fn parameter_types() -> Vec<Type>;
}

pub type ExternFunction = Box<dyn Fn(&[TaggedValue]) -> TaggedValue>;

pub struct ExternFunctionInfo {
  name: &'static str,
  parameter_types: Vec<Type>,
  return_type: Type,
  function: ExternFunction,
}

impl ExternFunctionInfo {
  pub fn create<P, R, F>(name: &'static str, func: F) -> Self
  where
    P: FunctionParameters,
    R: Return,
    F: Fn(P) -> R + 'static,
  {
    Self {
      name,
      parameter_types: P::parameter_types(),
      return_type: R::to_type(),
      function: Box::new(move |values| unsafe { func(P::from_stack(values)).to_value() }),
    }
  }

  pub fn get_name(&self) -> &str {
    self.name
  }

  pub fn get_parameters(&self) -> &[Type] {
    &self.parameter_types
  }

  pub fn get_return_type(&self) -> &Type {
    &self.return_type
  }

  pub fn get_extern_function(self) -> ExternFunction {
    self.function
  }
}

impl FromValue for () {
  unsafe fn from_value(_: TaggedValue) -> Self {}
}

impl ToType for () {
  fn to_type() -> Type {
    Type::Nothing
  }
}

impl ToValue for () {
  unsafe fn to_value(self) -> TaggedValue {
    TaggedValue::none()
  }
}

impl FromValue for bool {
  unsafe fn from_value(value: TaggedValue) -> Self {
    value.value.boolean
  }
}

impl ToType for bool {
  fn to_type() -> Type {
    Type::Bool
  }
}

impl ToValue for bool {
  unsafe fn to_value(self) -> TaggedValue {
    TaggedValue {
      value: Value { boolean: self },
      kind: ValueType::Bool,
    }
  }
}

impl FromValue for f64 {
  unsafe fn from_value(value: TaggedValue) -> Self {
    value.value.number
  }
}

impl ToValue for f64 {
  unsafe fn to_value(self) -> TaggedValue {
    TaggedValue {
      value: Value { number: self },
      kind: ValueType::Number,
    }
  }
}

impl ToType for f64 {
  fn to_type() -> Type {
    Type::Num
  }
}

impl FunctionParameters for TaggedValue {
  unsafe fn from_stack(values: &[TaggedValue]) -> Self {
    debug_assert_eq!(values.len(), 1);
    values[0]
  }

  fn parameter_types() -> Vec<Type> {
    vec![Type::Any]
  }
}

impl<P: Parameter> FunctionParameters for P {
  unsafe fn from_stack(values: &[TaggedValue]) -> Self {
    debug_assert_eq!(values.len(), 1);
    P::from_value(values[0])
  }

  fn parameter_types() -> Vec<Type> {
    vec![P::to_type()]
  }
}

impl<P1: Parameter, P2: Parameter> FunctionParameters for (P1, P2) {
  unsafe fn from_stack(values: &[TaggedValue]) -> Self {
    debug_assert_eq!(values.len(), 2);
    (P1::from_value(values[1]), P2::from_value(values[0]))
  }

  fn parameter_types() -> Vec<Type> {
    vec![P1::to_type(), P2::to_type()]
  }
}
