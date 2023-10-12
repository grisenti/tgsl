use crate::value::{FromValue, ToType, ToValue, Type, Value};
use crate::vm::value::TaggedValue;

pub trait Parameter: FromValue + ToType {}

impl<T> Parameter for T where T: FromValue + ToType {}

pub trait Return: ToValue + ToType {}

impl<T> Return for T where T: ToValue + ToType {}

pub trait ForeignParameters<const N: usize> {
  unsafe fn from_stack(values: &[TaggedValue]) -> Self;

  fn parameter_types() -> [Type; N];
}

pub type ForeignFunction = Box<dyn Fn(&[TaggedValue]) -> TaggedValue>;

pub struct ForeignFunctionInfo {
  name: &'static str,
  parameter_types: Box<[Type]>,
  return_type: Type,
  function: ForeignFunction,
}

impl ForeignFunctionInfo {
  pub fn create<P, R, F, const N: usize>(name: &'static str, func: F) -> Self
  where
    P: ForeignParameters<N>,
    R: Return,
    F: Fn(P) -> R + 'static,
  {
    Self {
      name,
      parameter_types: Box::new(P::parameter_types()),
      return_type: R::to_type(),
      function: Box::new(move |values| unsafe { func(P::from_stack(values)).to_value().vm_value }),
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

  pub fn get_foreign_function(self) -> ForeignFunction {
    self.function
  }
}

impl<P: Parameter> ForeignParameters<1> for P {
  unsafe fn from_stack(values: &[TaggedValue]) -> Self {
    debug_assert_eq!(values.len(), 1);
    P::from_value(Value {
      vm_value: values[0],
    })
  }

  fn parameter_types() -> [Type; 1] {
    [P::to_type()]
  }
}

impl<P1: Parameter, P2: Parameter> ForeignParameters<2> for (P1, P2) {
  unsafe fn from_stack(values: &[TaggedValue]) -> Self {
    debug_assert_eq!(values.len(), 2);
    (
      P1::from_value(Value {
        vm_value: values[1],
      }),
      P2::from_value(Value {
        vm_value: values[0],
      }),
    )
  }

  fn parameter_types() -> [Type; 2] {
    [P1::to_type(), P2::to_type()]
  }
}
