use crate::api::gc::Gc;
use crate::value::{ForeignValue, NativeValue, Type, Value};
use crate::vm::value::TaggedValue;

pub trait ForeignParameters<const N: usize> {
  unsafe fn from_stack(values: &[TaggedValue]) -> Self;

  fn parameter_types() -> [Type; N];
}

pub type ForeignFunction = Box<dyn Fn(Gc, &[TaggedValue]) -> TaggedValue>;

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
    R: ForeignValue,
    F: Fn(P) -> R + 'static,
  {
    Self {
      name,
      parameter_types: Box::new(P::parameter_types()),
      return_type: R::to_type(),
      function: Box::new(move |gc, values| unsafe {
        func(P::from_stack(values)).to_value(gc).vm_value
      }),
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

impl<P: NativeValue> ForeignParameters<1> for P {
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

impl<P1: NativeValue, P2: NativeValue> ForeignParameters<2> for (P1, P2) {
  unsafe fn from_stack(values: &[TaggedValue]) -> Self {
    debug_assert_eq!(values.len(), 2);
    (
      P1::from_value(Value {
        vm_value: values[0],
      }),
      P2::from_value(Value {
        vm_value: values[1],
      }),
    )
  }

  fn parameter_types() -> [Type; 2] {
    [P1::to_type(), P2::to_type()]
  }
}
