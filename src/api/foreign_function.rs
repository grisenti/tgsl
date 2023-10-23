use crate::api::types::Type;
use crate::value::{ForeignValue, NativeValue, Value};
use crate::vm::value::TaggedValue;
use crate::vm::ForeignCallable;

trait VmForeignParameters<const N: usize> {
  unsafe fn from_stack(values: &[TaggedValue]) -> Self;

  fn parameter_types() -> [Type; N];
}

#[allow(private_bounds)]
pub trait ForeignParameters<const N: usize>: VmForeignParameters<N> {}

impl<T, const N: usize> ForeignParameters<N> for T where T: VmForeignParameters<N> {}

pub struct ForeignFunction<'name> {
  name: &'name str,
  parameter_types: Box<[Type]>,
  return_type: Type,
  function: ForeignCallable,
}

impl<'name> ForeignFunction<'name> {
  pub fn create<P, R, F, const N: usize>(name: &'name str, func: F) -> Self
  where
    P: ForeignParameters<N>,
    R: ForeignValue,
    F: Fn(P) -> R + 'static,
  {
    Self {
      name,
      parameter_types: Box::new(<P as VmForeignParameters<N>>::parameter_types()),
      return_type: R::to_type(),
      function: Box::new(move |gc, values| unsafe {
        func(P::from_stack(values)).to_value(gc).vm_value
      }),
    }
  }

  pub fn create_no_arguments<R, F>(name: &'static str, func: F) -> Self
  where
    R: ForeignValue,
    F: Fn() -> R + 'static,
  {
    Self {
      name,
      parameter_types: Box::new([]),
      return_type: R::to_type(),
      function: Box::new(move |gc, _| unsafe { func().to_value(gc).vm_value }),
    }
  }

  pub(crate) fn get_name(&self) -> &str {
    self.name
  }

  pub(crate) fn get_parameters(&self) -> &[Type] {
    &self.parameter_types
  }

  pub(crate) fn get_return_type(&self) -> &Type {
    &self.return_type
  }

  pub(crate) fn get_foreign_function(self) -> ForeignCallable {
    self.function
  }
}

impl<P: NativeValue> VmForeignParameters<1> for P {
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

impl<P1: NativeValue, P2: NativeValue> VmForeignParameters<2> for (P1, P2) {
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
