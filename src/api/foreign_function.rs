use std::marker::PhantomData;

use crate::api::types::Type;
use crate::errors::RuntimeError;
use crate::value::{ForeignValue, NativeValue, Value};
use crate::vm::value::TaggedValue;
use crate::vm::ForeignCallable;

trait VmForeignParameters {
  unsafe fn from_stack(values: &[TaggedValue]) -> Self;

  fn parameter_types() -> Vec<Type>;
}

#[allow(private_bounds)]
pub trait ForeignParameters: VmForeignParameters {}

impl<T> ForeignParameters for T where T: VmForeignParameters {}

impl<P: NativeValue> VmForeignParameters for P {
  unsafe fn from_stack(values: &[TaggedValue]) -> Self {
    debug_assert_eq!(values.len(), 1);
    P::from_value(Value {
      vm_value: values[0],
    })
  }

  fn parameter_types() -> Vec<Type> {
    vec![P::to_type()]
  }
}

impl<P1: NativeValue, P2: NativeValue> VmForeignParameters for (P1, P2) {
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

  fn parameter_types() -> Vec<Type> {
    vec![P1::to_type(), P2::to_type()]
  }
}

pub trait ForeignFunction<T> {
  fn parameters() -> Vec<Type>;

  fn return_type() -> Type;

  fn raw_foreign_function(self) -> ForeignCallable;
}

impl<F, C: 'static, Params, Ret> ForeignFunction<PhantomData<(C, Params, Ret)>> for F
where
  Params: ForeignParameters,
  Ret: ForeignValue,
  F: Fn(&mut C, Params) -> Ret + 'static,
{
  fn parameters() -> Vec<Type> {
    Params::parameter_types()
  }

  fn return_type() -> Type {
    Ret::to_type()
  }

  fn raw_foreign_function(self) -> ForeignCallable {
    ForeignCallable::new::<C>(Box::new(move |arguments, gc, context| unsafe {
      Ok(
        self(
          context.downcast_mut().unwrap(),
          Params::from_stack(arguments),
        )
        .to_value(gc)
        .vm_value,
      )
    }))
  }
}

impl<F, Params, Ret> ForeignFunction<PhantomData<(Params, Ret)>> for F
where
  Params: ForeignParameters,
  Ret: ForeignValue,
  F: Fn(Params) -> Ret + 'static,
{
  fn parameters() -> Vec<Type> {
    Params::parameter_types()
  }

  fn return_type() -> Type {
    Ret::to_type()
  }

  fn raw_foreign_function(self) -> ForeignCallable {
    ForeignCallable::new_no_context(Box::new(move |arguments, gc, _| unsafe {
      Ok(self(Params::from_stack(arguments)).to_value(gc).vm_value)
    }))
  }
}

impl<F, Params, Ret> ForeignFunction<PhantomData<(Params, Result<Ret, RuntimeError>)>> for F
where
  Params: ForeignParameters,
  Ret: ForeignValue,
  F: Fn(Params) -> Result<Ret, RuntimeError> + 'static,
{
  fn parameters() -> Vec<Type> {
    Params::parameter_types()
  }

  fn return_type() -> Type {
    Ret::to_type()
  }

  fn raw_foreign_function(self) -> ForeignCallable {
    ForeignCallable::new_no_context(Box::new(move |arguments, gc, _| unsafe {
      self(Params::from_stack(arguments)).map(|v| v.to_value(gc).vm_value)
    }))
  }
}
