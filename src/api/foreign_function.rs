use std::any::TypeId;
use std::marker::PhantomData;

use crate::api::types::Type;
use crate::errors::UserError;
use crate::value::{ForeignValue, NativeValue, ToValue, Value};
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

pub trait FunctionSignature {
  fn context_type_id() -> TypeId;

  fn parameter_types() -> Vec<Type>;

  fn return_type() -> Type;
}

macro_rules! signature_variant {
  ($name:ident) => {
    pub struct $name<T: FunctionSignature>(PhantomData<T>);

    impl<T: FunctionSignature> FunctionSignature for $name<T> {
      fn context_type_id() -> TypeId {
        TypeId::of::<()>()
      }

      fn parameter_types() -> Vec<Type> {
        T::parameter_types()
      }

      fn return_type() -> Type {
        T::return_type()
      }
    }
  };
}

signature_variant!(Fallible);
signature_variant!(UnFallible);
signature_variant!(NoContext);

pub struct WithContext<C: 'static, S: FunctionSignature>(PhantomData<(C, S)>);

impl<C: 'static, S: FunctionSignature> FunctionSignature for WithContext<C, S> {
  fn context_type_id() -> TypeId {
    TypeId::of::<C>()
  }

  fn parameter_types() -> Vec<Type> {
    S::parameter_types()
  }

  fn return_type() -> Type {
    S::return_type()
  }
}

pub struct Signature<Args: ForeignParameters, Ret: ToValue>(PhantomData<(Args, Ret)>);

impl<Args: ForeignParameters, Ret: ForeignValue> FunctionSignature for Signature<Args, Ret> {
  fn context_type_id() -> TypeId {
    TypeId::of::<()>()
  }

  fn parameter_types() -> Vec<Type> {
    Args::parameter_types()
  }

  fn return_type() -> Type {
    Ret::to_type()
  }
}

pub struct SignatureNoParams<Ret: ToValue>(PhantomData<Ret>);

impl<Ret: ForeignValue> FunctionSignature for SignatureNoParams<Ret> {
  fn context_type_id() -> TypeId {
    TypeId::of::<()>()
  }

  fn parameter_types() -> Vec<Type> {
    vec![]
  }

  fn return_type() -> Type {
    Ret::to_type()
  }
}

pub trait ForeignFunction<T: FunctionSignature> {
  fn raw_foreign_function(self) -> ForeignCallable;
}

pub type CallResult<T> = Result<T, Box<dyn UserError>>;

impl<F, Ret> ForeignFunction<NoContext<UnFallible<SignatureNoParams<Ret>>>> for F
where
  Ret: ForeignValue,
  F: Fn() -> Ret + 'static,
{
  fn raw_foreign_function(self) -> ForeignCallable {
    Box::new(move |arguments, gc, _| unsafe { Ok(self().to_value(gc).vm_value) })
  }
}

impl<F, Ret> ForeignFunction<NoContext<Fallible<SignatureNoParams<Ret>>>> for F
where
  Ret: ForeignValue,
  F: Fn() -> CallResult<Ret> + 'static,
{
  fn raw_foreign_function(self) -> ForeignCallable {
    Box::new(move |arguments, gc, _| unsafe { self().map(|v| v.to_value(gc).vm_value) })
  }
}

impl<F, Context, Ret> ForeignFunction<WithContext<Context, UnFallible<SignatureNoParams<Ret>>>>
  for F
where
  Context: 'static,
  Ret: ForeignValue,
  F: Fn(&mut Context) -> Ret + 'static,
{
  fn raw_foreign_function(self) -> ForeignCallable {
    Box::new(move |_, gc, context| unsafe {
      let context = context.downcast_mut().unwrap();
      Ok(self(context).to_value(gc).vm_value)
    })
  }
}

impl<F, Context, Ret> ForeignFunction<WithContext<Context, Fallible<SignatureNoParams<Ret>>>> for F
where
  Context: 'static,
  Ret: ForeignValue,
  F: Fn(&mut Context) -> Result<Ret, Box<dyn UserError>> + 'static,
{
  fn raw_foreign_function(self) -> ForeignCallable {
    Box::new(move |arguments, gc, context| unsafe {
      let context = context.downcast_mut().unwrap();
      self(context).map(|v| v.to_value(gc).vm_value)
    })
  }
}

impl<F, Params, Ret> ForeignFunction<NoContext<UnFallible<Signature<Params, Ret>>>> for F
where
  Params: ForeignParameters,
  Ret: ForeignValue,
  F: Fn(Params) -> Ret + 'static,
{
  fn raw_foreign_function(self) -> ForeignCallable {
    Box::new(move |arguments, gc, _| unsafe {
      Ok(self(Params::from_stack(arguments)).to_value(gc).vm_value)
    })
  }
}

impl<F, Params, Ret> ForeignFunction<NoContext<Fallible<Signature<Params, Ret>>>> for F
where
  Params: ForeignParameters,
  Ret: ForeignValue,
  F: Fn(Params) -> Result<Ret, Box<dyn UserError>> + 'static,
{
  fn raw_foreign_function(self) -> ForeignCallable {
    Box::new(move |arguments, gc, _| unsafe {
      self(Params::from_stack(arguments)).map(|v| v.to_value(gc).vm_value)
    })
  }
}

impl<F, Context, Params, Ret>
  ForeignFunction<WithContext<Context, UnFallible<Signature<Params, Ret>>>> for F
where
  Context: 'static,
  Params: ForeignParameters,
  Ret: ForeignValue,
  F: Fn(&mut Context, Params) -> Ret + 'static,
{
  fn raw_foreign_function(self) -> ForeignCallable {
    Box::new(move |arguments, gc, context| unsafe {
      let context = context.downcast_mut().unwrap();
      Ok(
        self(context, Params::from_stack(arguments))
          .to_value(gc)
          .vm_value,
      )
    })
  }
}

impl<F, Context, Params, Ret>
  ForeignFunction<WithContext<Context, Fallible<Signature<Params, Ret>>>> for F
where
  Context: 'static,
  Params: ForeignParameters,
  Ret: ForeignValue,
  F: Fn(&mut Context, Params) -> Result<Ret, Box<dyn UserError>> + 'static,
{
  fn raw_foreign_function(self) -> ForeignCallable {
    Box::new(move |arguments, gc, context| unsafe {
      let context = context.downcast_mut().unwrap();
      self(context, Params::from_stack(arguments)).map(|v| v.to_value(gc).vm_value)
    })
  }
}
