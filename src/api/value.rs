use crate::api::gc::Gc;
use crate::api::types::ToType;
use crate::vm::value as vm_value;
use crate::vm::value::TaggedValue;

pub struct Value {
  pub(crate) vm_value: TaggedValue,
}

pub trait FromValue {
  unsafe fn from_value(value: Value) -> Self;
}

pub trait ToValue {
  unsafe fn to_value(self, gc: Gc) -> Value;
}

pub trait ForeignValue: ToValue + ToType {}

impl<T> ForeignValue for T where T: ToValue + ToType {}

pub trait NativeValue: FromValue + ToType {}

impl<T> NativeValue for T where T: FromValue + ToType {}

impl FromValue for () {
  unsafe fn from_value(_: Value) -> Self {}
}

impl ToValue for () {
  unsafe fn to_value(self, _: Gc) -> Value {
    Value {
      vm_value: TaggedValue::none(),
    }
  }
}

impl FromValue for bool {
  unsafe fn from_value(value: Value) -> Self {
    value.vm_value.as_bool()
  }
}

impl ToValue for bool {
  unsafe fn to_value(self, _: Gc) -> Value {
    Value {
      vm_value: TaggedValue {
        value: vm_value::Value { boolean: self },
        kind: vm_value::ValueType::Bool,
      },
    }
  }
}

impl FromValue for f64 {
  unsafe fn from_value(value: Value) -> Self {
    value.vm_value.as_f64()
  }
}

impl ToValue for f64 {
  unsafe fn to_value(self, _: Gc) -> Value {
    Value {
      vm_value: TaggedValue {
        value: vm_value::Value { number: self },
        kind: vm_value::ValueType::Number,
      },
    }
  }
}

impl FromValue for &mut String {
  unsafe fn from_value(value: Value) -> Self {
    value.vm_value.as_object().as_string()
  }
}

impl ToValue for String {
  unsafe fn to_value(self, mut gc: Gc) -> Value {
    gc.allocate_string(self)
  }
}

impl FromValue for &str {
  unsafe fn from_value(value: Value) -> Self {
    value.vm_value.as_object().as_string()
  }
}
