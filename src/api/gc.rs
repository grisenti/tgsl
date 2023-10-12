use crate::value::Value;
use crate::vm::runtime::gc;
use crate::vm::value::TaggedValue;

pub struct Gc<'vm>(pub(crate) &'vm mut gc::GC);

impl Gc<'_> {
  pub fn allocate_string(&mut self, s: String) -> Value {
    Value {
      vm_value: TaggedValue::object(self.0.alloc_string(s)),
    }
  }
}
