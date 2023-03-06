use std::{alloc::Layout, mem::ManuallyDrop};

use crate::compiler::bytecode::Function;

use super::value::*;

pub struct GC {
  allocations: Vec<*mut Object>,
}

unsafe fn alloc_value<T>(value: T) -> *mut T {
  let ptr = std::alloc::alloc(Layout::new::<T>()) as *mut T;
  std::ptr::write(ptr, value);
  ptr
}

unsafe fn dealloc_value<T>(value: *mut T) {
  value.drop_in_place();
  std::alloc::dealloc(value as *mut u8, Layout::new::<T>())
}

unsafe fn dealloc_object(obj: *mut Object) {
  match (*obj).kind {
    ObjectType::String => {
      ManuallyDrop::drop(&mut (*obj).value.string);
      dealloc_value(obj);
    }
    ObjectType::Closure => {
      ManuallyDrop::drop(&mut (*obj).value.closure);
      dealloc_value(obj);
    }
    ObjectType::Aggregate => {
      ManuallyDrop::drop(&mut (*obj).value.aggregate);
      dealloc_value(obj);
    }
  }
}

impl GC {
  fn mark_object(&mut self, obj: *mut Object) {
    let obj = unsafe { &mut (*obj) };
    if obj.marked {
      return;
    }
    obj.marked = true;
    match obj.kind {
      ObjectType::String => {}
      ObjectType::Aggregate => {
        let aggregate = unsafe { &mut obj.value.aggregate };
        self.mark(aggregate.members.iter());
      }
      ObjectType::Closure => {
        let closure = unsafe { &mut obj.value.closure };
        self.mark(closure.captures.iter());
      }
    }
  }

  pub fn mark<'a, I>(&mut self, values: I)
  where
    I: Iterator<Item = &'a TaggedValue>,
  {
    for val in values.filter(|v| v.kind == ValueType::Object) {
      let obj = unsafe { val.value.object };
      self.mark_object(obj);
    }
  }

  pub unsafe fn sweep(&mut self) {
    self.allocations.retain(|&obj| unsafe {
      if (*obj).marked {
        (*obj).marked = false;
        true
      } else {
        dealloc_object(obj);
        false
      }
    })
  }

  pub fn alloc_string(&mut self, val: String) -> *mut Object {
    let obj = unsafe {
      alloc_value(Object {
        kind: ObjectType::String,
        value: ObjectValue {
          string: ManuallyDrop::new(val),
        },
        marked: false,
      })
    };
    self.allocations.push(obj);
    obj
  }

  pub fn alloc_closure(&mut self, function: *const Function, captures: usize) -> *mut Object {
    let obj = unsafe {
      alloc_value(Object {
        kind: ObjectType::Closure,
        value: ObjectValue {
          closure: ManuallyDrop::new(Closure {
            function,
            captures: Vec::with_capacity(captures),
          }),
        },
        marked: false,
      })
    };
    self.allocations.push(obj);
    obj
  }

  pub fn alloc_aggregate(&mut self, members: Vec<TaggedValue>) -> *mut Object {
    println!("alloc aggregate");
    let obj = unsafe {
      alloc_value(Object {
        kind: ObjectType::Aggregate,
        value: ObjectValue {
          aggregate: ManuallyDrop::new(Aggregate { members }),
        },
        marked: false,
      })
    };
    self.allocations.push(obj);
    obj
  }

  pub fn new() -> Self {
    Self {
      allocations: Vec::new(),
    }
  }
}

impl Drop for GC {
  fn drop(&mut self) {
    for &obj in &self.allocations {
      unsafe {
        dealloc_object(obj);
      }
    }
  }
}
