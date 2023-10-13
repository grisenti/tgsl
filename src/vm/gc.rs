use std::{
  alloc::Layout,
  mem::{size_of, ManuallyDrop},
};

use crate::vm::{
  chunk::Function,
  value::{Aggregate, Closure, Object, ObjectType, ObjectValue, TaggedValue, ValueType},
};

pub struct GC {
  allocations: Vec<*mut Object>,
  next_collection: usize,
  bytes_allocated: usize,
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

unsafe fn dealloc_object(obj: *mut Object) -> usize {
  match (*obj).kind {
    ObjectType::String => {
      let size = (*obj).value.string.len();
      ManuallyDrop::drop(&mut (*obj).value.string);
      dealloc_value(obj);
      size
    }
    ObjectType::Closure => {
      let size = (*obj).value.closure.captures.len();
      ManuallyDrop::drop(&mut (*obj).value.closure);
      dealloc_value(obj);
      size
    }
    ObjectType::Aggregate => {
      let size = (*obj).value.aggregate.members.len();
      ManuallyDrop::drop(&mut (*obj).value.aggregate);
      dealloc_value(obj);
      size
    }
  }
}

impl GC {
  const GROWTH_FACTOR: usize = 2;
  const MIN_HEAP_SIZE: usize = 1024 * 1024;

  pub fn should_run(&self) -> bool {
    self.bytes_allocated > self.next_collection
  }

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
    println!("running gc at {}", self.bytes_allocated);
    self.allocations.retain(|&obj| unsafe {
      if (*obj).marked {
        (*obj).marked = false;
        true
      } else {
        self.bytes_allocated -= dealloc_object(obj);
        false
      }
    });
    self.next_collection = usize::max(self.bytes_allocated * GC::GROWTH_FACTOR, GC::MIN_HEAP_SIZE);
  }

  pub fn alloc_string(&mut self, val: String) -> *mut Object {
    self.bytes_allocated += val.len();
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
    self.bytes_allocated += captures * size_of::<TaggedValue>();
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
    self.bytes_allocated += members.len();
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
    Default::default()
  }
}

impl Default for GC {
  fn default() -> Self {
    Self {
      allocations: Vec::new(),
      next_collection: 1024 * 1024,
      bytes_allocated: 0,
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
