use std::alloc::Layout;

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
  std::alloc::dealloc(value as *mut u8, Layout::new::<String>())
}

unsafe fn dealloc_object(obj: *mut Object) {
  match (*obj).kind {
    ObjectType::String => {
      dealloc_value((*obj).value.string);
    }
    ObjectType::Closure => {
      dealloc_value((*obj).value.closure);
    }
    ObjectType::Aggregate => {
      println!("dealloc aggregate");
      dealloc_value((*obj).value.aggregate);
    }
  }
  dealloc_value(obj)
}

impl GC {
  fn mark_object(&mut self, obj: *mut Object) {
    unsafe {
      if (*obj).marked {
        return;
      }
      match (*obj).kind {
        ObjectType::String => (*obj).marked = true,
        ObjectType::Aggregate => {
          (*obj).marked = true;
          let aggregate = unsafe { (*obj).value.aggregate };
          self.mark(unsafe { (*aggregate).members.iter() });
        }
        ObjectType::Closure => {
          (*obj).marked = true;
          let closure = unsafe { (*obj).value.closure };
          self.mark(unsafe { (*closure).captures.iter() });
        }
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
          string: alloc_value(val),
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
          closure: unsafe {
            alloc_value(Closure {
              function,
              captures: Vec::with_capacity(captures),
            })
          },
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
          aggregate: unsafe { alloc_value(Aggregate { members }) },
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
