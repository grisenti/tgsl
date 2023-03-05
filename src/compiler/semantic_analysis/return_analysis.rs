use crate::compiler::types::Type;

pub enum ReturnType {
  Conditional(Type),
  Unconditional(Type),
}

pub fn to_conditional(opt_ret: Option<ReturnType>) -> Option<ReturnType> {
  if let Some(ReturnType::Unconditional(ret_type)) = opt_ret {
    Some(ReturnType::Conditional(ret_type))
  } else {
    opt_ret
  }
}

impl ReturnType {
  pub fn merge(self, other: ReturnType) -> Option<Self> {
    match (self, other) {
      (ReturnType::Unconditional(t1), ReturnType::Conditional(t2)) if t1 == t2 => {
        Some(ReturnType::Unconditional(t1))
      }
      (ReturnType::Conditional(t1), ReturnType::Unconditional(t2)) if t1 == t2 => {
        Some(ReturnType::Unconditional(t1))
      }
      (ReturnType::Unconditional(t1), ReturnType::Unconditional(t2)) if t1 == t2 => {
        Some(ReturnType::Unconditional(t1))
      }
      (ReturnType::Conditional(t1), ReturnType::Conditional(t2)) if t1 == t2 => {
        Some(ReturnType::Conditional(t1))
      }
      _ => None,
    }
  }
}
