use std::fmt::{Display, Formatter};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BinaryOperator {
  // numbers
  AddNum,
  SubNum,
  MulNum,
  DivNum,
  LeNum,
  GeNum,
  LeqNum,
  GeqNum,
  SameNum,
  DiffNum,

  // strings
  AddStr,
  LeStr,
  GeStr,
  LeqStr,
  GeqStr,
  SameStr,
  DiffStr,

  // bool
  SameBool,
  DiffBool,

  Invalid,
}

impl Display for BinaryOperator {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::AddNum | Self::AddStr => write!(f, "+"),
      Self::SubNum => write!(f, "-"),
      Self::MulNum => write!(f, "*"),
      Self::DivNum => write!(f, "/"),
      Self::LeNum | Self::LeStr => write!(f, "<"),
      Self::GeNum | Self::GeStr => write!(f, ">"),
      Self::LeqNum | Self::LeqStr => write!(f, "<="),
      Self::GeqNum | Self::GeqStr => write!(f, ">="),
      Self::SameNum | Self::SameStr | Self::SameBool => write!(f, "=="),
      Self::DiffNum | Self::DiffStr | Self::DiffBool => write!(f, "!="),
      Self::Invalid => write!(f, "<INVALID>"),
    }
  }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum UnaryOperator {
  // numbers
  NegNum,

  // bool
  NotBool,

  Invalid,
}

impl Display for UnaryOperator {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::NegNum => write!(f, "-"),
      Self::NotBool => write!(f, "!"),
      Self::Invalid => write!(f, "<INVALID>"),
    }
  }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum LogicalOperator {
  And,
  Or,
  Invalid,
}

impl Display for LogicalOperator {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::And => write!(f, "and"),
      Self::Or => write!(f, "or"),
      Self::Invalid => write!(f, "<INVALID>"),
    }
  }
}
