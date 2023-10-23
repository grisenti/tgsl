use crate::foreign_function::{ForeignFunction, ForeignParameters};
use crate::value::ForeignValue;

pub struct Module<'m> {
  pub(crate) foreign_functions: Vec<ForeignFunction<'m>>,
  pub(crate) source: &'m str,
}

impl<'m> Module<'m> {
  pub fn new(source: &'m str) -> Self {
    Self {
      source,
      foreign_functions: Default::default(),
    }
  }

  pub fn add_function<P, R, F, const N: usize>(&mut self, name: &'m str, func: F) -> &mut Self
  where
    P: ForeignParameters<N>,
    R: ForeignValue,
    F: Fn(P) -> R + 'static,
  {
    self
      .foreign_functions
      .push(ForeignFunction::create(name, func));
    self
  }
}
