
use crate::compiler::codegen::function_code::FunctionCode;


use std::fmt::Debug;

pub mod bytecode;
pub mod function_code;

#[derive(Default, Clone)]
pub struct ModuleCode {
  pub functions: Vec<FunctionCode>,
  pub global_code: FunctionCode,
}

impl ModuleCode {}

impl Debug for ModuleCode {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut result = "".to_string();
    for func in &self.functions {
      result += &format!(
        "++ fn {} ++\n{:?}-- fn {} --\n",
        func.get_name(),
        func,
        func.get_name()
      );
    }
    result += &format!("{:?}", self.global_code);
    write!(f, "{result}")
  }
}
