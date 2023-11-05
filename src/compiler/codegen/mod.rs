use std::fmt::Debug;

use crate::compiler::codegen::function_code::FunctionCode;

pub mod bytecode;
pub mod function_code;

#[derive(Default, Clone)]
pub struct ModuleCode {
  pub functions: Vec<FunctionCode>,
  pub global_code: FunctionCode,
  pub global_variables_count: u32,
  pub foreign_functions_count: u32,
}

impl Debug for ModuleCode {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut result = "".to_string();
    for func in &self.functions {
      result += &format!("{}:\n{:?}", func.get_name(), func);
    }
    result += &format!("<global>:\n{:?}", self.global_code);
    write!(f, "{result}")
  }
}
