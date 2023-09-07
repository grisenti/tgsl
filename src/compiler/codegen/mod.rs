use crate::compiler::ast::AST;
use crate::compiler::codegen::function_code::FunctionCode;

use bytecode::OpCode;
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
    for (i, func) in self.functions.iter().enumerate() {
      result += &format!("++ fn {i} ++\n{:?}-- fn {i} --\n", func);
    }
    result += &format!("{:?}", self.global_code);
    write!(f, "{result}")
  }
}
