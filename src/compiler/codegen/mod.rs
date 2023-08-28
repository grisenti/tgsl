use crate::compiler::ast::AST;
use crate::compiler::codegen::function_code::FunctionCode;

use crate::compiler::codegen::generation::BytecodeGenerator;

use bytecode::OpCode;
use std::fmt::Debug;

pub mod bytecode;
pub mod function_code;
mod generation;

#[derive(Default, Clone)]
pub struct ModuleCode {
  pub functions: Vec<FunctionCode>,
  pub global_code: FunctionCode,
}

impl ModuleCode {
  pub fn generate_program(ast: &AST) -> Self {
    let mut module_code = Self::default();
    module_code.global_code =
      BytecodeGenerator::generate_function(ast.get_program(), ast, &mut module_code.functions);
    unsafe { module_code.global_code.push_op(OpCode::Return) };
    module_code
  }
}

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
