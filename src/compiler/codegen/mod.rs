use std::fmt::Debug;

use crate::compiler::codegen::program_chunk::ProgramChunk;

pub mod bytecode;
pub mod program_chunk;

#[derive(Default, Clone)]
pub struct ModuleProgram {
  pub functions: Vec<ProgramChunk>,
  pub global_code: ProgramChunk,
  pub global_variables_count: u32,
  pub foreign_functions_count: u32,
}

impl Debug for ModuleProgram {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut result = "".to_string();
    for func in &self.functions {
      result += &format!("{}:\n{:?}", func.get_name(), func);
    }
    result += &format!("<global>:\n{:?}", self.global_code);
    write!(f, "{result}")
  }
}
