use std::collections::HashMap;

use self::function_analysis::{FunctionAnalizer, FunctionAnalysisResult, FunctionInfo};

use super::{
  ast::*,
  codegen::BytecodeBuilder,
  identifier::Identifier,
  types::{type_map::TypeMap, TypeId},
};
use crate::errors::SourceError;

mod function_analysis;
mod return_analysis;

#[derive(Clone)]
struct Struct {
  member_names: Vec<StrHandle>,
  member_types: Vec<TypeId>,
}

type StructMap = HashMap<Identifier, Struct>;

struct SemAState {
  structs: StructMap,
  errors: Vec<SourceError>,
}

struct SemAParameters<'a> {
  ast: &'a AST,
  type_map: &'a TypeMap,
}

pub struct SemanticAnalizer;

impl SemanticAnalizer {
  pub fn analyze(
    ast: AST,
    global_types: &mut Vec<TypeId>,
    type_map: &TypeMap,
  ) -> Result<BytecodeBuilder, SourceError> {
    let program = ast.get_program();
    let mut global_env = SemAState {
      structs: HashMap::new(),
      errors: Vec::new(),
    };
    let FunctionAnalysisResult { code, .. } = FunctionAnalizer::analyze(
      FunctionInfo::default(),
      program,
      SemAParameters {
        ast: &ast,
        type_map,
      },
      &mut global_env,
      global_types,
    );
    if global_env.errors.is_empty() {
      Ok(code)
    } else {
      Err(SourceError::from_err_vec(global_env.errors))
    }
  }
}
