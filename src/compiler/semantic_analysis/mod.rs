use std::collections::HashMap;

use self::function_analysis::{FunctionAnalizer, FunctionAnalysisResult};

use super::{
  ast::*,
  bytecode::Chunk,
  identifier::Identifier,
  types::{type_map::TypeMap, Type, TypeId},
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

struct GlobalEnv {
  structs: StructMap,
  errors: Vec<SourceError>,
}

pub struct SemanticAnalizer;

impl SemanticAnalizer {
  pub fn analyze(
    ast: AST,
    global_types: &mut Vec<TypeId>,
    type_map: &TypeMap,
  ) -> Result<Chunk, SourceError> {
    let program = ast.get_program();
    let mut global_env = GlobalEnv {
      structs: HashMap::new(),
      errors: Vec::new(),
    };
    let FunctionAnalysisResult { code, .. } = FunctionAnalizer::analyze(
      Vec::new(),
      program,
      true,
      Vec::new(),
      &mut global_env,
      &ast,
      type_map,
      global_types,
      None,
    );
    if global_env.errors.is_empty() {
      Ok(code)
    } else {
      Err(SourceError::from_err_vec(global_env.errors))
    }
  }
}
