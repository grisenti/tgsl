use std::collections::HashMap;

use self::function_analysis::{FunctionAnalizer, FunctionAnalysisResult};

use super::{
  ast::*,
  bytecode::Chunk,
  identifier::Identifier,
  type_map::ReverseTypeMap,
  types::{Type, TypeId},
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
type FunctionMap = HashMap<Identifier, Vec<Type>>;

struct GlobalEnv {
  structs: StructMap,
  global_types: Vec<TypeId>,
  type_map: ReverseTypeMap,
  errors: Vec<SourceError>,
  ast: AST,
}

pub struct SemanticAnalizer;

impl SemanticAnalizer {
  pub fn analyze(
    ast: AST,
    global_types: Vec<TypeId>,
    type_map: ReverseTypeMap,
  ) -> Result<Chunk, SourceError> {
    let program = ast.get_program().to_owned();
    let mut global_env = GlobalEnv {
      structs: HashMap::new(),
      global_types,
      type_map,
      errors: Vec::new(),
      ast,
    };
    let FunctionAnalysisResult { code, .. } = FunctionAnalizer::analyze(
      Vec::new(),
      &program,
      true,
      Vec::new(),
      &mut global_env,
      None,
    );
    if global_env.errors.is_empty() {
      Ok(code)
    } else {
      Err(SourceError::from_err_vec(global_env.errors))
    }
  }
}
