use std::collections::HashMap;

use self::function_analysis::{FunctionAnalizer, FunctionAnalysisResult, FunctionInfo};

use super::{
  ast::*,
  codegen::BytecodeBuilder,
  errors::CompilerError,
  global_env::GlobalTypes,
  identifier::Identifier,
  types::{type_map::TypeMap, TypeId},
};

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
  errors: Vec<CompilerError>,
}

struct SemAParameters<'a> {
  ast: &'a AST,
  type_map: &'a TypeMap,
  global_types: GlobalTypes<'a>,
}

pub struct SemanticAnalizer;

impl SemanticAnalizer {
  pub fn analyze(
    ast: &AST,
    global_types: GlobalTypes,
    module_global_types: &mut [TypeId],
    type_map: &TypeMap,
  ) -> Result<BytecodeBuilder, Vec<CompilerError>> {
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
        global_types,
      },
      &mut global_env,
      module_global_types,
    );
    if global_env.errors.is_empty() {
      Ok(code)
    } else {
      Err(global_env.errors)
    }
  }
}
