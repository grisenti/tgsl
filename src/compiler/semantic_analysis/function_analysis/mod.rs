use crate::{
  compiler::{
    ast::{SourceInfoHandle, Stmt, StmtHandle, AST},
    bytecode::{ConstantValue, OpCode},
    codegen::BytecodeBuilder,
    error_from_source_info,
    identifier::Identifier,
    types::{type_map::TypeMap, Type, TypeId},
  },
  errors::{SourceError, SourceInfo},
};

use super::{return_analysis::ReturnType, SemAParameters, SemAState};

mod expression_analysis;
mod statement_analysis;

#[derive(Default)]
pub struct FunctionInfo {
  parameters: Vec<TypeId>,
  captures: Vec<TypeId>,
  declaration_src_info: Option<SourceInfoHandle>,
}

pub struct FunctionAnalizer<'analysis> {
  captures: Vec<TypeId>,
  locals: Vec<TypeId>,
  scope_depth: u8,
  global_scope: bool,
  loop_depth: u32,

  code: BytecodeBuilder,

  global_env: &'analysis mut SemAState,
  ast: &'analysis AST,
  type_map: &'analysis TypeMap,
  global_types: &'analysis mut Vec<TypeId>,
  declaration_info: Option<SourceInfoHandle>,
}

pub struct FunctionAnalysisResult {
  pub code: BytecodeBuilder,
  pub return_type: TypeId,
}

fn set_type_or_push_error(
  lhs: &mut TypeId,
  rhs: TypeId,
  name_info: SourceInfo,
  errors: &mut Vec<SourceError>,
  type_map: &TypeMap,
) {
  match *lhs {
    TypeId::UNKNOWN => {
      *lhs = rhs;
    }
    TypeId::ERROR => {} // error already reported
    lhs if lhs != rhs => errors.push(error_from_source_info(
      &name_info,
      format!(
        "cannot assign value of type {} to identifier of type {}",
        type_map.type_to_string(rhs),
        type_map.type_to_string(lhs)
      ),
    )),
    _ => {} // equal types, no need to set
  };
}

impl<'analysis> FunctionAnalizer<'analysis> {
  fn pop_scope(&mut self, locals: u8) {
    assert!(self.scope_depth >= 1);
    assert!(locals as usize <= self.locals.len());
    for _ in 0..locals {
      unsafe { self.code.push_op(OpCode::Pop) }
    }
    self.locals.truncate(self.locals.len() - locals as usize);
    self.scope_depth -= 1;
  }

  fn push_scope(&mut self) {
    self.scope_depth += 1;
  }

  fn get_type_mut_ref(&mut self, id: Identifier) -> &mut TypeId {
    match id {
      Identifier::Global(gid) => &mut self.global_types[gid as usize],
      Identifier::Local(id) => &mut self.locals[id as usize],
      Identifier::Capture(id) => &mut self.captures[id as usize],
    }
  }

  fn get_type(&self, id: TypeId) -> &Type {
    self.type_map.get_type(id)
  }

  fn get_typeid(&mut self, id: Identifier) -> TypeId {
    *self.get_type_mut_ref(id)
  }

  fn set_type(&mut self, id: Identifier, ty: TypeId) {
    *self.get_type_mut_ref(id) = ty;
  }

  fn type_string(&self, id: TypeId) -> String {
    self.type_map.type_to_string(id)
  }

  fn declare(&mut self, id: Identifier, var_type: TypeId) {
    match id {
      Identifier::Global(gid) => {
        unsafe {
          self.code.push_constant(ConstantValue::GlobalId(gid));
          self.code.push_op(OpCode::SetGlobal);
          self.code.push_op(OpCode::Pop);
        }
        self.set_type(id, var_type); // multiple declarations already checked during parsing
      }
      Identifier::Local(_) => self.locals.push(var_type),
      Identifier::Capture(_) => {}
    }
  }

  fn assign(&mut self, id: Identifier, id_info: SourceInfoHandle, value_type: TypeId) {
    match id {
      Identifier::Global(gid) => {
        unsafe {
          self.code.push_constant(ConstantValue::GlobalId(gid));
          self.code.push_op(OpCode::SetGlobal);
        }
        set_type_or_push_error(
          &mut self.global_types[gid as usize],
          value_type,
          id_info.get(self.ast),
          &mut self.global_env.errors,
          self.type_map,
        );
      }
      Identifier::Local(id) => {
        unsafe {
          self.code.push_op2(OpCode::SetLocal, id);
        }
        set_type_or_push_error(
          &mut self.locals[id as usize],
          value_type,
          id_info.get(self.ast),
          &mut self.global_env.errors,
          self.type_map,
        );
      }
      Identifier::Capture(id) => {
        unsafe {
          self.code.push_op2(OpCode::SetCapture, id);
        }
        set_type_or_push_error(
          &mut self.captures[id as usize],
          value_type,
          id_info.get(self.ast),
          &mut self.global_env.errors,
          self.type_map,
        );
      }
    }
  }

  fn emit_error(&mut self, info: SourceInfoHandle, msg: String) {
    self
      .global_env
      .errors
      .push(error_from_source_info(&info.get(self.ast), msg));
  }

  fn check_function(
    &mut self,
    param_types: &[TypeId],
    captures: &[Identifier],
    declaration_info: SourceInfoHandle,
    body: &[StmtHandle],
  ) {
    let capture_types = captures.iter().map(|id| self.get_typeid(*id)).collect();
    let result = FunctionAnalizer::analyze(
      FunctionInfo {
        parameters: param_types.to_vec(),
        captures: capture_types,
        declaration_src_info: Some(declaration_info),
      },
      body,
      SemAParameters {
        ast: self.ast,
        type_map: self.type_map,
      },
      self.global_env,
      self.global_types,
    );
    unsafe {
      self.code.push_function(result.code);
    }
  }

  pub(super) fn analyze(
    function_info: FunctionInfo,
    body: &[StmtHandle],
    analysis_parameters: SemAParameters<'analysis>,
    global_env: &'analysis mut SemAState,
    global_types: &'analysis mut Vec<TypeId>,
  ) -> FunctionAnalysisResult {
    let mut analizer = Self {
      captures: function_info.captures,
      locals: function_info.parameters,
      scope_depth: 0,
      // TODO: maybe remove this member and add function
      global_scope: function_info.declaration_src_info.is_none(),
      loop_depth: 0,
      code: BytecodeBuilder::new(),
      global_env,
      declaration_info: function_info.declaration_src_info,
      type_map: analysis_parameters.type_map,
      global_types,
      ast: analysis_parameters.ast,
    };
    let ret = analizer.process_statements(body);
    match ret {
      Some(ReturnType::Conditional(TypeId::NOTHING)) | None => {
        unsafe {
          analizer.code.push_constant_none();
          analizer.code.push_op(OpCode::Return);
        }
        FunctionAnalysisResult {
          code: analizer.code,
          return_type: TypeId::NOTHING,
        }
      }
      Some(ReturnType::Conditional(_)) => {
        analizer.emit_error(
          analizer.declaration_info.unwrap(),
          "function has only conditional return types".to_string(),
        );
        FunctionAnalysisResult {
          code: BytecodeBuilder::new(),
          return_type: TypeId::ERROR,
        }
      }
      Some(ReturnType::Unconditional(return_type)) => FunctionAnalysisResult {
        code: analizer.code,
        return_type,
      },
    }
  }
}
