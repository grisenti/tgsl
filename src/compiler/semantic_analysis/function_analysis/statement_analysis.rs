use crate::compiler::{
  ast::{Expr, ExprHandle, StrHandle},
  bytecode::ConstantValue,
  identifier::ExternId,
  semantic_analysis::{return_analysis::to_conditional, Struct},
};

use super::*;

type OptRet = Option<ReturnType>;

impl FunctionAnalizer<'_> {
  fn merge_return_types(&mut self, r1: OptRet, r2: OptRet) -> OptRet {
    match (r1, r2) {
      (Some(ret1), Some(ret2)) => ret1.merge(ret2).or_else(|| {
        let decl_sr = self.declaration_info.unwrap();
        self.emit_error(sema_err::inconsistent_return_types(decl_sr));
        Some(ReturnType::Unconditional(TypeId::ERROR))
      }),
      (None, Some(ret)) | (Some(ret), None) => Some(ret),
      _ => None,
    }
  }

  pub fn process_statements(&mut self, statements: &[StmtHandle]) -> Option<ReturnType> {
    let mut return_type: OptRet = None;
    for s in statements {
      let ret = self.analyze_stmt(*s);
      return_type = self.merge_return_types(return_type, ret);
    }
    return_type
  }

  fn check_self_assignment(&mut self, lhs_id: Identifier, expression: ExprHandle) {
    match expression.get(self.ast) {
      &Expr::Variable { id: rhs_id, id_sr } if rhs_id == lhs_id => {
        self.emit_error(sema_err::cannot_initialize_with_itself(id_sr));
      }
      _ => {}
    }
  }

  fn var_decl(
    &mut self,
    id: Identifier,
    var_type: TypeId,
    id_sr: SourceRange,
    init_expr: Option<ExprHandle>,
  ) {
    if let Identifier::Local(_) = id {
      self.locals.push(var_type);
    }
    if let Some(expr) = init_expr {
      self.check_self_assignment(id, expr);
      let rhs = self.analyze_expr(expr);
      self.assign(id, id_sr, rhs);
    } else {
      unsafe { self.code.push_constant_none() }
    }
  }

  fn check_valid_condition_type(&mut self, condition_sr: SourceRange, condition_type: TypeId) {
    if condition_type != TypeId::BOOL {
      self.emit_error(sema_err::incorrect_conditional_type(
        condition_sr,
        self.type_string(condition_type),
      ));
    }
  }

  fn while_stmt(
    &mut self,
    while_sr: SourceRange,
    condition: ExprHandle,
    loop_body: StmtHandle,
  ) -> OptRet {
    let label = self.code.get_next_instruction_label();
    let condition_type = self.analyze_expr(condition);
    let loop_condition = unsafe { self.code.push_jump(OpCode::JumpIfFalsePop) };
    self.check_valid_condition_type(while_sr, condition_type);
    self.loop_depth += 1;
    let ret = self.analyze_stmt(loop_body);
    self.loop_depth -= 1;
    self.code.push_back_jump(label);
    self.code.backpatch_current_instruction(loop_condition);
    to_conditional(ret)
  }

  fn generate_constructor(&mut self, id: Identifier, members: usize, constructor_type: TypeId) {
    self.code.create_constructor(members as u8);
    self.declare(id, constructor_type);
  }

  fn struct_stmt(
    &mut self,
    id: Identifier,
    _name_sr: SourceRange,
    member_names: &[StrHandle],
    member_types: &[TypeId],
    constructor_type: TypeId,
  ) {
    self.generate_constructor(id, member_types.len(), constructor_type);
    self.global_env.structs.insert(
      id,
      Struct {
        member_names: member_names.to_vec(),
        member_types: member_types.to_vec(),
      },
    );
  }

  fn if_stmt(
    &mut self,
    if_sr: SourceRange,
    condition: ExprHandle,
    true_branch: StmtHandle,
    else_branch: Option<StmtHandle>,
  ) -> OptRet {
    let condition_type = self.analyze_expr(condition);
    self.check_valid_condition_type(if_sr, condition_type);
    let if_jump_point = unsafe { self.code.push_jump(OpCode::JumpIfFalsePop) };
    let ret_true = to_conditional(self.analyze_stmt(true_branch));
    let ret_false = if let Some(stmt) = else_branch {
      let skip_else = unsafe { self.code.push_jump(OpCode::Jump) };
      self.code.backpatch_current_instruction(if_jump_point);
      let res = self.analyze_stmt(stmt);
      self.code.backpatch_current_instruction(skip_else);
      res
    } else {
      self.code.backpatch_current_instruction(if_jump_point);
      None
    };
    self.merge_return_types(ret_true, ret_false)
  }

  fn block_stmt(&mut self, statements: &[StmtHandle], locals: u8) -> OptRet {
    self.push_scope();
    let ret = self.process_statements(statements);
    self.pop_scope(locals);
    ret
  }

  fn function_stmt(
    &mut self,
    id: Identifier,
    parameters: &[TypeId],
    fn_type: TypeId,
    captures: &[Identifier],
    declaration_sr: SourceRange,
    body: &[StmtHandle],
  ) {
    self.check_function(parameters, captures, declaration_sr, body);
    unsafe {
      self.declare(id, fn_type);
      self.code.maybe_create_closure(captures);
    }
  }

  fn break_stmt(&mut self, break_sr: SourceRange) {
    if self.loop_depth == 0 {
      self.emit_error(sema_err::break_outside_loop(break_sr));
    }
  }

  fn return_stmt(&mut self, return_expr: Option<ExprHandle>, return_sr: SourceRange) -> ReturnType {
    if self.global_scope {
      self.emit_error(sema_err::return_outside_function(return_sr));
      return ReturnType::Unconditional(TypeId::ERROR);
    }
    let ret = if let Some(expr) = return_expr {
      ReturnType::Unconditional(self.analyze_expr(expr))
    } else {
      unsafe { self.code.push_constant_none() }
      ReturnType::Unconditional(TypeId::NOTHING)
    };
    unsafe { self.code.push_op(OpCode::Return) };
    ret
  }

  fn expr_stmt(&mut self, expr: ExprHandle) {
    self.analyze_expr(expr);
    unsafe { self.code.push_op(OpCode::Pop) }
  }

  fn extern_function(&mut self, name_id: Identifier, fn_type: TypeId, extern_id: ExternId) {
    unsafe {
      self
        .code
        .push_constant(ConstantValue::ExternId(extern_id.0));
    }
    self.declare(name_id, fn_type);
  }

  pub(super) fn analyze_stmt(&mut self, stmt: StmtHandle) -> OptRet {
    match stmt.get(self.ast) {
      Stmt::VarDecl {
        identifier,
        id_sr,
        var_type,
        expression,
      } => {
        self.var_decl(*identifier, *var_type, *id_sr, *expression);
        None
      }
      Stmt::While {
        while_sr,
        condition,
        loop_body,
      } => self.while_stmt(*while_sr, *condition, *loop_body),
      Stmt::Struct {
        id,
        name_sr,
        member_names,
        member_types,
        constructor_type,
        ..
      } => {
        self.struct_stmt(*id, *name_sr, member_names, member_types, *constructor_type);
        None
      }
      Stmt::IfBranch {
        if_sr,
        condition,
        true_branch,
        else_branch,
      } => self.if_stmt(*if_sr, *condition, *true_branch, *else_branch),
      Stmt::Block { statements, locals } => self.block_stmt(statements, *locals),
      Stmt::FunctionDefinition {
        id,
        name_sr,
        captures,
        fn_type,
        body,
        parameter_types,
        ..
      } => {
        self.function_stmt(*id, parameter_types, *fn_type, captures, *name_sr, body);
        None
      }
      Stmt::Break(info) => {
        self.break_stmt(*info);
        None
      }
      Stmt::Return { expr, return_sr } => Some(self.return_stmt(*expr, *return_sr)),
      Stmt::Expr(expr) => {
        self.expr_stmt(*expr);
        None
      }
      Stmt::ExternFunction {
        name_id,
        fn_type,
        extern_id,
        ..
      } => {
        self.extern_function(*name_id, *fn_type, *extern_id);
        None
      }
      Stmt::Import { .. } => None,
      Stmt::FunctionDeclaration { id, fn_type, .. } => {
        self.declare(*id, *fn_type);
        None
      }
    }
  }
}
