use crate::compiler::{
  ast::{Expr, ExprHandle, SourceInfoHandle, StrHandle},
  bytecode::{ConstantValue, Function},
  identifier::ExternId,
  semantic_analysis::{return_analysis::to_conditional, Struct},
};

use super::*;

type OptRet = Option<ReturnType>;

impl FunctionAnalizer<'_> {
  fn merge_return_types(&mut self, r1: OptRet, r2: OptRet) -> OptRet {
    match (r1, r2) {
      (Some(ret1), Some(ret2)) => ret1.merge(ret2).or_else(|| {
        self.emit_error(
          self.declaration_info.unwrap(),
          "inconsistent return types in function".to_string(),
        );
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
    match expression.get(&self.global_env.ast) {
      Expr::Variable {
        id: rhs_id,
        id_info,
      } if rhs_id == lhs_id => {
        self.emit_error(
          id_info,
          "cannot initialize identifier with itself".to_string(),
        );
      }
      _ => {}
    }
  }

  fn var_decl(
    &mut self,
    id: Identifier,
    var_type: TypeId,
    id_info: SourceInfoHandle,
    init_expr: Option<ExprHandle>,
  ) {
    if let Identifier::Local(_) = id {
      self.locals.push(var_type);
    }
    if let Some(expr) = init_expr {
      self.check_self_assignment(id, expr);
      let rhs = self.analyze_expr(expr);
      self.assign(id, id_info, rhs);
    } else {
      unsafe { self.code.push_constant_none() }
    }
  }

  fn check_valid_condition_type(&mut self, info: SourceInfoHandle, condition_type: TypeId) {
    if condition_type != TypeId::BOOL {
      self.emit_error(
        info,
        format!(
          "cannot use value of type {} in a condition",
          self.type_string(condition_type)
        ),
      )
    }
  }

  fn while_stmt(
    &mut self,
    info: SourceInfoHandle,
    condition: ExprHandle,
    loop_body: StmtHandle,
  ) -> OptRet {
    let label = self.code.get_next_instruction_label();
    let condition_type = self.analyze_expr(condition);
    let loop_condition = unsafe { self.code.push_jump(OpCode::JumpIfFalsePop) };
    self.check_valid_condition_type(info, condition_type);
    self.loop_depth += 1;
    let ret = self.analyze_stmt(loop_body);
    self.loop_depth -= 1;
    self.code.push_back_jump(label);
    self.code.backpatch_current_instruction(loop_condition);
    to_conditional(ret)
  }

  fn generate_constructor(
    &mut self,
    id: Identifier,
    member_types: Vec<TypeId>,
    constructor_type: TypeId,
  ) {
    let members = member_types.len();
    // TODO: maybe move this to codegen
    // --
    let mut constructor_code = BytecodeBuilder::new();
    unsafe {
      constructor_code.push_op2(OpCode::Construct, members as u8);
      constructor_code.push_op(OpCode::Return);
      self.code.push_function(Function {
        code: constructor_code.finalize(),
      });
    }
    self.declare(id, constructor_type);
    // --
  }

  fn struct_stmt(
    &mut self,
    id: Identifier,
    name_info: SourceInfoHandle,
    member_names: Vec<StrHandle>,
    member_types: Vec<TypeId>,
    constructor_type: TypeId,
  ) {
    if !self.global_scope || self.scope_depth > 0 {
      self.emit_error(
        name_info,
        "cannot declare struct in local scope".to_string(),
      )
    }
    self.generate_constructor(id, member_types.clone(), constructor_type);
    self.global_env.structs.insert(
      id,
      Struct {
        member_names,
        member_types,
      },
    );
  }

  fn if_stmt(
    &mut self,
    if_info: SourceInfoHandle,
    condition: ExprHandle,
    true_branch: StmtHandle,
    else_branch: Option<StmtHandle>,
  ) -> OptRet {
    let condition_type = self.analyze_expr(condition);
    self.check_valid_condition_type(if_info, condition_type);
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

  fn block_stmt(&mut self, statements: Vec<StmtHandle>, locals: u8) -> OptRet {
    self.push_scope();
    let ret = self.process_statements(&statements);
    self.pop_scope(locals);
    ret
  }

  fn function_stmt(
    &mut self,
    id: Identifier,
    parameters: Vec<TypeId>,
    fn_type: TypeId,
    captures: Vec<Identifier>,
    declaration_info: SourceInfoHandle,
    body: Vec<StmtHandle>,
  ) {
    self.check_function(&parameters, &captures, declaration_info, body);
    unsafe {
      self.declare(id, fn_type);
      self.code.maybe_create_closure(&captures);
    }
  }

  fn break_stmt(&mut self, info: SourceInfoHandle) {
    if self.loop_depth == 0 {
      self.emit_error(info, "cannot have break outside of loop body".to_string());
    }
  }

  fn return_stmt(
    &mut self,
    return_expr: Option<ExprHandle>,
    src_info: SourceInfoHandle,
  ) -> ReturnType {
    if self.global_scope {
      self.emit_error(
        src_info,
        "cannot have return outside of function body".to_string(),
      );
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

  fn print_stmt(&mut self, expr: ExprHandle) {
    self.analyze_expr(expr);
    unsafe { self.code.push_op(OpCode::Print) };
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
    match stmt.clone().get(&self.global_env.ast) {
      Stmt::VarDecl {
        identifier,
        id_info,
        var_type,
        expression,
      } => {
        self.var_decl(identifier, var_type, id_info, expression);
        None
      }
      Stmt::While {
        info,
        condition,
        loop_body,
      } => self.while_stmt(info, condition, loop_body),
      Stmt::Struct {
        name,
        name_info,
        member_names,
        member_types,
        struct_type,
        constructor_type,
      } => {
        self.struct_stmt(
          name,
          name_info,
          member_names,
          member_types,
          constructor_type,
        );
        None
      }
      Stmt::IfBranch {
        if_info,
        condition,
        true_branch,
        else_branch,
      } => self.if_stmt(if_info, condition, true_branch, else_branch),
      Stmt::Block { statements, locals } => self.block_stmt(statements, locals),
      Stmt::Function {
        id,
        name_info,
        captures,
        fn_type,
        body,
        parameters,
        ..
      } => {
        self.function_stmt(id, parameters, fn_type, captures, name_info, body);
        None
      }
      Stmt::Break(info) => {
        self.break_stmt(info);
        None
      }
      Stmt::Return { expr, src_info } => Some(self.return_stmt(expr, src_info)),
      Stmt::Print(expr) => {
        self.print_stmt(expr);
        None
      }
      Stmt::Expr(expr) => {
        self.expr_stmt(expr);
        None
      }
      Stmt::ExternFunction {
        name_id,
        fn_type,
        extern_id,
        ..
      } => {
        self.extern_function(name_id, fn_type, extern_id);
        None
      }
    }
  }
}
