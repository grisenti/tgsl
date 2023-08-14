use crate::compiler::ast::expression::expr::{
  Assignment, Binary, DotCall, FnCall, Id, Lambda, LiteralBool, LiteralNumber, LiteralString,
  Logical, MemberGet, MemberSet, Paren, Unary,
};
use crate::compiler::ast::statement::stmt::{
  Block, Break, ExternFunction, FunctionDeclaration, FunctionDefinition, IfBranch, Import, Return,
  StmtExpr, Struct, VarDecl, While,
};
use crate::compiler::ast::visitor::{ExprVisitor, StmtVisitor};
use crate::compiler::ast::AST;
use crate::compiler::bytecode::OpCode;
use crate::compiler::operators::LogicalOperator;
use std::fmt::Debug;

use super::{
  bytecode::ConstantValue,
  identifier::{Identifier, VariableIdentifier},
};

pub struct Label(usize);
pub struct JumpPoint(usize);
pub struct Address(usize);

#[derive(Default, Clone)]
pub struct BytecodeBuilder {
  code: Vec<u8>,
  functions: Vec<BytecodeBuilder>,
  constants: Vec<ConstantValue>,
}

impl BytecodeBuilder {
  pub unsafe fn push_constant(&mut self, val: ConstantValue) {
    let constant_offset = self.constants.len() as u8;
    let constant_type = if matches!(val, ConstantValue::Str(_)) {
      OpCode::ConstantStr
    } else {
      OpCode::Constant
    };
    self.constants.push(val);
    self.code.push(constant_type as u8);
    self.code.push(constant_offset);
  }

  pub unsafe fn push_function(&mut self, func: BytecodeBuilder) {
    let offset = self.functions.len() as u8;
    self.functions.push(func);
    self.code.push(OpCode::Function as u8);
    self.code.push(offset);
  }

  pub unsafe fn push_constant_none(&mut self) {
    self.code.push(OpCode::Constant as u8);
    self.code.push(0);
  }

  pub unsafe fn push_op(&mut self, op: OpCode) {
    self.code.push(op as u8);
  }

  /// pushes opcode with additional data
  pub unsafe fn push_op2(&mut self, op: OpCode, data: u8) {
    self.code.push(op as u8);
    self.code.push(data);
  }

  pub unsafe fn push_jump(&mut self, jump_type: OpCode) -> JumpPoint {
    debug_assert!(matches!(
      jump_type,
      OpCode::Jump | OpCode::JumpIfFalsePop | OpCode::JumpIfFalseNoPop
    ));
    unsafe { self.push_op(jump_type) };
    let index = self.code.len();
    self.code.push(0);
    self.code.push(0);
    JumpPoint(index)
  }

  pub fn push_back_jump(&mut self, Label(to): Label) {
    assert!((self.code.len() - to + 2) <= u16::MAX as usize);
    unsafe { self.push_op(OpCode::BackJump) };
    // 2 added to skip jump point
    let split = ((self.code.len() - to + 2) as u16).to_ne_bytes();
    self.code.push(split[0]);
    self.code.push(split[1]);
  }

  pub fn backpatch_current_instruction(&mut self, JumpPoint(jump_point): JumpPoint) {
    assert!((self.code.len() - jump_point - 2) <= u16::MAX as usize);
    let split = ((self.code.len() - jump_point - 2) as u16).to_ne_bytes();
    // 2 removed to skip jump point
    self.code[jump_point] = split[0];
    self.code[jump_point + 1] = split[1];
  }

  pub fn get_next_instruction_label(&self) -> Label {
    Label(self.code.len())
  }

  pub fn get_next_instruction_address(&self) -> Address {
    Address(self.code.len())
  }

  pub unsafe fn swap(
    &mut self,
    Address(start): Address,
    Address(mid): Address,
    Address(end): Address,
  ) {
    self.code[start..end].rotate_left(mid - start);
  }

  pub unsafe fn get_variable(&mut self, id: VariableIdentifier) {
    match id {
      VariableIdentifier::Global(gid) => {
        self.push_constant(ConstantValue::GlobalId(gid));
        self.push_op(OpCode::GetGlobal);
      }
      VariableIdentifier::Local(id) => {
        self.push_op2(OpCode::GetLocal, id);
      }
      VariableIdentifier::Capture(id) => {
        self.push_op2(OpCode::GetCapture, id);
      }
      VariableIdentifier::Invalid => panic!("codegen with invalid ast"),
    }
  }

  pub unsafe fn set_variable(&mut self, id: VariableIdentifier) {
    match id {
      VariableIdentifier::Global(gid) => {
        self.push_constant(ConstantValue::GlobalId(gid));
        self.push_op(OpCode::SetGlobal);
      }
      VariableIdentifier::Capture(id) => {
        self.push_op2(OpCode::SetCapture, id);
      }
      VariableIdentifier::Local(id) => {
        self.push_op2(OpCode::SetLocal, id);
      }
      VariableIdentifier::Invalid => panic!("codegen with invalid ast"),
    }
  }

  pub fn create_constructor(&mut self, members: u8) {
    let mut constructor_code = Self::new();
    unsafe {
      constructor_code.push_op2(OpCode::Construct, members as u8);
      constructor_code.push_op(OpCode::Return);
      self.push_function(constructor_code);
    }
  }

  pub unsafe fn maybe_create_closure(&mut self, captures: &[VariableIdentifier]) {
    if !captures.is_empty() {
      self.push_op2(OpCode::MakeClosure, captures.len() as u8);
      for c in captures {
        self.get_variable(*c);
        self.push_op(OpCode::Capture);
      }
    }
  }

  pub fn into_parts(self) -> (Vec<u8>, Vec<BytecodeBuilder>, Vec<ConstantValue>) {
    (self.code, self.functions, self.constants)
  }

  pub fn new() -> Self {
    Self {
      code: Vec::new(),
      constants: vec![ConstantValue::None],
      functions: Vec::new(),
    }
  }
}

impl Debug for BytecodeBuilder {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut result = "".to_string();
    for (i, func) in self.functions.iter().enumerate() {
      result += &format!("++ fn {i} ++\n{:?}-- fn {i} --\n", func);
    }
    let mut index = 0;
    while index < self.code.len() {
      let code = unsafe { std::mem::transmute::<u8, OpCode>(self.code[index]) };
      result += &format!("{index}: ");
      match code {
        OpCode::Constant | OpCode::ConstantStr => {
          index += 1;
          result += &format!(
            "Constant: {:?}\n",
            self.constants[self.code[index] as usize]
          );
        }
        OpCode::Function => {
          index += 1;
          result += &format!("Function: {}\n", self.code[index]);
        }
        OpCode::Call => {
          index += 1;
          result += &format!("Call: {}\n", self.code[index]);
        }
        OpCode::JumpIfFalsePop | OpCode::Jump | OpCode::JumpIfFalseNoPop => {
          index += 2;
          let jump_point = u16::from_ne_bytes([self.code[index - 1], self.code[index]]);
          result += &format!("{code:?}: {}\n", index + 1 + jump_point as usize);
        }
        OpCode::BackJump => {
          index += 2;
          let jump_point = u16::from_ne_bytes([self.code[index - 1], self.code[index]]);
          result += &format!("{code:?}: {}\n", index + 1 - jump_point as usize);
        }
        OpCode::GetLocal
        | OpCode::SetLocal
        | OpCode::GetCapture
        | OpCode::SetCapture
        | OpCode::GetMember
        | OpCode::SetMember
        | OpCode::Construct => {
          index += 1;
          result += &format!("{code:?}: {}\n", self.code[index])
        }
        OpCode::MakeClosure => {
          index += 1;
          result += &format!("MakeClosure, captures: {}\n", self.code[index])
        }
        code => result += &format!("{code:?}\n"),
      }
      index += 1;
    }
    write!(f, "{result}")
  }
}

impl ExprVisitor<()> for BytecodeBuilder {
  fn visit_literal_string(&mut self, ast: &AST, literal_string: &LiteralString) {
    unsafe {
      self.push_constant(ConstantValue::Str(
        literal_string.handle.get(ast).to_string(),
      ));
    }
  }

  fn visit_literal_number(&mut self, ast: &AST, literal_number: &LiteralNumber) -> () {
    unsafe {
      self.push_constant(ConstantValue::Number(literal_number.value));
    }
  }

  fn visit_literal_bool(&mut self, ast: &AST, literal_bool: &LiteralBool) {
    unsafe {
      self.push_constant(ConstantValue::Bool(literal_bool.value));
    }
  }

  fn visit_id(&mut self, ast: &AST, id: &Id) -> () {
    match id.id {
      Identifier::Variable(var_id) => unsafe { self.get_variable(var_id) },
      Identifier::ExternFunction(ext_id) => unsafe {
        self.push_constant(ConstantValue::ExternId(ext_id))
      },
      Identifier::Struct(struct_id) => {}
      Identifier::Invalid => panic!("invalid identifier"),
    }
  }

  fn visit_paren(&mut self, ast: &AST, paren: &Paren) -> () {
    self.visit_expr(ast, paren.inner);
  }

  fn visit_assignment(&mut self, ast: &AST, assignment: &Assignment) -> () {
    self.visit_expr(ast, assignment.value);
    unsafe { self.set_variable(assignment.id) };
  }

  fn visit_binary(&mut self, ast: &AST, binary: &Binary) -> () {
    self.visit_expr(ast, binary.left);
    self.visit_expr(ast, binary.right);
    unsafe { self.push_op(binary.operator.into()) };
  }

  fn visit_logical(&mut self, ast: &AST, logical: &Logical) -> () {
    self.visit_expr(ast, logical.left);
    match logical.operator {
      LogicalOperator::And => {
        let jump = unsafe { self.push_jump(OpCode::JumpIfFalseNoPop) };
        unsafe { self.push_op(OpCode::Pop) };
        self.visit_expr(ast, logical.right);
        self.backpatch_current_instruction(jump);
      }
      LogicalOperator::Or => {
        let check_rhs = unsafe { self.push_jump(OpCode::JumpIfFalseNoPop) };
        let skip_rhs_jump = unsafe { self.push_jump(OpCode::Jump) };
        self.backpatch_current_instruction(check_rhs);
        unsafe { self.push_op(OpCode::Pop) };
        self.visit_expr(ast, logical.right);
        self.backpatch_current_instruction(skip_rhs_jump);
      }
      _ => panic!(),
    };
  }

  fn visit_unary(&mut self, ast: &AST, unary: &Unary) -> () {
    self.visit_expr(ast, unary.right);
    unsafe { self.push_op(unary.operator.into()) };
  }

  fn visit_lambda(&mut self, ast: &AST, lambda: &Lambda) -> () {
    let mut code = BytecodeBuilder::new();
    for stmt in &lambda.body {
      code.visit_stmt(ast, *stmt);
    }
    unsafe {
      self.push_function(code);
      self.maybe_create_closure(&lambda.captures);
    }
  }

  fn visit_fn_call(&mut self, ast: &AST, fn_call: &FnCall) -> () {
    self.visit_expr(ast, fn_call.func);
    for arg in &fn_call.arguments {
      self.visit_expr(ast, *arg);
    }
    unsafe { self.push_op2(OpCode::Call, fn_call.arguments.len() as u8) };
  }

  fn visit_member_get(&mut self, ast: &AST, member_get: &MemberGet) -> () {
    self.visit_expr(ast, member_get.lhs);
    unsafe { self.push_op2(OpCode::GetMember, member_get.member_index.get_index() as u8) };
  }

  fn visit_member_set(&mut self, ast: &AST, member_set: &MemberSet) -> () {
    self.visit_expr(ast, member_set.member.lhs);
    self.visit_expr(ast, member_set.value);
    unsafe {
      self.push_op2(
        OpCode::SetMember,
        member_set.member.member_index.get_index() as u8,
      )
    };
  }

  fn visit_dot_call(&mut self, ast: &AST, dot_call: &DotCall) -> () {
    unsafe { self.get_variable(dot_call.function) };
    self.visit_expr(ast, dot_call.lhs);
    for arg in &dot_call.arguments {
      self.visit_expr(ast, *arg);
    }
    unsafe { self.push_op2(OpCode::Call, dot_call.arguments.len() as u8 + 1) };
  }
}

impl StmtVisitor<()> for BytecodeBuilder {
  fn visit_var_decl(&mut self, ast: &AST, var_decl: &VarDecl) -> () {
    self.visit_expr(ast, var_decl.init_expr);
    unsafe { self.set_variable(var_decl.identifier) };
  }

  fn visit_stmt_expr(&mut self, ast: &AST, expr: &StmtExpr) -> () {
    self.visit_expr(ast, expr.expr);
  }

  fn visit_block(&mut self, ast: &AST, block: &Block) -> () {
    for stmt in &block.statements {
      self.visit_stmt(ast, *stmt);
    }
    unsafe { self.push_op2(OpCode::Pop, block.locals) };
  }

  fn visit_if_branch(&mut self, ast: &AST, if_branch: &IfBranch) -> () {
    self.visit_expr(ast, if_branch.condition);
    let if_jump_point = unsafe { self.push_jump(OpCode::JumpIfFalsePop) };
    self.visit_stmt(ast, if_branch.true_branch);
    if let Some(stmt) = if_branch.else_branch {
      let skip_else = unsafe { self.push_jump(OpCode::Jump) };
      self.backpatch_current_instruction(if_jump_point);
      self.visit_stmt(ast, stmt);
      self.backpatch_current_instruction(skip_else);
    } else {
      self.backpatch_current_instruction(if_jump_point);
    };
  }

  fn visit_while(&mut self, ast: &AST, while_node: &While) -> () {
    let label = self.get_next_instruction_label();
    self.visit_expr(ast, while_node.condition);
    let loop_condition = unsafe { self.push_jump(OpCode::JumpIfFalsePop) };
    self.visit_stmt(ast, while_node.loop_body);
    self.push_back_jump(label);
    self.backpatch_current_instruction(loop_condition);
  }

  fn visit_function_definition(
    &mut self,
    ast: &AST,
    function_definition: &FunctionDefinition,
  ) -> () {
    let mut code = BytecodeBuilder::new();
    for stmt in &function_definition.body {
      code.visit_stmt(ast, *stmt);
    }
    unsafe {
      self.push_function(code);
      self.set_variable(function_definition.id);
    }
  }

  fn visit_function_declaration(&mut self, _: &AST, _: &FunctionDeclaration) -> () {
    // nothing
  }

  fn visit_extern_function(&mut self, _: &AST, _: &ExternFunction) -> () {
    // nothing
  }

  fn visit_break(&mut self, ast: &AST, break_node: &Break) -> () {
    // TODO
  }

  fn visit_return(&mut self, ast: &AST, return_stmt: &Return) -> () {
    if let Some(expr) = return_stmt.expr {
      self.visit_expr(ast, expr);
    } else {
      unsafe { self.push_constant_none() };
    }
    unsafe { self.push_op(OpCode::Return) };
  }

  fn visit_struct(&mut self, ast: &AST, struct_stmt: &Struct) -> () {
    // nothing
  }

  fn visit_import(&mut self, ast: &AST, import: &Import) -> () {
    // nothing
  }
}
