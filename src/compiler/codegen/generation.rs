use crate::compiler::ast::expression::expr;
use crate::compiler::ast::statement::stmt;
use crate::compiler::ast::visitor::{ExprVisitor, StmtVisitor};
use crate::compiler::ast::{StmtHandle, AST};
use crate::compiler::codegen::bytecode::{ConstantValue, OpCode};
use crate::compiler::codegen::function_code::FunctionCode;
use crate::compiler::identifier::Identifier;
use crate::compiler::operators::LogicalOperator;

pub struct BytecodeGenerator<'g> {
  code: FunctionCode,
  functions: &'g mut Vec<FunctionCode>,
}

impl<'g> BytecodeGenerator<'g> {
  pub(crate) fn generate_function(
    statements: &[StmtHandle],
    ast: &AST,
    functions: &'g mut Vec<FunctionCode>,
  ) -> FunctionCode {
    let mut generator = Self {
      code: FunctionCode::default(),
      functions,
    };
    for stmt in statements {
      generator.visit_stmt(ast, *stmt);
    }
    generator.code
  }
}

impl ExprVisitor<()> for BytecodeGenerator<'_> {
  fn visit_literal_string(&mut self, ast: &AST, literal_string: &expr::LiteralString) {
    unsafe {
      self.code.push_constant(ConstantValue::Str(
        literal_string.handle.get(ast).to_string(),
      ));
    }
  }

  fn visit_literal_number(&mut self, _ast: &AST, literal_number: &expr::LiteralNumber) -> () {
    unsafe {
      self
        .code
        .push_constant(ConstantValue::Number(literal_number.value));
    }
  }

  fn visit_literal_bool(&mut self, _ast: &AST, literal_bool: &expr::LiteralBool) {
    unsafe {
      self
        .code
        .push_constant(ConstantValue::Bool(literal_bool.value));
    }
  }

  fn visit_id(&mut self, _ast: &AST, id: &expr::Id) -> () {
    match id.id {
      Identifier::Variable(var_id) => unsafe { self.code.get_variable(var_id) },
      Identifier::ExternFunction(ext_id) => unsafe {
        self.code.push_constant(ConstantValue::ExternId(ext_id))
      },
      Identifier::Function(function_id) => unsafe {
        self
          .code
          .push_constant(ConstantValue::FunctionId(function_id))
      },
      Identifier::Struct(_) => {}
      Identifier::Invalid => panic!("invalid identifier"),
    }
  }

  fn visit_paren(&mut self, ast: &AST, paren: &expr::Paren) -> () {
    self.visit_expr(ast, paren.inner);
  }

  fn visit_assignment(&mut self, ast: &AST, assignment: &expr::Assignment) -> () {
    self.visit_expr(ast, assignment.value);
    unsafe { self.code.set_variable(assignment.id) };
  }

  fn visit_binary(&mut self, ast: &AST, binary: &expr::Binary) -> () {
    self.visit_expr(ast, binary.left);
    self.visit_expr(ast, binary.right);
    unsafe { self.code.push_op(binary.operator.into()) };
  }

  fn visit_logical(&mut self, ast: &AST, logical: &expr::Logical) -> () {
    self.visit_expr(ast, logical.left);
    match logical.operator {
      LogicalOperator::And => {
        let jump = unsafe { self.code.push_jump(OpCode::JumpIfFalseNoPop) };
        unsafe { self.code.push_op(OpCode::Pop) };
        self.visit_expr(ast, logical.right);
        self.code.backpatch_current_instruction(jump);
      }
      LogicalOperator::Or => {
        let check_rhs = unsafe { self.code.push_jump(OpCode::JumpIfFalseNoPop) };
        let skip_rhs_jump = unsafe { self.code.push_jump(OpCode::Jump) };
        self.code.backpatch_current_instruction(check_rhs);
        unsafe { self.code.push_op(OpCode::Pop) };
        self.visit_expr(ast, logical.right);
        self.code.backpatch_current_instruction(skip_rhs_jump);
      }
      _ => panic!(),
    };
  }

  fn visit_unary(&mut self, ast: &AST, unary: &expr::Unary) -> () {
    self.visit_expr(ast, unary.right);
    unsafe { self.code.push_op(unary.operator.into()) };
  }

  fn visit_lambda(&mut self, ast: &AST, lambda: &expr::Lambda) -> () {
    let function_code =
      BytecodeGenerator::generate_function(&lambda.body, ast, &mut self.functions);
    let function_id = lambda.id.get_id() as usize;
    assert_eq!(self.functions.len(), function_id);
    self.functions.push(function_code);
    unsafe {
      self
        .code
        .push_constant(ConstantValue::FunctionId(lambda.id));
      self.code.maybe_create_closure(&lambda.captures);
    }
  }

  fn visit_fn_call(&mut self, ast: &AST, fn_call: &expr::FnCall) -> () {
    self.visit_expr(ast, fn_call.func);
    for arg in &fn_call.arguments {
      self.visit_expr(ast, *arg);
    }
    unsafe {
      self
        .code
        .push_op2(OpCode::Call, fn_call.arguments.len() as u8)
    };
  }

  fn visit_member_get(&mut self, ast: &AST, member_get: &expr::MemberGet) -> () {
    self.visit_expr(ast, member_get.lhs);
    unsafe {
      self
        .code
        .push_op2(OpCode::GetMember, member_get.member_index.get_index() as u8)
    };
  }

  fn visit_member_set(&mut self, ast: &AST, member_set: &expr::MemberSet) -> () {
    self.visit_expr(ast, member_set.member.lhs);
    self.visit_expr(ast, member_set.value);
    unsafe {
      self.code.push_op2(
        OpCode::SetMember,
        member_set.member.member_index.get_index() as u8,
      )
    };
  }

  fn visit_dot_call(&mut self, ast: &AST, dot_call: &expr::DotCall) -> () {
    unsafe { self.code.get_variable(dot_call.function) };
    self.visit_expr(ast, dot_call.lhs);
    for arg in &dot_call.arguments {
      self.visit_expr(ast, *arg);
    }
    unsafe {
      self
        .code
        .push_op2(OpCode::Call, dot_call.arguments.len() as u8 + 1)
    };
  }

  fn visit_constructor(&mut self, ast: &AST, constructor: &expr::Construct) -> () {
    for arg in &constructor.arguments {
      self.visit_expr(ast, *arg);
    }
    unsafe {
      self
        .code
        .push_op2(OpCode::Construct, constructor.arguments.len() as u8)
    };
  }
}

impl StmtVisitor<()> for BytecodeGenerator<'_> {
  fn visit_var_decl(&mut self, ast: &AST, var_decl: &stmt::VarDecl) -> () {
    self.visit_expr(ast, var_decl.init_expr);
    unsafe { self.code.set_variable(var_decl.identifier) };
  }

  fn visit_stmt_expr(&mut self, ast: &AST, expr: &stmt::StmtExpr) -> () {
    self.visit_expr(ast, expr.expr);
  }

  fn visit_block(&mut self, ast: &AST, block: &stmt::Block) -> () {
    for stmt in &block.statements {
      self.visit_stmt(ast, *stmt);
    }
    unsafe { self.code.push_op2(OpCode::Pop, block.locals) };
  }

  fn visit_if_branch(&mut self, ast: &AST, if_branch: &stmt::IfBranch) -> () {
    self.visit_expr(ast, if_branch.condition);
    let if_jump_point = unsafe { self.code.push_jump(OpCode::JumpIfFalsePop) };
    self.visit_stmt(ast, if_branch.true_branch);
    if let Some(stmt) = if_branch.else_branch {
      let skip_else = unsafe { self.code.push_jump(OpCode::Jump) };
      self.code.backpatch_current_instruction(if_jump_point);
      self.visit_stmt(ast, stmt);
      self.code.backpatch_current_instruction(skip_else);
    } else {
      self.code.backpatch_current_instruction(if_jump_point);
    };
  }

  fn visit_while(&mut self, ast: &AST, while_node: &stmt::While) -> () {
    let label = self.code.get_next_instruction_label();
    self.visit_expr(ast, while_node.condition);
    let loop_condition = unsafe { self.code.push_jump(OpCode::JumpIfFalsePop) };
    self.visit_stmt(ast, while_node.loop_body);
    self.code.push_back_jump(label);
    self.code.backpatch_current_instruction(loop_condition);
  }

  fn visit_function_definition(
    &mut self,
    ast: &AST,
    function_definition: &stmt::FunctionDefinition,
  ) -> () {
    assert!(function_definition.id.is_relative());

    let function_code =
      BytecodeGenerator::generate_function(&function_definition.body, ast, &mut self.functions);
    let function_id = function_definition.id.get_id() as usize;
    if function_id < self.functions.len() {
      self.functions[function_id] = function_code;
    } else {
      assert_eq!(function_id, self.functions.len());
      self.functions.push(function_code);
    }
  }

  fn visit_function_declaration(
    &mut self,
    _: &AST,
    function_declaration: &stmt::FunctionDeclaration,
  ) -> () {
    let function_id = function_declaration.id.get_id() as usize;
    // cannot be bigger, otherwise declarations are not ordered
    assert!(function_id <= self.functions.len());
    if function_id == self.functions.len() {
      // first function declaration. If function_id > self.functions.len()
      self.functions.push(FunctionCode::default());
    }
  }

  fn visit_extern_function(&mut self, _: &AST, _: &stmt::ExternFunction) -> () {
    // nothing
  }

  fn visit_break(&mut self, _ast: &AST, _break_node: &stmt::Break) -> () {
    // TODO
  }

  fn visit_return(&mut self, ast: &AST, return_stmt: &stmt::Return) -> () {
    if let Some(expr) = return_stmt.expr {
      self.visit_expr(ast, expr);
    } else {
      unsafe { self.code.push_constant_none() };
    }
    unsafe { self.code.push_op(OpCode::Return) };
  }

  fn visit_struct(&mut self, _ast: &AST, _struct_stmt: &stmt::Struct) -> () {
    // nothing
  }

  fn visit_import(&mut self, _ast: &AST, _import: &stmt::Import) -> () {
    // nothing
  }
}
