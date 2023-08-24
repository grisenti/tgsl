use std::{marker::PhantomData, slice::Iter};

use super::{expression::*, statement::*, ExprHandle, StmtHandle, AST};

pub trait ExprVisitor<T> {
  fn visit_literal_string(&mut self, ast: &AST, literal_string: &expr::LiteralString) -> T;
  fn visit_literal_number(&mut self, ast: &AST, literal_number: &expr::LiteralNumber) -> T;
  fn visit_literal_bool(&mut self, ast: &AST, literal_bool: &expr::LiteralBool) -> T;
  fn visit_id(&mut self, ast: &AST, id: &expr::Id) -> T;
  fn visit_paren(&mut self, ast: &AST, paren: &expr::Paren) -> T;
  fn visit_assignment(&mut self, ast: &AST, assignment: &expr::Assignment) -> T;
  fn visit_binary(&mut self, ast: &AST, binary: &expr::Binary) -> T;
  fn visit_logical(&mut self, ast: &AST, logical: &expr::Logical) -> T;
  fn visit_unary(&mut self, ast: &AST, unary: &expr::Unary) -> T;
  fn visit_lambda(&mut self, ast: &AST, lambda: &expr::Lambda) -> T;
  fn visit_fn_call(&mut self, ast: &AST, fn_call: &expr::FnCall) -> T;
  fn visit_member_get(&mut self, ast: &AST, member_get: &expr::MemberGet) -> T;
  fn visit_member_set(&mut self, ast: &AST, member_set: &expr::MemberSet) -> T;
  fn visit_dot_call(&mut self, ast: &AST, dot_call: &expr::DotCall) -> T;
  fn visit_constructor(&mut self, ast: &AST, constructor: &expr::Construct) -> T;

  fn visit_expr(&mut self, ast: &AST, expr: ExprHandle) -> T {
    match expr.get(ast) {
      Expr::LiteralString(value) => self.visit_literal_string(ast, value),
      Expr::LiteralNumber(value) => self.visit_literal_number(ast, value),
      Expr::LiteralBool(value) => self.visit_literal_bool(ast, value),
      Expr::Id(value) => self.visit_id(ast, value),
      Expr::Paren(value) => self.visit_paren(ast, value),
      Expr::Assignment(value) => self.visit_assignment(ast, value),
      Expr::Binary(value) => self.visit_binary(ast, value),
      Expr::Logical(value) => self.visit_logical(ast, value),
      Expr::Unary(value) => self.visit_unary(ast, value),
      Expr::Lambda(value) => self.visit_lambda(ast, value),
      Expr::FnCall(value) => self.visit_fn_call(ast, value),
      Expr::MemberGet(value) => self.visit_member_get(ast, value),
      Expr::MemberSet(value) => self.visit_member_set(ast, value),
      Expr::DotCall(value) => self.visit_dot_call(ast, value),
      Expr::Construct(value) => self.visit_constructor(ast, value),
    }
  }
}

pub trait StmtVisitor<T> {
  fn visit_var_decl(&mut self, ast: &AST, var_decl: &stmt::VarDecl) -> T;
  fn visit_stmt_expr(&mut self, ast: &AST, expr: &stmt::StmtExpr) -> T;
  fn visit_block(&mut self, ast: &AST, block: &stmt::Block) -> T;
  fn visit_if_branch(&mut self, ast: &AST, if_branch: &stmt::IfBranch) -> T;
  fn visit_while(&mut self, ast: &AST, while_node: &stmt::While) -> T;
  fn visit_function_definition(
    &mut self,
    ast: &AST,
    function_definition: &stmt::FunctionDefinition,
  ) -> T;
  fn visit_function_declaration(
    &mut self,
    ast: &AST,
    function_declaration: &stmt::FunctionDeclaration,
  ) -> T;
  fn visit_extern_function(&mut self, ast: &AST, extern_function: &stmt::ExternFunction) -> T;
  fn visit_break(&mut self, ast: &AST, break_node: &stmt::Break) -> T;
  fn visit_return(&mut self, ast: &AST, return_stmt: &stmt::Return) -> T;
  fn visit_struct(&mut self, ast: &AST, struct_stmt: &stmt::Struct) -> T;
  fn visit_import(&mut self, ast: &AST, import: &stmt::Import) -> T;

  fn visit_stmt(&mut self, ast: &AST, stmt: StmtHandle) -> T {
    match stmt.get(ast) {
      Stmt::VarDecl(value) => self.visit_var_decl(ast, value),
      Stmt::StmtExpr(value) => self.visit_stmt_expr(ast, value),
      Stmt::Block(value) => self.visit_block(ast, value),
      Stmt::IfBranch(value) => self.visit_if_branch(ast, value),
      Stmt::While(value) => self.visit_while(ast, value),
      Stmt::FunctionDefinition(value) => self.visit_function_definition(ast, value),
      Stmt::FunctionDeclaration(value) => self.visit_function_declaration(ast, value),
      Stmt::ExternFunction(value) => self.visit_extern_function(ast, value),
      Stmt::Break(value) => self.visit_break(ast, value),
      Stmt::Return(value) => self.visit_return(ast, value),
      Stmt::Struct(value) => self.visit_struct(ast, value),
      Stmt::Import(value) => self.visit_import(ast, value),
    }
  }
}

pub trait ASTVisitor<V, S, E>: StmtVisitor<S> + ExprVisitor<E> {}

impl<V, S, E> ASTVisitor<V, S, E> for V where V: StmtVisitor<S> + ExprVisitor<E> {}

pub struct ProgramVisitor<'ast, V, S, E>
where
  V: ASTVisitor<V, S, E>,
{
  visitor: V,
  program_iter: Iter<'ast, StmtHandle>,
  ast: &'ast AST,
  _phantom_expr_value: PhantomData<E>,
  _phantom_stmt_value: PhantomData<S>,
}

impl<'ast, V, S, E> ProgramVisitor<'ast, V, S, E>
where
  V: ASTVisitor<V, S, E>,
{
  pub fn new(visitor: V, ast: &'ast AST) -> Self {
    Self {
      visitor,
      program_iter: ast.program.iter(),
      ast,
      _phantom_expr_value: PhantomData::default(),
      _phantom_stmt_value: PhantomData::default(),
    }
  }

  pub fn visit_program(mut self) -> V {
    for stmt in self.ast.get_program() {
      self.visitor.visit_stmt(self.ast, *stmt);
    }
    self.visitor
  }
}

impl<'ast, V, S, E> Iterator for ProgramVisitor<'ast, V, S, E>
where
  V: StmtVisitor<S> + ExprVisitor<E>,
{
  type Item = S;

  fn next(&mut self) -> Option<Self::Item> {
    if let Some(&stmt) = self.program_iter.next() {
      Some(self.visitor.visit_stmt(self.ast, stmt))
    } else {
      None
    }
  }
}
