use crate::compiler::ast::parsed_type::{ParsedFunctionType, ParsedType};
use crate::compiler::ast::TypeHandle;
use std::{marker::PhantomData, slice::Iter};

use super::{expression::*, statement::*, ExprHandle, StmtHandle, AST};

pub trait ExprVisitor<T> {
  fn visit_literal(&mut self, ast: &AST, literal: &expr::Literal) -> T;
  fn visit_id(&mut self, ast: &AST, id: &expr::Id) -> T;
  fn visit_paren(&mut self, ast: &AST, paren: &expr::Paren) -> T;
  fn visit_assignment(&mut self, ast: &AST, assignment: &expr::Assignment) -> T;
  fn visit_binary(&mut self, ast: &AST, binary: &expr::Binary) -> T;
  fn visit_unary(&mut self, ast: &AST, unary: &expr::Unary) -> T;
  fn visit_lambda(&mut self, ast: &AST, lambda: &expr::Lambda) -> T;
  fn visit_fn_call(&mut self, ast: &AST, fn_call: &expr::FnCall) -> T;
  fn visit_dot(&mut self, ast: &AST, dot: &expr::Dot) -> T;
  fn visit_constructor(&mut self, ast: &AST, constructor: &expr::Construct) -> T;

  fn visit_expr(&mut self, ast: &AST, expr: ExprHandle) -> T {
    match expr.get_expr(ast) {
      Expr::Literal(value) => self.visit_literal(ast, value),
      Expr::Id(value) => self.visit_id(ast, value),
      Expr::Paren(value) => self.visit_paren(ast, value),
      Expr::Assignment(value) => self.visit_assignment(ast, value),
      Expr::Binary(value) => self.visit_binary(ast, value),
      Expr::Unary(value) => self.visit_unary(ast, value),
      Expr::Lambda(value) => self.visit_lambda(ast, value),
      Expr::FnCall(value) => self.visit_fn_call(ast, value),
      Expr::Dot(value) => self.visit_dot(ast, value),
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
  fn visit_break(&mut self, ast: &AST) -> T;
  fn visit_return(&mut self, ast: &AST, return_stmt: &stmt::Return) -> T;
  fn visit_struct(&mut self, ast: &AST, struct_stmt: &stmt::Struct) -> T;
  fn visit_import(&mut self, ast: &AST, import: &stmt::Import) -> T;
  fn visit_module_decl(&mut self, ast: &AST, module_decl: &stmt::ModuleDecl) -> T;

  fn visit_stmt(&mut self, ast: &AST, stmt: StmtHandle) -> T {
    match stmt.get_stmt(ast) {
      Stmt::VarDecl(value) => self.visit_var_decl(ast, value),
      Stmt::StmtExpr(value) => self.visit_stmt_expr(ast, value),
      Stmt::Block(value) => self.visit_block(ast, value),
      Stmt::IfBranch(value) => self.visit_if_branch(ast, value),
      Stmt::While(value) => self.visit_while(ast, value),
      Stmt::FunctionDefinition(value) => self.visit_function_definition(ast, value),
      Stmt::FunctionDeclaration(value) => self.visit_function_declaration(ast, value),
      Stmt::ExternFunction(value) => self.visit_extern_function(ast, value),
      Stmt::Break => self.visit_break(ast),
      Stmt::Return(value) => self.visit_return(ast, value),
      Stmt::Struct(value) => self.visit_struct(ast, value),
      Stmt::Import(value) => self.visit_import(ast, value),
      Stmt::ModuleDecl(value) => self.visit_module_decl(ast, value),
    }
  }
}

pub trait ParsedTypeVisitor<T> {
  fn visit_num(&mut self, ast: &AST) -> T;
  fn visit_str(&mut self, ast: &AST) -> T;
  fn visit_bool(&mut self, ast: &AST) -> T;
  fn visit_nothing(&mut self, ast: &AST) -> T;
  fn visit_any(&mut self, ast: &AST) -> T;
  fn visit_named(&mut self, ast: &AST, name: &str) -> T;
  fn visit_function(&mut self, ast: &AST, function: &ParsedFunctionType) -> T;

  fn visit_parsed_type(&mut self, ast: &AST, handle: TypeHandle) -> T {
    match handle.get_type(ast) {
      ParsedType::Num => self.visit_num(ast),
      ParsedType::Str => self.visit_str(ast),
      ParsedType::Bool => self.visit_bool(ast),
      ParsedType::Nothing => self.visit_nothing(ast),
      ParsedType::Any => self.visit_any(ast),
      ParsedType::Named(name) => self.visit_named(ast, name),
      ParsedType::Function(function) => self.visit_function(ast, function),
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
  ast: &'ast AST<'ast>,
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
