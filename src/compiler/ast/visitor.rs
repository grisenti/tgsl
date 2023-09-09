use crate::compiler::ast::parsed_type::{ParsedFunctionType, ParsedType};
use crate::compiler::ast::TypeHandle;
use std::{marker::PhantomData, slice::Iter};

use super::{expression::*, statement::*, ExprHandle, StmtHandle, AST};

pub trait ExprVisitor<'ast, 'src, T> {
  fn visit_literal(
    &mut self,
    ast: &'ast AST,
    literal: &'ast expr::Literal<'src>,
    expr: ExprHandle,
  ) -> T;
  fn visit_id(&mut self, ast: &'ast AST, id: &'ast expr::Id<'src>, expr: ExprHandle) -> T;
  fn visit_paren(&mut self, ast: &'ast AST, paren: &'ast expr::Paren, expr: ExprHandle) -> T;
  fn visit_assignment(
    &mut self,
    ast: &'ast AST,
    assignment: &'ast expr::Assignment<'src>,
    expr: ExprHandle,
  ) -> T;
  fn visit_binary(
    &mut self,
    ast: &'ast AST,
    binary: &'ast expr::Binary<'src>,
    expr: ExprHandle,
  ) -> T;
  fn visit_unary(&mut self, ast: &'ast AST, unary: &'ast expr::Unary<'src>, expr: ExprHandle) -> T;
  fn visit_lambda(
    &mut self,
    ast: &'ast AST,
    lambda: &'ast expr::Lambda<'src>,
    expr: ExprHandle,
  ) -> T;
  fn visit_fn_call(&mut self, ast: &'ast AST, fn_call: &'ast expr::FnCall, expr: ExprHandle) -> T;
  fn visit_member_get(
    &mut self,
    ast: &'ast AST,
    member_get: &'ast expr::MemberGet<'src>,
    expr: ExprHandle,
  ) -> T;
  fn visit_dot_call(
    &mut self,
    ast: &'ast AST,
    dot_call: &'ast expr::DotCall<'src>,
    expr: ExprHandle,
  ) -> T;
  fn visit_constructor(
    &mut self,
    ast: &'ast AST,
    constructor: &'ast expr::Construct<'src>,
    expr: ExprHandle,
  ) -> T;

  fn visit_expr(&mut self, ast: &'ast AST<'src>, expr: ExprHandle) -> T {
    match expr.get_expr(ast) {
      Expr::Literal(value) => self.visit_literal(ast, value, expr),
      Expr::Id(value) => self.visit_id(ast, value, expr),
      Expr::Paren(value) => self.visit_paren(ast, value, expr),
      Expr::Assignment(value) => self.visit_assignment(ast, value, expr),
      Expr::Binary(value) => self.visit_binary(ast, value, expr),
      Expr::Unary(value) => self.visit_unary(ast, value, expr),
      Expr::Lambda(value) => self.visit_lambda(ast, value, expr),
      Expr::FnCall(value) => self.visit_fn_call(ast, value, expr),
      Expr::MemberGet(value) => self.visit_member_get(ast, value, expr),
      Expr::DotCall(value) => self.visit_dot_call(ast, value, expr),
      Expr::Construct(value) => self.visit_constructor(ast, value, expr),
    }
  }
}

pub trait StmtVisitor<'ast, 'src, T> {
  fn visit_var_decl(
    &mut self,
    ast: &'ast AST,
    var_decl: &'ast stmt::VarDecl<'src>,
    stmt_handle: StmtHandle,
  ) -> T;
  fn visit_stmt_expr(
    &mut self,
    ast: &'ast AST,
    expr: &'ast stmt::StmtExpr,
    stmt_handle: StmtHandle,
  ) -> T;
  fn visit_block(&mut self, ast: &'ast AST, block: &'ast stmt::Block, stmt_handle: StmtHandle)
    -> T;
  fn visit_if_branch(
    &mut self,
    ast: &'ast AST,
    if_branch: &'ast stmt::IfBranch,
    stmt_handle: StmtHandle,
  ) -> T;
  fn visit_while(
    &mut self,
    ast: &'ast AST,
    while_node: &'ast stmt::While,
    stmt_handle: StmtHandle,
  ) -> T;
  fn visit_function_definition(
    &mut self,
    ast: &'ast AST,
    function_definition: &'ast stmt::FunctionDefinition<'src>,
    stmt_handle: StmtHandle,
  ) -> T;
  fn visit_function_declaration(
    &mut self,
    ast: &'ast AST,
    function_declaration: &'ast stmt::FunctionDeclaration<'src>,
    stmt_handle: StmtHandle,
  ) -> T;
  fn visit_extern_function(
    &mut self,
    ast: &'ast AST,
    extern_function: &'ast stmt::ExternFunction<'src>,
    stmt_handle: StmtHandle,
  ) -> T;
  fn visit_break(&mut self, ast: &'ast AST, stmt_handle: StmtHandle) -> T;
  fn visit_return(
    &mut self,
    ast: &'ast AST,
    return_stmt: &'ast stmt::Return,
    stmt_handle: StmtHandle,
  ) -> T;
  fn visit_struct(
    &mut self,
    ast: &'ast AST,
    struct_stmt: &'ast stmt::Struct<'src>,
    stmt_handle: StmtHandle,
  ) -> T;
  fn visit_import(
    &mut self,
    ast: &'ast AST,
    import: &'ast stmt::Import<'src>,
    stmt_handle: StmtHandle,
  ) -> T;
  fn visit_module_decl(
    &mut self,
    ast: &'ast AST,
    module_decl: &'ast stmt::ModuleDecl<'src>,
    stmt_handle: StmtHandle,
  ) -> T;

  fn visit_stmt(&mut self, ast: &'ast AST<'src>, stmt: StmtHandle) -> T {
    match stmt.get_stmt(ast) {
      Stmt::VarDecl(value) => self.visit_var_decl(ast, value, stmt),
      Stmt::StmtExpr(value) => self.visit_stmt_expr(ast, value, stmt),
      Stmt::Block(value) => self.visit_block(ast, value, stmt),
      Stmt::IfBranch(value) => self.visit_if_branch(ast, value, stmt),
      Stmt::While(value) => self.visit_while(ast, value, stmt),
      Stmt::FunctionDefinition(value) => self.visit_function_definition(ast, value, stmt),
      Stmt::FunctionDeclaration(value) => self.visit_function_declaration(ast, value, stmt),
      Stmt::ExternFunction(value) => self.visit_extern_function(ast, value, stmt),
      Stmt::Break => self.visit_break(ast, stmt),
      Stmt::Return(value) => self.visit_return(ast, value, stmt),
      Stmt::Struct(value) => self.visit_struct(ast, value, stmt),
      Stmt::Import(value) => self.visit_import(ast, value, stmt),
      Stmt::ModuleDecl(value) => self.visit_module_decl(ast, value, stmt),
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
