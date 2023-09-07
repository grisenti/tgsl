use crate::compiler::ast::statement::stmt::{
  Block, ExternFunction, FunctionDeclaration, FunctionDefinition, IfBranch, Import, ModuleDecl,
  Return, StmtExpr, Struct, VarDecl, While,
};
use crate::compiler::ast::visitor::{ExprVisitor, ParsedTypeVisitor, StmtVisitor};
use crate::compiler::ast::{StmtHandle, AST};
use crate::compiler::codegen::bytecode::OpCode;
use crate::compiler::errors::{ty_err, CompilerResult};
use crate::compiler::lexer::SourceRange;
use crate::compiler::semantics::SemanticChecker;
use crate::compiler::types::Type;

pub enum ReturnType {
  Conditional(Type),
  Unconditional(Type),
  None,
}

impl SemanticChecker<'_> {
  fn check_return_types(&mut self, t1: Type, t2: Type, sr: SourceRange) -> Type {
    todo!()
  }
  fn combine_return_types(&mut self, a: ReturnType, b: ReturnType, sr: SourceRange) -> ReturnType {
    match (a, b) {
      (ReturnType::None, other) | (other, ReturnType::None) => other,
      (ReturnType::Unconditional(unconditional), ReturnType::Conditional(conditional))
      | (ReturnType::Conditional(conditional), ReturnType::Unconditional(unconditional)) => {
        ReturnType::Unconditional(self.check_return_types(conditional, unconditional, sr))
      }
      (ReturnType::Conditional(t1), ReturnType::Conditional(t2)) => {
        ReturnType::Conditional(self.check_return_types(t1, t2, sr))
      }
      (ReturnType::Unconditional(t1), ReturnType::Unconditional(t2)) => {
        ReturnType::Unconditional(self.check_return_types(t1, t2, sr))
      }
    }
  }
}

impl<'a> StmtVisitor<'a, 'a, ReturnType> for SemanticChecker<'a> {
  fn visit_var_decl(
    &mut self,
    ast: &'a AST,
    var_decl: &'a VarDecl<'a>,
    expr_handle: StmtHandle,
  ) -> ReturnType {
    let var_type = self.visit_expr(ast, var_decl.init_expr);
    let specified_type = self.visit_parsed_type(ast, var_decl.specified_type);
    let stmt_sr = expr_handle.get_source_range(ast);
    if var_type.is_error() {
      return ReturnType::None;
    }
    if var_type != specified_type {
      self.emit_error(ty_err::type_specifier_expression_mismatch(
        stmt_sr,
        var_type.print_pretty(),
        specified_type.print_pretty(),
      ));
      return ReturnType::None;
    }
    let id = self.new_variable(var_decl.name, var_type, stmt_sr);
    unsafe { self.code().set_variable(id) };
    ReturnType::None
  }

  fn visit_stmt_expr(
    &mut self,
    ast: &'a AST,
    expr: &StmtExpr,
    expr_handle: StmtHandle,
  ) -> ReturnType {
    self.visit_expr(ast, expr.expr);
    ReturnType::None
  }

  fn visit_block(&mut self, ast: &'a AST, block: &Block, stmt_handle: StmtHandle) -> ReturnType {
    let mut return_type = ReturnType::None;
    let stmt_sr = stmt_handle.get_source_range(ast);
    self.env.push_scope();
    for stmt in &block.statements {
      let stmt_return_type = self.visit_stmt(ast, *stmt);
      return_type = self.combine_return_types(return_type, stmt_return_type, stmt_sr);
    }
    let locals = self.env.pop_scope();
    (0..=locals).for_each(|_| unsafe { self.code().push_op(OpCode::Pop) });
    return_type
  }

  fn visit_if_branch(
    &mut self,
    ast: &'a AST,
    if_branch: &IfBranch,
    expr_handle: StmtHandle,
  ) -> ReturnType {
    todo!()
  }

  fn visit_while(
    &mut self,
    ast: &'a AST,
    while_node: &While,
    expr_handle: StmtHandle,
  ) -> ReturnType {
    todo!()
  }

  fn visit_function_definition(
    &mut self,
    ast: &'a AST,
    function_definition: &FunctionDefinition,
    expr_handle: StmtHandle,
  ) -> ReturnType {
    todo!()
  }

  fn visit_function_declaration(
    &mut self,
    ast: &'a AST,
    function_declaration: &FunctionDeclaration,
    expr_handle: StmtHandle,
  ) -> ReturnType {
    todo!()
  }

  fn visit_extern_function(
    &mut self,
    ast: &'a AST,
    extern_function: &ExternFunction,
    expr_handle: StmtHandle,
  ) -> ReturnType {
    todo!()
  }

  fn visit_break(&mut self, ast: &'a AST, expr_handle: StmtHandle) -> ReturnType {
    todo!()
  }

  fn visit_return(&mut self, ast: &'a AST, return_stmt: &Return, _: StmtHandle) -> ReturnType {
    if let Some(expr) = return_stmt.expr {
      let return_type = self.visit_expr(ast, expr);
      ReturnType::Unconditional(return_type)
    } else {
      ReturnType::Unconditional(Type::Nothing)
    }
  }

  fn visit_struct(
    &mut self,
    ast: &'a AST,
    struct_stmt: &Struct,
    stmt_handle: StmtHandle,
  ) -> ReturnType {
    todo!()
  }

  fn visit_import(&mut self, ast: &'a AST, import: &Import, expr_handle: StmtHandle) -> ReturnType {
    todo!()
  }

  fn visit_module_decl(
    &mut self,
    ast: &'a AST,
    module_decl: &ModuleDecl,
    stmt_handle: StmtHandle,
  ) -> ReturnType {
    todo!()
  }
}
