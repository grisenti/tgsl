use std::rc::Rc;

use crate::compiler::ast::statement::stmt::{
  Block, ExternFunction, FunctionDeclaration, FunctionDefinition, IfBranch, Import, ModuleDecl,
  Return, StmtExpr, StructDeclaration, StructDefinition, VarDecl, While,
};
use crate::compiler::ast::visitor::{ExprVisitor, ParsedTypeVisitor, StmtVisitor};
use crate::compiler::ast::{StmtHandle, AST};
use crate::compiler::codegen::bytecode::{ConstantValue, OpCode};
use crate::compiler::codegen::function_code::FunctionCode;
use crate::compiler::errors::{import_err, sema_err, ty_err};
use crate::compiler::functions::RelativeFunctionAddress;
use crate::compiler::lexer::SourceRange;
use crate::compiler::semantics::environment::imports::ImportError;
use crate::compiler::semantics::{combine_returns, ReturnKind, SemanticChecker};
use crate::compiler::structs::StructInsertError;
use crate::compiler::types::{parameter_types_to_string, FunctionSignature, Type};

impl<'a> StmtVisitor<'a, 'a, ReturnKind> for SemanticChecker<'a> {
  fn visit_var_decl(
    &mut self,
    ast: &'a AST,
    var_decl: &'a VarDecl<'a>,
    stmt_handle: StmtHandle,
  ) -> ReturnKind {
    let var_type = self.visit_expr(ast, var_decl.init_expr);
    if var_type.is_error() {
      return ReturnKind::None;
    }
    let specified_type = self.visit_parsed_type(ast, var_decl.specified_type);
    let stmt_sr = stmt_handle.get_source_range(ast);

    if specified_type != Type::Nothing && var_type != specified_type {
      self.emit_error(ty_err::type_specifier_expression_mismatch(
        stmt_sr,
        var_type.print_pretty(),
        specified_type.print_pretty(),
      ));
      return ReturnKind::None;
    }

    if var_type == Type::UnresolvedOverload {
      self.emit_error(ty_err::cannot_initialize_with_overloaded_function(stmt_sr));
      self.declare_initialized_variable(var_decl.name, Type::Error, stmt_sr);
      return ReturnKind::None;
    }

    self.declare_initialized_variable(var_decl.name, var_type, stmt_sr);
    ReturnKind::None
  }

  fn visit_stmt_expr(&mut self, ast: &'a AST, expr: &StmtExpr, _: StmtHandle) -> ReturnKind {
    self.visit_expr(ast, expr.expr);
    unsafe { self.code().push_op(OpCode::Pop) };
    ReturnKind::None
  }

  fn visit_block(&mut self, _ast: &'a AST, block: &Block, _stmt_handle: StmtHandle) -> ReturnKind {
    self.env.push_scope();
    let return_type = self.visit_statements(&block.statements);
    let locals = self.env.pop_scope();
    (0..locals).for_each(|_| unsafe { self.code().push_op(OpCode::Pop) });
    return_type
  }

  fn visit_if_branch(
    &mut self,
    ast: &'a AST,
    if_branch: &IfBranch,
    stmt_handle: StmtHandle,
  ) -> ReturnKind {
    let condition_type = self.visit_expr(ast, if_branch.condition);
    if !self.check_condition(condition_type, stmt_handle.get_source_range(ast)) {
      return ReturnKind::Unconditional;
    }
    let if_jump_point = unsafe { self.code().push_jump(OpCode::JumpIfFalsePop) };
    let if_branch_return_kind = self.visit_stmt(ast, if_branch.true_branch).to_conditional();
    let else_branch_return_kind = if let Some(stmt) = if_branch.else_branch {
      let skip_else = unsafe { self.code().push_jump(OpCode::Jump) };
      self.code().backpatch_current_instruction(if_jump_point);
      let return_kind = self.visit_stmt(ast, stmt);
      self.code().backpatch_current_instruction(skip_else);
      return_kind
    } else {
      self.code().backpatch_current_instruction(if_jump_point);
      ReturnKind::None
    };
    combine_returns(if_branch_return_kind, else_branch_return_kind)
  }

  fn visit_while(
    &mut self,
    ast: &'a AST,
    while_node: &While,
    stmt_handle: StmtHandle,
  ) -> ReturnKind {
    let label = self.code().get_next_instruction_label();
    let condition_type = self.visit_expr(ast, while_node.condition);
    if !self.check_condition(condition_type, stmt_handle.get_source_range(ast)) {
      return ReturnKind::Unconditional;
    }
    let loop_condition = unsafe { self.code().push_jump(OpCode::JumpIfFalsePop) };
    let return_kind = self.visit_stmt(ast, while_node.loop_body).to_conditional();
    self.code().push_back_jump(label);
    self.code().backpatch_current_instruction(loop_condition);
    return_kind
  }

  fn visit_function_definition(
    &mut self,
    ast: &'a AST,
    function_definition: &FunctionDefinition<'a>,
    stmt_handle: StmtHandle,
  ) -> ReturnKind {
    let parameter_types = self.convert_type_list(&function_definition.parameter_types);
    let return_type = self.visit_parsed_type(ast, function_definition.return_type);
    let function_signature = FunctionSignature::new(parameter_types.clone(), return_type);
    let stmt_sr = stmt_handle.get_source_range(ast);

    let function_id = self.start_function(function_definition.name, function_signature, stmt_sr);
    self.declare_function_parameters(
      &function_definition.parameter_names,
      &parameter_types,
      stmt_sr,
    );
    self.visit_function_body(&function_definition.body, stmt_sr);
    self.end_function(function_id);

    ReturnKind::None
  }

  fn visit_function_declaration(
    &mut self,
    ast: &'a AST,
    function_declaration: &FunctionDeclaration,
    stmt_handle: StmtHandle,
  ) -> ReturnKind {
    let parameter_types = self.convert_type_list(&function_declaration.parameter_types);
    let return_type = self.visit_parsed_type(ast, function_declaration.return_type);
    let stmt_sr = stmt_handle.get_source_range(ast);

    self.declare_function(
      function_declaration.name,
      FunctionSignature::new(parameter_types, return_type).into(),
      stmt_sr,
    );

    ReturnKind::None
  }

  fn visit_extern_function(
    &mut self,
    ast: &'a AST,
    extern_function: &ExternFunction,
    _: StmtHandle,
  ) -> ReturnKind {
    let parameter_types = self.convert_type_list(&extern_function.parameter_types);
    let return_type = self.visit_parsed_type(ast, extern_function.return_type);

    self
      .env
      .declare_extern_function(
        extern_function.name,
        FunctionSignature::new(parameter_types, return_type).into(),
      )
      .expect("error declaring extern function");

    ReturnKind::None
  }

  fn visit_break(&mut self, _ast: &'a AST, _: StmtHandle) -> ReturnKind {
    todo!()
  }

  fn visit_return(
    &mut self,
    ast: &'a AST,
    return_stmt: &Return,
    stmt_handle: StmtHandle,
  ) -> ReturnKind {
    let return_type = if let Some(expr) = return_stmt.expr {
      self.visit_expr(ast, expr)
    } else {
      Type::Nothing
    };
    if let Some(required_return_type) = self.env.get_current_function_return_type() {
      if !required_return_type.is_error()
        && !return_type.is_error()
        && *required_return_type != return_type
      {
        self.emit_error(ty_err::incorrect_return_type(
          stmt_handle.get_source_range(ast),
          return_type.print_pretty(),
          required_return_type.print_pretty(),
        ));
      }
      unsafe { self.code().push_op(OpCode::Return) };
      ReturnKind::Unconditional
    } else {
      self.emit_error(sema_err::return_outside_of_function(
        stmt_handle.get_source_range(ast),
      ));
      ReturnKind::None
    }
  }

  fn visit_struct_declaration(
    &mut self,
    ast: &AST,
    struct_decl: &StructDeclaration,
    stmt_handle: StmtHandle,
  ) -> ReturnKind {
    if let Err(err) = self.env.declare_struct(struct_decl.name) {
      self.handle_struct_decl_error(err, struct_decl.name, stmt_handle.get_source_range(ast))
    }
    ReturnKind::None
  }

  fn visit_struct_definition(
    &mut self,
    ast: &'a AST,
    struct_def: &StructDefinition,
    stmt_handle: StmtHandle,
  ) -> ReturnKind {
    let member_names = struct_def
      .member_names
      .iter()
      .map(|s| s.to_string())
      .collect();
    let member_types = self.convert_type_list(&struct_def.member_types);
    if let Err(err) = self
      .env
      .define_struct(struct_def.name, member_names, member_types)
    {
      self.handle_struct_decl_error(err, struct_def.name, stmt_handle.get_source_range(ast))
    }

    ReturnKind::None
  }

  fn visit_import(&mut self, ast: &'a AST, import: &Import, stmt_handle: StmtHandle) -> ReturnKind {
    match self.env.import_module(import.module_name) {
      Err(ImportError::NotAValidModule) => self.errors.push(import_err::not_a_loaded_module(
        stmt_handle.get_source_range(ast),
        import.module_name,
      )),
      Err(ImportError::NameRedefinition(name)) => self.errors.push(import_err::name_redeclaration(
        stmt_handle.get_source_range(ast),
        name,
      )),
      Err(ImportError::OverloadRedefinition(name)) => self.errors.push(
        import_err::overload_conflict(stmt_handle.get_source_range(ast), name),
      ),
      Ok(()) => {}
    }
    ReturnKind::None
  }

  fn visit_module_decl(
    &mut self,
    ast: &'a AST,
    module_decl: &ModuleDecl,
    stmt_handle: StmtHandle,
  ) -> ReturnKind {
    assert!(self.module_name.is_none());
    if self.env.ensure_module_name_available(module_decl.name) {
      self.module_name = Some(Rc::from(module_decl.name));
    } else {
      self.emit_error(import_err::module_already_declared(
        stmt_handle.get_source_range(ast),
        module_decl.name,
      ))
    }
    ReturnKind::None
  }
}

impl<'a> SemanticChecker<'a> {
  fn declare_function(&mut self, name: &str, signature: FunctionSignature, stmt_sr: SourceRange) {
    match self.env.declare_native_function(name, signature) {
      Ok(relative_address) => {
        assert!(relative_address as usize <= self.checked_functions.len());
        if relative_address as usize == self.checked_functions.len() {
          self.checked_functions.push(FunctionCode::default());
        }
      }
      Err(_) => todo!(),
    }
  }

  fn start_function(
    &mut self,
    name: &str,
    signature: FunctionSignature,
    stmt_sr: SourceRange,
  ) -> RelativeFunctionAddress {
    let function_name = format!(
      "{name}({})",
      parameter_types_to_string(signature.get_parameters())
    );
    self
      .env
      .push_function(function_name, signature.get_return_type().clone());
    self.checked_functions.push(FunctionCode::default());
    match self.env.define_native_function(name, signature) {
      Ok(relative_address) => relative_address,
      Err(_) => panic!(),
    }
  }

  fn end_function(&mut self, function_id: RelativeFunctionAddress) {
    let function_index = function_id as usize;
    assert!(function_index < self.checked_functions.len());

    self.finalize_function_code();
    let function = self.env.pop_function();
    self.checked_functions[function_index] = function.code;
  }

  fn handle_struct_decl_error(
    &mut self,
    error: StructInsertError,
    struct_name: &str,
    sr: SourceRange,
  ) {
    match error {
      StructInsertError::AlreadyDefined => {
        self.emit_error(sema_err::name_already_defined(sr, struct_name));
      }
      StructInsertError::AlreadyDeclared => todo!(),
      StructInsertError::Imported => todo!(),
      StructInsertError::TooManyStructs => todo!(),
      StructInsertError::NameIsAlreadyAFunction => todo!(),
      StructInsertError::NameIsAlreadyAVariable => todo!(),
    }
  }

  fn declare_initialized_variable(&mut self, name: &'a str, var_type: Type, stmt_sr: SourceRange) {
    if self.env.in_global_scope() {
      match self.env.declare_global_var(name, var_type) {
        Ok(global_id) => {
          unsafe {
            self
              .code()
              .push_constant(ConstantValue::RelativeNativeGlobalVar(global_id));
            self.code().push_op(OpCode::SetGlobal);
            self.code().push_op(OpCode::Pop);
          };
        }
        Err(err) => {
          todo!()
        }
      }
    } else {
      // we don't have to set it, we just need to put it on the stack and not pop it
      self.declare_local_var(name, var_type, stmt_sr);
    };
  }
}
