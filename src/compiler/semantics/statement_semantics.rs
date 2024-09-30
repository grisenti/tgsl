use std::rc::Rc;

use crate::compiler::ast::statement::{stmt, Stmt};

use crate::compiler::ast::parsed_type::ParsedType;
use crate::compiler::ast::{StmtHandle, TypeHandle, AST};

use crate::compiler::errors::{import_err, sema_err, ty_err};
use crate::compiler::ir::{
  Block, FunctionIr, IfBranch, Ir, IrStmtHandle, Return, StmtIr, VarDecl, VarIndex, While,
};
use crate::compiler::lexer::SourceRange;
use crate::compiler::semantics::environment::ImportError;
use crate::compiler::semantics::expression_semantics::CheckExprSemantics;
use crate::compiler::semantics::{
  check_statement_block, combine_returns, convert_parsed_type, convert_type_list, ReturnKind,
  SemanticState,
};
use crate::compiler::types::{FunctionSignature, Type};

pub struct CheckedStmt {
  pub return_kind: ReturnKind,
  pub handle: Option<IrStmtHandle>,
}
trait CheckType {
  fn check(&self, ast: &AST, state: &mut SemanticState) -> Type;
}

impl CheckType for TypeHandle {
  fn check(&self, ast: &AST, state: &mut SemanticState) -> Type {
    match self.get_type(ast) {
      ParsedType::Str => Type::Str,
      ParsedType::Num => Type::Num,
      ParsedType::Bool => Type::Bool,
      ParsedType::Nothing => Type::Nothing,
      _ => todo!(),
    }
  }
}

impl CheckedStmt {
  fn new<S: Into<StmtIr>>(
    stmt: S,
    source_range: SourceRange,
    return_kind: ReturnKind,
    ir: &mut Ir,
  ) -> Self {
    Self {
      return_kind,
      handle: Some(ir.add_statement(stmt.into(), source_range)),
    }
  }

  const ERROR: Self = Self {
    return_kind: ReturnKind::None,
    handle: Some(IrStmtHandle::INVALID),
  };

  const EMPTY: Self = Self {
    return_kind: ReturnKind::None,
    handle: None,
  };
}

pub trait CheckStmtSemantics {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedStmt;
}

impl CheckStmtSemantics for StmtHandle {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedStmt {
    state.push_source_range(self.get_source_range(ast));
    let checked_stmt = match self.get_stmt(ast) {
      Stmt::VarDecl(stmt) => stmt.check(ast, ir, state),
      Stmt::StmtExpr(stmt) => stmt.check(ast, ir, state),
      Stmt::Block(stmt) => stmt.check(ast, ir, state),
      Stmt::IfBranch(stmt) => stmt.check(ast, ir, state),
      Stmt::While(stmt) => stmt.check(ast, ir, state),
      Stmt::FunctionDefinition(stmt) => stmt.check(ast, ir, state),
      Stmt::FunctionDeclaration(stmt) => stmt.check(ast, ir, state),
      Stmt::ForeignFunction(stmt) => stmt.check(ast, ir, state),
      Stmt::Return(stmt) => stmt.check(ast, ir, state),
      Stmt::StructDeclaration(stmt) => stmt.check(ast, ir, state),
      Stmt::StructDefinition(stmt) => stmt.check(ast, ir, state),
      Stmt::Import(stmt) => stmt.check(ast, ir, state),
      Stmt::ModuleDecl(stmt) => stmt.check(ast, ir, state),
      Stmt::Break => todo!(),
    };
    state.pop_source_range();
    checked_stmt
  }
}

impl CheckStmtSemantics for stmt::VarDecl<'_> {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedStmt {
    let init = self.init_expr.check(ast, ir, state);

    // the error should have been reported already
    if init.type_.is_error() {
      return CheckedStmt::ERROR;
    }
    let specified_type = self.specified_type.check(ast, state);
    if specified_type != Type::Nothing && init.type_ != specified_type {
      state
        .errors
        .push(ty_err::type_specifier_expression_mismatch(
          state.top_source_range(),
          &init.type_,
          &specified_type,
        ));
      // we still declare it, so further uses will not generate an error
      declare_variable(self.name, Type::Error, state);
      return CheckedStmt::ERROR;
    }

    if init.type_ == Type::UnresolvedOverload {
      state
        .errors
        .push(ty_err::cannot_initialize_with_overloaded_function(
          state.top_source_range(),
        ));
      declare_variable(self.name, Type::Error, state);
      return CheckedStmt::ERROR;
    }

    let identifier = declare_variable(self.name, init.type_.clone(), state);
    CheckedStmt::new(
      VarDecl {
        identifier,
        type_: init.type_,
        init_expr: init.handle,
      },
      state.top_source_range(),
      ReturnKind::None,
      ir,
    )
  }
}

impl CheckStmtSemantics for stmt::StmtExpr {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedStmt {
    let expr = self.expr.check(ast, ir, state);
    CheckedStmt::new(expr.handle, state.top_source_range(), ReturnKind::None, ir)
  }
}

impl CheckStmtSemantics for stmt::Block {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedStmt {
    state.env.push_scope();
    let mut statements = Vec::new();
    let mut return_kind = ReturnKind::None;
    for stmt in self.statements.iter().map(|s| s.check(ast, ir, state)) {
      return_kind = combine_returns(return_kind, stmt.return_kind);
      if let Some(handle) = stmt.handle {
        statements.push(handle);
      }
    }
    let locals_count = state.env.pop_scope();
    CheckedStmt::new(
      Block {
        statements,
        locals_count,
      },
      state.top_source_range(),
      return_kind,
      ir,
    )
  }
}

impl CheckStmtSemantics for stmt::IfBranch {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedStmt {
    let condition = self.condition.check(ast, ir, state);
    if condition.type_ == Type::Error {
      return CheckedStmt::ERROR;
    }

    if condition.type_ != Type::Bool {
      todo!("condition is not of type bool")
    }

    let true_branch = self.true_branch.check(ast, ir, state);
    let else_branch = self.else_branch.map(|stmt| stmt.check(ast, ir, state));
    let else_branch_return_kind = else_branch
      .as_ref()
      .map_or(ReturnKind::None, |stmt| stmt.return_kind);
    let else_branch_handle = else_branch.as_ref().map(|stmt| stmt.handle);
    let return_kind = combine_returns(true_branch.return_kind, else_branch_return_kind);
    CheckedStmt::new(
      IfBranch {
        condition: condition.handle,
        true_branch: true_branch.handle.unwrap(),
        else_branch: else_branch_handle.unwrap(),
      },
      state.top_source_range(),
      return_kind,
      ir,
    )
  }
}

impl CheckStmtSemantics for stmt::While {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedStmt {
    let condition = self.condition.check(ast, ir, state);
    if condition.type_ == Type::Error {
      return CheckedStmt::ERROR;
    }

    if condition.type_ != Type::Bool {
      todo!("condition is not of type bool")
    }

    let loop_body = self.loop_body.check(ast, ir, state);
    CheckedStmt::new(
      While {
        condition: condition.handle,
        loop_body: loop_body.handle.unwrap(),
      },
      state.top_source_range(),
      loop_body.return_kind,
      ir,
    )
  }
}

impl CheckStmtSemantics for stmt::FunctionDefinition<'_> {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedStmt {
    let name = Rc::<str>::from(self.name);
    let parameter_types = convert_type_list(&self.parameter_types, ast, state);
    let return_type = convert_parsed_type(self.return_type, ast, state);

    let function_index = state.open_function(
      name.clone(),
      &self.parameter_names,
      &parameter_types,
      return_type.clone(),
    );

    let checked_body = check_statement_block(&self.body, ast, ir, state);

    state.close_function();

    ir.add_function(FunctionIr {
      name,
      index: function_index,
      signature: FunctionSignature::new(parameter_types, return_type),
      source_range: state.top_source_range(),
      instructions: checked_body.block_statements,
    });

    CheckedStmt::EMPTY
  }
}

impl CheckStmtSemantics for stmt::FunctionDeclaration<'_> {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedStmt {
    let parameter_types = convert_type_list(&self.parameter_types, ast, state);
    let return_type = convert_parsed_type(self.return_type, ast, state);
    let signature = FunctionSignature::new(parameter_types, return_type);
    state
      .env
      .declare_native_function(Rc::from(self.name), signature)
      .expect("TODO: error checking");
    CheckedStmt::EMPTY
  }
}

impl CheckStmtSemantics for stmt::ForeignFunction<'_> {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedStmt {
    let parameter_types = convert_type_list(&self.parameter_types, ast, state);
    let return_type = convert_parsed_type(self.return_type, ast, state);
    state
      .env
      .declare_foreign_function(
        Rc::from(self.name),
        FunctionSignature::new(parameter_types, return_type),
      )
      .expect("TODO: error checking");
    CheckedStmt::EMPTY
  }
}

impl CheckStmtSemantics for stmt::Return {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedStmt {
    let return_expr = self.expr.map(|e| e.check(ast, ir, state));
    let return_expr_type = return_expr.as_ref().map_or(&Type::Nothing, |e| &e.type_);

    // the error should have been reported already
    if return_expr_type.is_error() {
      return CheckedStmt::ERROR;
    }

    let function_return_type =
      if let Some(return_type) = state.env.get_current_function_return_type() {
        return_type
      } else {
        state.errors.push(sema_err::return_outside_of_function(
          state.top_source_range(),
        ));
        return CheckedStmt::ERROR;
      };

    assert!(!function_return_type.is_error());

    if return_expr_type != function_return_type {
      todo!()
    }
    CheckedStmt::new(
      Return {
        value: return_expr.map(|e| e.handle),
      },
      state.top_source_range(),
      ReturnKind::Unconditional,
      ir,
    )
  }
}

impl CheckStmtSemantics for stmt::StructDeclaration<'_> {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedStmt {
    state
      .env
      .declare_struct(Rc::from(self.name))
      .expect("TODO: error checking");
    CheckedStmt::EMPTY
  }
}

impl CheckStmtSemantics for stmt::StructDefinition<'_> {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedStmt {
    let member_names = self.member_names.iter().map(|s| s.to_string()).collect();
    let member_types = convert_type_list(&self.member_types, ast, state);
    state
      .env
      .define_struct(Rc::from(self.name), member_names, member_types)
      .expect("TODO: error checking");

    CheckedStmt::EMPTY
  }
}

impl CheckStmtSemantics for stmt::Import<'_> {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedStmt {
    match state.env.import_module(self.module_name) {
      Err(ImportError::NotAValidModule) => state.errors.push(import_err::not_a_loaded_module(
        state.top_source_range(),
        self.module_name,
      )),
      Err(ImportError::NameRedefinition(name)) => state.errors.push(
        import_err::name_redeclaration(state.top_source_range(), name),
      ),
      Err(ImportError::OverloadRedefinition(name)) => state.errors.push(
        import_err::overload_conflict(state.top_source_range(), name),
      ),
      Ok(()) => {}
    }
    CheckedStmt::EMPTY
  }
}

impl CheckStmtSemantics for stmt::ModuleDecl<'_> {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedStmt {
    assert!(state.module_name.is_none());
    if state.env.ensure_module_name_available(self.name) {
      state.module_name = Some(Rc::from(self.name));
    } else {
      state.errors.push(import_err::module_already_declared(
        state.top_source_range(),
        self.name,
      ))
    }
    CheckedStmt::EMPTY
  }
}

fn declare_variable(name: &str, type_: Type, state: &mut SemanticState) -> VarIndex {
  state
    .env
    .declare_variable(Rc::from(name), type_)
    .unwrap_or_else(|_| todo!())
}
