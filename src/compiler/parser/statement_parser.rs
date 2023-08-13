use crate::{
  check_error,
  compiler::{
    errors::{ty_err, SourceRangeProvider},
    identifier::{ExternId, StructId, VariableIdentifier},
  },
  return_if_err,
};

use super::*;

use crate::compiler::errors::sema_err;
use crate::compiler::types::FunctionSignature;
use ast::statement::*;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ReturnKind {
  None,
  Conditional,
  Unconditional,
}

impl ReturnKind {
  fn update(&mut self, other: Self) {
    // one is unconditional -> unconditional
    // one is conditional other is none -> conditional
    // both are conditional -> conditional
    // both are none -> none
    *self = match (self.clone(), other) {
      (ReturnKind::None, ReturnKind::None) => ReturnKind::None,
      (ReturnKind::Unconditional, _) | (_, ReturnKind::Unconditional) => ReturnKind::Unconditional,
      (ReturnKind::Conditional, _) | (_, ReturnKind::Conditional) => ReturnKind::Conditional,
    }
  }

  fn to_conditional(self) -> ReturnKind {
    if self != ReturnKind::None {
      ReturnKind::Conditional
    } else {
      self
    }
  }
}

pub struct ParsedStatement {
  pub handle: StmtHandle,
  pub return_kind: ReturnKind,
}

impl From<StmtHandle> for ParsedStatement {
  fn from(value: StmtHandle) -> Self {
    Self {
      handle: value,
      return_kind: ReturnKind::None,
    }
  }
}

impl ParsedStatement {
  const INVALID: ParsedStatement = ParsedStatement {
    handle: StmtHandle::INVALID,
    return_kind: ReturnKind::None,
  };
}

impl<'src> Parser<'src> {
  pub(super) fn parse_unscoped_block(&mut self) -> (Vec<StmtHandle>, ReturnKind) {
    return_if_err!(self, (vec![], ReturnKind::None));
    // FIXME: this is very similar to the one below
    self.match_or_err(Token::Basic('{'));
    let mut statements = Vec::new();
    let mut return_kind = ReturnKind::None;
    while self.lookahead != Token::Basic('}')
      && !self.is_at_end()
      && self.state != ParserState::UnrecoverableError
    {
      let stmt = self.parse_decl();
      return_kind.update(stmt.return_kind);
      statements.push(stmt.handle);
      if self.in_panic_state() {
        self.recover_from_errors();
      }
    }
    self.match_or_err(Token::Basic('}'));
    (statements, return_kind)
  }

  fn parse_block(&mut self) -> ParsedStatement {
    self.match_or_err(Token::Basic('{'));
    self.env.push_scope();
    let (statements, return_kind) = self.parse_unscoped_block();
    let locals = self.env.pop_scope();
    ParsedStatement {
      handle: self.ast.add_statement(stmt::Block { statements, locals }),
      return_kind: return_kind,
    }
  }

  fn parse_expr_stmt(&mut self) -> ParsedStatement {
    return_if_err!(self, ParsedStatement::INVALID);

    let expr = self.parse_expression();
    let handle = self.ast.add_statement(stmt::StmtExpr {
      expr: expr.handle,
      expr_type: expr.type_,
    });
    self.match_or_err(Token::Basic(';'));
    handle.into()
  }

  fn parse_if_stmt(&mut self) -> ParsedStatement {
    assert_eq!(self.lookahead, Token::If);
    let if_sr = self.lex.previous_token_range();
    self.advance();
    self.match_or_err(Token::Basic('('));
    let condition = self.parse_expression();
    if condition.type_ != Type::Bool {
      self.emit_error(ty_err::incorrect_conditional_type(
        if_sr,
        condition.type_.print_pretty(),
      ));
      return ParsedStatement::INVALID;
    }
    self.match_or_err(Token::Basic(')'));
    let true_branch = self.parse_statement();
    let (else_branch, mut return_kind) = if (self.match_next(Token::Else)).is_some() {
      let stmt = self.parse_statement();
      (Some(stmt.handle), stmt.return_kind)
    } else {
      (None, ReturnKind::None)
    };
    return_kind.update(true_branch.return_kind.to_conditional());
    let handle = self.ast.add_statement(stmt::IfBranch {
      if_sr,
      condition: condition.handle,
      true_branch: true_branch.handle,
      else_branch,
    });
    ParsedStatement {
      handle,
      return_kind,
    }
  }

  fn parse_while_stmt(&mut self) -> ParsedStatement {
    assert_eq!(self.lookahead, Token::While);
    let while_sr = self.lex.previous_token_range();
    self.advance(); // consume while
    self.loop_depth += 1;
    self.match_or_err(Token::Basic('('));
    let condition = self.parse_expression();
    if condition.type_ != Type::Bool {
      self.emit_error(ty_err::incorrect_conditional_type(
        while_sr,
        condition.type_.print_pretty(),
      ));
      return ParsedStatement::INVALID;
    }
    self.match_or_err(Token::Basic(')'));
    let loop_body = self.parse_statement();
    let handle = self.ast.add_statement(stmt::While {
      while_sr,
      condition: condition.handle,
      loop_body: loop_body.handle,
    });
    self.loop_depth -= 1;
    ParsedStatement {
      handle,
      return_kind: loop_body.return_kind.to_conditional(),
    }
  }

  fn parse_for_stmt(&mut self) -> ParsedStatement {
    assert_eq!(self.lookahead, Token::For);
    unimplemented!();
  }

  fn parse_loop_break(&mut self) -> ParsedStatement {
    assert!(matches!(self.lookahead, Token::Break));
    let sr = self.lex.previous_token_range();
    if self.loop_depth == 0 {
      self.emit_error(sema_err::break_outside_loop(sr));
      return ParsedStatement::INVALID;
    }
    self.advance();
    self.match_or_err(Token::Basic(';'));
    self.ast.add_statement(stmt::Break { sr }).into()
  }

  fn parse_function_return(&mut self) -> ParsedStatement {
    assert!(matches!(self.lookahead, Token::Return));

    let return_sr = self.lex.previous_token_range();
    self.advance();

    let (expr, type_) = if self.lookahead != Token::Basic(';') {
      let expr = self.parse_expression();
      (Some(expr.handle), expr.type_)
    } else {
      (None, Type::Nothing)
    };

    if let Some(current_function_return_type) = self.env.get_function_return_type() {
      if type_ != *current_function_return_type && *current_function_return_type != Type::Error {
        self.emit_error(ty_err::incorrect_return_type(
          return_sr,
          type_.print_pretty(),
          current_function_return_type.print_pretty(),
        ));
        return ParsedStatement::INVALID;
      }
    } else {
      self.emit_error(ty_err::return_outside_of_function(return_sr));
      return ParsedStatement::INVALID;
    }

    self.match_or_err(Token::Basic(';'));
    let handle = self.ast.add_statement(stmt::Return { expr, return_sr });
    ParsedStatement {
      handle,
      return_kind: ReturnKind::Unconditional,
    }
  }

  fn parse_statement(&mut self) -> ParsedStatement {
    return_if_err!(self, ParsedStatement::INVALID);
    match self.lookahead {
      Token::Basic('{') => self.parse_block(),
      Token::If => self.parse_if_stmt(),
      Token::While => self.parse_while_stmt(),
      Token::For => self.parse_for_stmt(),
      Token::Break => self.parse_loop_break(),
      Token::Return => self.parse_function_return(),
      _ => self.parse_expr_stmt(),
    }
  }

  fn parse_var_decl(&mut self) -> ParsedStatement {
    assert!(self.lookahead == Token::Var);
    return_if_err!(self, ParsedStatement::INVALID);

    self.advance();
    let (name, id_sr) = self.match_id_or_err();

    let type_specifier = self.parse_opt_type_specifier();
    self.match_or_err(Token::Basic('='));
    let expr = self.parse_expression();

    if let Some(specifier) = type_specifier {
      if specifier != expr.type_ {
        self.emit_error(ty_err::type_specifier_expression_mismatch(
          id_sr,
          specifier.print_pretty(),
          expr.type_.print_pretty(),
        ));
        return ParsedStatement::INVALID;
      }
    }

    let identifier = check_error!(
      self,
      self.env.define_variable(name, id_sr, expr.type_.clone()),
      VariableIdentifier::Invalid
    );

    self.match_or_err(Token::Basic(';'));
    self
      .ast
      .add_statement(stmt::VarDecl {
        identifier,
        var_type: expr.type_,
        id_sr,
        init_expr: expr.handle,
      })
      .into()
  }

  pub(super) fn parse_function_params(&mut self, call_start: SourceRange) -> Vec<Type> {
    return_if_err!(self, vec![]);
    let mut parameter_types = Vec::new();
    loop {
      let (name, name_sr) = self.match_id_or_err();
      let param_type = self.parse_type_specifier_or_err();
      check_error!(
        self,
        self.env.define_variable(name, name_sr, param_type.clone()),
        VariableIdentifier::Invalid
      );
      parameter_types.push(param_type);
      if self.match_next(Token::Basic(',')).is_none() {
        break;
      }
    }
    if parameter_types.len() > 255 {
      self.emit_error(parser_err::too_many_function_parameters(call_start));
      vec![]
    } else {
      parameter_types
    }
  }

  pub(super) fn parse_function_return_type(&mut self) -> Type {
    return_if_err!(self, Type::Error);
    if self.match_next(Token::ThinArrow).is_some() {
      self.match_type_name_or_err()
    } else {
      Type::Nothing
    }
  }

  fn parse_function_decl(&mut self) -> ParsedStatement {
    assert_eq!(self.lookahead, Token::Fn);

    self.advance(); // consume fun
    let (name, name_sr) = self.match_id_or_err();
    let call_start = self.lex.previous_token_range();
    self.env.push_function();
    self.match_or_err(Token::Basic('('));
    let parameter_types = if self.lookahead != Token::Basic(')') {
      self.parse_function_params(call_start)
    } else {
      Vec::new()
    };
    self.match_or_err(Token::Basic(')'));
    let return_type = self.parse_function_return_type();
    self.env.define_function_return_type(return_type.clone());
    let function_type = FunctionSignature::new(parameter_types.clone(), return_type.clone()).into();
    if self.lookahead == Token::Basic(';') {
      self.env.pop_function();
      self.advance();
      let id = check_error!(
        self,
        self
          .env
          .declare_global_function(name, name_sr, function_type),
        VariableIdentifier::Invalid
      );
      return self
        .ast
        .add_statement(stmt::FunctionDeclaration {
          id,
          name_sr,
          parameter_types,
          return_type,
        })
        .into();
    }
    let id = check_error!(
      self,
      self
        .env
        .define_global_function(name, name_sr, function_type),
      VariableIdentifier::Invalid
    );
    let body = self.parse_unscoped_block();
    let captures = self.env.pop_function();

    if matches!(body.1, ReturnKind::Conditional | ReturnKind::None if return_type != Type::Nothing)
    {
      self.emit_error(ty_err::no_unconditional_return(name_sr));
      return ParsedStatement::INVALID;
    }

    self
      .ast
      .add_statement(stmt::FunctionDefinition {
        id,
        name_sr,
        parameter_types,
        captures,
        body: body.0,
        return_type,
      })
      .into()
  }

  fn parse_struct_decl(&mut self) -> ParsedStatement {
    assert_eq!(self.lookahead, Token::Struct);
    self.advance();
    let (name, name_sr) = self.match_id_or_err();

    self.match_or_err(Token::Basic('{'));
    let mut member_names = Vec::new();
    let mut member_types = Vec::new();
    while self.lookahead != Token::Basic('}') {
      let name = self.id_str_or_err().to_string();
      let member_type = self.parse_type_specifier_or_err();
      member_names.push(name);
      member_types.push(member_type);
      self.match_or_err(Token::Basic(','));
    }
    self.match_or_err(Token::Basic('}'));
    let name_id = check_error!(
      self,
      self
        .env
        .define_struct(name, name_sr, member_names, member_types),
      StructId::relative(0)
    );
    self
      .ast
      .add_statement(stmt::Struct {
        id: name_id,
        name_sr,
      })
      .into()
  }

  fn parse_extern_function(&mut self) -> ParsedStatement {
    assert_eq!(self.lookahead, Token::Extern);
    if !self.env.in_global_scope() {
      let err = parser_err::extern_function_in_local_scope(&self.lex);
      self.emit_error(err);
      return ParsedStatement::INVALID;
    }
    self.advance();
    self.match_or_err(Token::Fn);
    let (name, name_sr) = self.match_id_or_err();
    let parameter_types = self.parse_function_param_types();
    let return_type = self.parse_function_return_type();
    let signature = FunctionSignature::new(parameter_types.clone(), return_type.clone());
    let identifier = check_error!(
      self,
      self.env.declare_extern_function(name, name_sr, signature),
      ExternId::relative(0)
    );
    self
      .ast
      .add_statement(stmt::ExternFunction {
        identifier,
        name_sr,
        parameter_types,
        return_type,
      })
      .into()
  }

  fn parse_import(&mut self) -> ParsedStatement {
    assert_eq!(self.lookahead, Token::Import);
    self.advance();
    if !self.env.in_global_scope() {
      let err = parser_err::import_in_local_scope(&self.lex);
      self.emit_error(err);
      return ParsedStatement::INVALID;
    }
    if let Token::Id(module_name) = self.lookahead {
      let module_name_sr = self.lex.current_range();
      self.advance();
      self.match_or_err(Token::Basic(';'));
      match self.env.import_module(module_name, module_name_sr) {
        Ok(module_id) => self.ast.add_statement(stmt::Import { module_id }).into(),
        Err(error) => {
          self.emit_error(error);
          ParsedStatement::INVALID
        }
      }
    } else {
      let err = parser_err::expected_module_identifier(&self.lex, self.lookahead);
      self.emit_error(err);
      ParsedStatement::INVALID
    }
  }

  fn module_err(&mut self) -> ParsedStatement {
    self.emit_error(parser_err::module_declarations_is_not_first_statement(
      &self.lex,
    ));
    ParsedStatement::INVALID
  }

  pub(super) fn parse_decl(&mut self) -> ParsedStatement {
    match self.lookahead {
      Token::Var => self.parse_var_decl(),
      Token::Fn => self.parse_function_decl(),
      Token::Struct => self.parse_struct_decl(),
      Token::Extern => self.parse_extern_function(),
      Token::Import => self.parse_import(),
      Token::Module => self.module_err(),
      _ => self.parse_statement(),
    }
  }
}

#[cfg(test)]
mod test {
  use json::{array, JsonValue};

  use crate::compiler::types::Type;
  use crate::compiler::{
    ast::{json::ASTJSONPrinter, visitor::StmtVisitor, AST},
    errors::CompilerError,
    global_env::GlobalEnv,
    lexer::{Lexer, Token},
    parser::{environment::Environment, Parser, ParserState},
  };

  fn parse_statement(expr: &str) -> Result<JsonValue, Vec<CompilerError>> {
    let mut empty_global_env = GlobalEnv::new();
    let mut parser = Parser {
      lex: Lexer::new(expr),
      lookahead: Token::EndOfFile,
      ast: AST::new(),
      env: Environment::new(&mut empty_global_env),
      errors: Vec::new(),
      state: ParserState::NoErrors,
      loop_depth: 0,
    };
    parser.advance();
    let stmt = parser.parse_decl();
    if !parser.errors.is_empty() {
      Err(parser.errors)
    } else {
      let mut printer = ASTJSONPrinter {};
      Ok(printer.visit_stmt(&parser.ast, stmt.handle))
    }
  }

  #[test]
  fn parse_function_declaration_with_parameter_names() {
    let function_decl =
      parse_statement("fn fwd_decl(a: num, b: str) -> num;").expect("parsing error");
    let function_decl = &function_decl["FunctionDeclaration"];
    assert_eq!(
      function_decl["parameter types"],
      array![&Type::Num, &Type::Str]
    );
    assert_eq!(Type::Num, function_decl["return type"],)
  }
}
