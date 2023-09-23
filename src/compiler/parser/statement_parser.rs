use crate::return_if_err;

use super::*;
use ast::statement::*;

impl<'src> Parser<'src> {
  pub(super) fn parse_block_components(&mut self) -> (Vec<StmtHandle>, SourceRange) {
    return_if_err!(self, (vec![], SourceRange::EMPTY));

    let block_start = self.lex.previous_token_range();
    self.match_token(Token::Basic('{'));
    let mut statements = Vec::new();
    while self.lookahead != Token::Basic('}')
      && !self.is_at_end()
      && self.state != ParserState::UnrecoverableError
    {
      let stmt = self.parse_decl();
      statements.push(stmt);
      if self.in_panic_state() {
        self.recover_from_errors();
      }
    }
    let block_end = self.lex.previous_token_range();
    self.match_token(Token::Basic('}'));
    (statements, SourceRange::combine(block_start, block_end))
  }

  fn parse_block(&mut self) -> StmtHandle {
    let (statements, source_range) = self.parse_block_components();
    self
      .ast
      .add_statement(stmt::Block { statements }, source_range)
  }

  fn parse_expr_stmt(&mut self) -> StmtHandle {
    return_if_err!(self, StmtHandle::INVALID);

    let stmt_start = self.lex.previous_token_range();
    let expr = self.parse_expression();
    let stmt_end = self.lex.previous_token_range();
    self.match_token(Token::Basic(';'));
    self.ast.add_statement(
      stmt::StmtExpr { expr },
      SourceRange::combine(stmt_start, stmt_end),
    )
  }

  fn parse_if_stmt(&mut self) -> StmtHandle {
    assert_eq!(self.lookahead, Token::If);
    let stmt_start = self.lex.previous_token_range();
    self.advance();
    self.match_token(Token::Basic('('));
    let condition = self.parse_expression();
    self.match_token(Token::Basic(')'));
    let true_branch = self.parse_statement();
    let else_branch = if (self.match_next(Token::Else)).is_some() {
      Some(self.parse_statement())
    } else {
      None
    };
    let stmt_end = else_branch
      .map(|stmt| stmt.get_source_range(&self.ast))
      .unwrap_or(true_branch.get_source_range(&self.ast));
    self.ast.add_statement(
      stmt::IfBranch {
        condition,
        true_branch,
        else_branch,
      },
      SourceRange::combine(stmt_start, stmt_end),
    )
  }

  fn parse_while_stmt(&mut self) -> StmtHandle {
    assert_eq!(self.lookahead, Token::While);
    let stmt_start = self.lex.previous_token_range();
    self.advance();
    self.match_token(Token::Basic('('));
    let condition = self.parse_expression();
    self.match_token(Token::Basic(')'));
    let loop_body = self.parse_statement();
    let stmt_end = loop_body.get_source_range(&self.ast);
    self.ast.add_statement(
      stmt::While {
        condition,
        loop_body,
      },
      SourceRange::combine(stmt_start, stmt_end),
    )
  }

  fn parse_for_stmt(&mut self) -> StmtHandle {
    assert_eq!(self.lookahead, Token::For);
    unimplemented!();
  }

  fn parse_loop_break(&mut self) -> StmtHandle {
    assert!(matches!(self.lookahead, Token::Break));
    let stmt_start = self.lex.previous_token_range();
    self.advance();
    let stmt_end = self.lex.previous_token_range();
    self.match_token(Token::Basic(';'));
    self
      .ast
      .add_statement(Stmt::Break, SourceRange::combine(stmt_start, stmt_end))
  }

  fn parse_return(&mut self) -> StmtHandle {
    assert_eq!(self.lookahead, Token::Return);

    let stmt_start = self.lex.previous_token_range();
    self.advance();
    let expr = if self.lookahead != Token::Basic(';') {
      let expr = self.parse_expression();
      Some(expr)
    } else {
      None
    };
    let stmt_end = self.lex.previous_token_range();
    self.match_token(Token::Basic(';'));
    self.ast.add_statement(
      stmt::Return { expr },
      SourceRange::combine(stmt_start, stmt_end),
    )
  }

  fn parse_statement(&mut self) -> StmtHandle {
    return_if_err!(self, StmtHandle::INVALID);
    match self.lookahead {
      Token::Basic('{') => self.parse_block(),
      Token::If => self.parse_if_stmt(),
      Token::While => self.parse_while_stmt(),
      Token::For => self.parse_for_stmt(),
      Token::Break => self.parse_loop_break(),
      Token::Return => self.parse_return(),
      _ => self.parse_expr_stmt(),
    }
  }

  fn parse_var_decl(&mut self) -> StmtHandle {
    assert_eq!(self.lookahead, Token::Var);
    return_if_err!(self, StmtHandle::INVALID);

    let stmt_start = self.lex.previous_token_range();
    self.advance();
    let name = self.match_id();
    let name_sr = self.lex.previous_token_range();

    let specified_type = self
      .parse_opt_type_specifier()
      .unwrap_or(TypeHandle::NOTHING);
    if self.lookahead != Token::Basic('=') {
      self.emit_error(parser_err::missing_initialization_at_variable_declaration(
        name_sr,
      ));
      return StmtHandle::INVALID;
    }
    self.advance();
    let init_expr = self.parse_expression();

    let stmt_end = self.lex.previous_token_range();
    self.match_token(Token::Basic(';'));
    self.ast.add_statement(
      stmt::VarDecl {
        name,
        specified_type,
        init_expr,
      },
      SourceRange::combine(stmt_start, stmt_end),
    )
  }

  fn parse_function_decl(&mut self) -> StmtHandle {
    assert_eq!(self.lookahead, Token::Fn);

    let stmt_start = self.lex.previous_token_range();
    self.advance();

    let name = self.match_id();
    let (parameter_types, parameter_names) = self.parse_function_parameters();
    let return_type = self.parse_function_return_type();
    if self.lookahead == Token::Basic(';') {
      let stmt_end = self.lex.previous_token_range();
      self.advance();
      return self.ast.add_statement(
        stmt::FunctionDeclaration {
          name,
          parameter_names,
          parameter_types,
          return_type,
        },
        SourceRange::combine(stmt_start, stmt_end),
      );
    }

    let (body, body_sr) = self.parse_block_components();
    self.ast.add_statement(
      stmt::FunctionDefinition {
        name,
        parameter_names,
        parameter_types,
        body,
        return_type,
      },
      SourceRange::combine(stmt_start, body_sr),
    )
  }

  fn parse_member(
    &mut self,
    member_names: &mut Vec<&'src str>,
    member_types: &mut Vec<TypeHandle>,
  ) {
    let name = self.match_id();
    let member_type = self.parse_type_specifier();
    member_names.push(name);
    member_types.push(member_type);
  }

  fn parse_struct_decl(&mut self) -> StmtHandle {
    assert_eq!(self.lookahead, Token::Struct);

    let stmt_start = self.lex.previous_token_range();
    self.advance();
    let name = self.match_id();
    if self.lookahead == Token::Basic(';') {
      let stmt_end = self.lex.previous_token_range();
      self.advance();
      return self.ast.add_statement(
        stmt::StructDeclaration { name },
        SourceRange::combine(stmt_start, stmt_end),
      );
    }
    self.match_token(Token::Basic('{'));
    let mut member_names = Vec::new();
    let mut member_types = Vec::new();
    self.parse_member(&mut member_names, &mut member_types);
    while self.lookahead != Token::Basic('}') {
      self.match_token(Token::Basic(','));
      if self.lookahead != Token::Basic('}') {
        self.parse_member(&mut member_names, &mut member_types);
      }
    }
    let stmt_end = self.lex.previous_token_range();
    self.match_token(Token::Basic('}'));
    self.ast.add_statement(
      stmt::StructDefinition {
        name,
        member_names,
        member_types,
      },
      SourceRange::combine(stmt_start, stmt_end),
    )
  }

  fn parse_extern_function(&mut self) -> StmtHandle {
    assert_eq!(self.lookahead, Token::Extern);
    let stmt_start = self.lex.previous_token_range();

    self.advance();
    self.match_token(Token::Fn);
    let name = self.match_id();
    let (parameter_types, parameter_names) = self.parse_function_parameters();
    let return_type = self.parse_function_return_type();
    let stmt_end = self.lex.previous_token_range();
    self.match_token(Token::Basic(';'));
    self.ast.add_statement(
      stmt::ExternFunction {
        name,
        parameter_names,
        parameter_types,
        return_type,
      },
      SourceRange::combine(stmt_start, stmt_end),
    )
  }

  fn parse_import(&mut self) -> StmtHandle {
    assert_eq!(self.lookahead, Token::Import);
    let stmt_start = self.lex.previous_token_range();
    self.advance();

    if let Token::Id(module_name) = self.lookahead {
      self.advance();
      self.match_token(Token::Basic(';'));
      let stmt_end = self.lex.previous_token_range();
      self.ast.add_statement(
        stmt::Import { module_name },
        SourceRange::combine(stmt_start, stmt_end),
      )
    } else {
      let err = parser_err::expected_module_identifier(&self.lex, self.lookahead);
      self.emit_error(err);
      StmtHandle::INVALID
    }
  }

  fn module_err(&mut self) -> StmtHandle {
    self.emit_error(parser_err::module_declarations_is_not_first_statement(
      &self.lex,
    ));
    StmtHandle::INVALID
  }

  pub fn parse_decl(&mut self) -> StmtHandle {
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
  use json::{JsonValue};

  use crate::compiler::parser::test::TestParser;

  fn parse_correct_statement(stmt: &'static str) -> JsonValue {
    TestParser::new(stmt).parse_correct_statement()
  }

  #[test]
  fn parse_function_declaration_with_parameter_names() {
    let function_decl = parse_correct_statement("fn fwd_decl(a: num, b: str) -> num;");
    let function_decl = &function_decl["FunctionDeclaration"];
    assert_eq!(function_decl["name"], "fwd_decl");
    assert_eq!(
      function_decl["parameter_names"].len(),
      function_decl["parameter_types"].len()
    );
    assert_eq!(function_decl["return_type"], "num")
  }

  #[test]
  fn parse_struct_declaration() {
    let struct_def = parse_correct_statement("struct VeryImportantType;");
    assert_eq!(struct_def["StructDeclaration"]["name"], "VeryImportantType");
  }
}
