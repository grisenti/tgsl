use crate::{
  check_error,
  compiler::{
    errors::SourceRangeProvider,
    identifier::{ExternId, StructId, VariableIdentifier},
  },
  return_if_err,
};

use super::*;

use ast::statement::*;

impl<'src> Parser<'src> {
  pub(super) fn parse_unscoped_block(&mut self) -> Vec<StmtHandle> {
    return_if_err!(self, vec![]);
    // FIXME: this is very similar to the one below
    self.match_or_err(Token::Basic('{'));
    let mut statements = Vec::new();
    while self.lookahead != Token::Basic('}')
      && !self.is_at_end()
      && self.state != ParserState::UnrecoverableError
    {
      statements.push(self.parse_decl());
      if self.in_panic_state() {
        self.recover_from_errors();
      }
    }
    self.match_or_err(Token::Basic('}'));
    statements
  }

  fn parse_block(&mut self) -> StmtHandle {
    self.match_or_err(Token::Basic('{'));
    self.env.push_scope();
    let mut statements = Vec::new();
    while self.lookahead != Token::Basic('}')
      && !self.is_at_end()
      && self.state != ParserState::UnrecoverableError
    {
      statements.push(self.parse_decl());
      if self.in_panic_state() {
        self.recover_from_errors();
      }
    }
    let locals = self.env.pop_scope();
    self.match_or_err(Token::Basic('}'));
    self.ast.add_statement(stmt::Block { statements, locals })
  }

  fn parse_expr_stmt(&mut self) -> StmtHandle {
    let expr = self.parse_expression();
    let ret = self.ast.add_statement(stmt::StmtExpr {
      expr: expr.handle,
      expr_type: expr.type_id,
    });
    self.match_or_err(Token::Basic(';'));
    ret
  }

  fn parse_if_stmt(&mut self) -> StmtHandle {
    assert_eq!(self.lookahead, Token::If);
    let if_sr = self.lex.previous_token_range();
    self.advance();
    self.match_or_err(Token::Basic('('));
    let condition = self.parse_expression();
    self.match_or_err(Token::Basic(')'));
    let true_branch = self.parse_statement();
    let else_branch = if (self.match_next(Token::Else)).is_some() {
      Some(self.parse_statement())
    } else {
      None
    };
    self.ast.add_statement(stmt::IfBranch {
      if_sr,
      condition: condition.handle,
      true_branch,
      else_branch,
    })
  }

  fn parse_while_stmt(&mut self) -> StmtHandle {
    assert_eq!(self.lookahead, Token::While);
    let while_sr = self.lex.previous_token_range();
    self.advance(); // consume while
    self.match_or_err(Token::Basic('('));
    let condition = self.parse_expression();
    self.match_or_err(Token::Basic(')'));
    let loop_body = self.parse_statement();
    self.ast.add_statement(stmt::While {
      while_sr,
      condition: condition.handle,
      loop_body,
    })
  }

  fn parse_for_stmt(&mut self) -> StmtHandle {
    assert_eq!(self.lookahead, Token::For);
    let for_sr = self.lex.previous_token_range();
    self.advance(); //consume for
    self.match_or_err(Token::Basic('('));
    self.env.push_scope(); // for statement scope
    let init = self.parse_decl();
    let condition = self.parse_expression();
    self.match_or_err(Token::Basic(';'));
    let after = self.parse_expression();
    self.match_or_err(Token::Basic(')'));
    let body = self.parse_statement();
    let while_finally = self.ast.add_statement(stmt::StmtExpr {
      expr: after.handle,
      expr_type: after.type_id,
    });
    let while_body = self.ast.add_statement(stmt::Block {
      statements: vec![body, while_finally],
      locals: 0,
    });
    let while_loop = self.ast.add_statement(stmt::While {
      while_sr: for_sr,
      condition: condition.handle,
      loop_body: while_body,
    });
    let _locals = self.env.pop_scope(); // for statement scope
    self.ast.add_statement(stmt::Block {
      statements: vec![init, while_loop],
      locals: 0, // FIXME: maybe not 0
    })
  }

  fn parse_loop_break(&mut self) -> StmtHandle {
    assert!(matches!(self.lookahead, Token::Break));
    let sr = self.lex.previous_token_range();
    self.advance();
    self.match_or_err(Token::Basic(';'));
    self.ast.add_statement(stmt::Break { sr })
  }

  fn parse_function_return(&mut self) -> StmtHandle {
    assert!(matches!(self.lookahead, Token::Return));
    let return_sr = self.lex.previous_token_range();
    self.advance();
    let expr = if self.lookahead != Token::Basic(';') {
      Some(self.parse_expression())
    } else {
      None
    };
    self.match_or_err(Token::Basic(';'));
    self.ast.add_statement(stmt::Return {
      expr: expr.map(|expr| expr.handle),
      return_sr,
    })
  }

  fn parse_statement(&mut self) -> StmtHandle {
    return_if_err!(self, StmtHandle::INVALID);
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

  fn parse_var_decl(&mut self) -> StmtHandle {
    assert!(self.lookahead == Token::Var);
    return_if_err!(self, StmtHandle::INVALID);
    self.advance();
    let (name, id_sr) = self.match_id_or_err();
    let identifier = check_error!(
      self,
      self.env.define_variable(name, id_sr),
      VariableIdentifier::Invalid
    );
    let var_type = self.parse_opt_type_specifier();
    let ret = if self.lookahead == Token::Basic('=') {
      self.advance(); // consume '='
      stmt::VarDecl {
        identifier,
        id_sr,
        var_type,
        expression: Some(self.parse_expression().handle),
      }
    } else {
      stmt::VarDecl {
        identifier,
        var_type,
        id_sr,
        expression: None,
      }
    };
    self.match_or_err(Token::Basic(';'));
    self.ast.add_statement(ret)
  }

  pub(super) fn parse_function_params(&mut self, call_start: SourceRange) -> Vec<TypeId> {
    return_if_err!(self, vec![]);
    let mut parameter_types = Vec::new();
    loop {
      let (name, name_sr) = self.match_id_or_err();
      check_error!(
        self,
        self.env.define_variable(name, name_sr),
        VariableIdentifier::Invalid
      );
      let param_type = self.parse_type_specifier_or_err();
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

  pub(super) fn parse_function_return_type(&mut self) -> TypeId {
    return_if_err!(self, TypeId::ERROR);
    if self.match_next(Token::ThinArrow).is_some() {
      self.match_type_name_or_err()
    } else {
      TypeId::NOTHING
    }
  }

  fn parse_function_decl(&mut self) -> StmtHandle {
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
    let fn_type = self.type_map.get_or_add(Type::Function {
      parameters: parameter_types.clone(),
      ret: return_type,
    });
    if self.lookahead == Token::Basic(';') {
      self.env.pop_function();
      self.advance();
      let id = check_error!(
        self,
        self.env.declare_global_function(name, name_sr),
        VariableIdentifier::Invalid
      );
      return self.ast.add_statement(stmt::FunctionDeclaration {
        id,
        name_sr,
        parameter_types,
        return_type,
        fn_type,
      });
    }
    let id = check_error!(
      self,
      self.env.define_global_function(name, name_sr),
      VariableIdentifier::Invalid
    );
    let body = self.parse_unscoped_block();
    let captures = self.env.pop_function();

    self.ast.add_statement(stmt::FunctionDefinition {
      id,
      name_sr,
      fn_type,
      parameter_types,
      captures,
      body,
      return_type,
    })
  }

  fn parse_struct_decl(&mut self) -> StmtHandle {
    assert_eq!(self.lookahead, Token::Struct);
    self.advance();
    let (name, name_sr) = self.match_id_or_err();
    let name_id = check_error!(
      self,
      self.env.declare_struct(name, name_sr),
      StructId::relative(0)
    );
    let constructor_id = self
      .env
      .declare_global_function(&format!("constructor<{name}>"), SourceRange::EMPTY)
      .unwrap();
    self.match_or_err(Token::Basic('{'));
    let mut member_names = Vec::new();
    let mut member_types = Vec::new();
    while self.lookahead != Token::Basic('}') {
      let name = self.id_str_or_err();
      let member_type = self.parse_type_specifier_or_err();
      member_names.push(name);
      member_types.push(member_type);
      self.match_or_err(Token::Basic(','));
    }
    self.match_or_err(Token::Basic('}'));
    let (struct_type, constructor_type) = self
      .type_map
      .add_struct_type(name_id.into(), member_types.clone());
    self.ast.add_statement(stmt::Struct {
      id: name_id,
      name_sr,
      member_names,
      member_types,
      constructor_type,
      constructor_id,
      struct_type,
    })
  }

  fn parse_extern_function(&mut self) -> StmtHandle {
    assert_eq!(self.lookahead, Token::Extern);
    if !self.env.in_global_scope() {
      let err = parser_err::extern_function_in_local_scope(&self.lex);
      self.emit_error(err);
      return StmtHandle::INVALID;
    }
    self.advance();
    self.match_or_err(Token::Fn);
    let (name, name_sr) = self.match_id_or_err();
    let identifier = check_error!(
      self,
      self.env.declare_extern_function(name, name_sr),
      ExternId::relative(0)
    );
    let parameters = self.parse_function_param_types();
    let ret = self.parse_function_return_type();
    let fn_type = self.type_map.get_or_add(Type::Function { parameters, ret });
    self.ast.add_statement(stmt::ExternFunction {
      identifier,
      name_sr,
      fn_type,
    })
  }

  fn parse_import(&mut self) -> StmtHandle {
    assert_eq!(self.lookahead, Token::Import);
    self.advance();
    if !self.env.in_global_scope() {
      let err = parser_err::import_in_local_scope(&self.lex);
      self.emit_error(err);
      return StmtHandle::INVALID;
    }
    if let Token::Id(module_name) = self.lookahead {
      let module_name_sr = self.lex.current_range();
      self.advance();
      self.match_or_err(Token::Basic(';'));
      match self.env.import_module(module_name, module_name_sr) {
        Ok(module_id) => self.ast.add_statement(stmt::Import { module_id }),
        Err(error) => {
          self.emit_error(error);
          StmtHandle::INVALID
        }
      }
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

  pub(super) fn parse_decl(&mut self) -> StmtHandle {
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

#[cfg(disable)]
mod test {
  use json::{array, JsonValue};

  use crate::compiler::{
    ast::{json::ASTJSONPrinter, visitor::StmtVisitor, AST},
    errors::CompilerError,
    global_env::GlobalEnv,
    lexer::{Lexer, Token},
    parser::{environment::Environment, Parser, ParserState},
    types::{type_map::TypeMap, TypeId},
  };

  fn parse_statement(expr: &str) -> Result<JsonValue, Vec<CompilerError>> {
    let mut empty_type_map = TypeMap::new();
    let mut empty_global_env = GlobalEnv::new();
    let mut parser = Parser {
      lex: Lexer::new(expr),
      type_map: &mut empty_type_map,
      lookahead: Token::EndOfFile,
      ast: AST::new(),
      env: Environment::new(&mut empty_global_env),
      errors: Vec::new(),
      state: ParserState::NoErrors,
    };
    parser.advance();
    let handle = parser.parse_decl();
    if !parser.errors.is_empty() {
      Err(parser.errors)
    } else {
      let mut printer = ASTJSONPrinter {};
      Ok(printer.visit_stmt(&parser.ast, handle))
    }
  }

  //#[test]
  fn parse_function_declaration_with_parameter_names() {
    let function_decl =
      parse_statement("fn fwd_decl(a: num, b: str) -> num;").expect("parsing error");
    assert_eq!(function_decl["FunctionDeclaration"]["id"], "Global(0)");
    assert_eq!(
      function_decl["FunctionDeclaration"]["parameter types"],
      array![TypeId::NUM, TypeId::STR]
    );
    assert_eq!(
      function_decl["FunctionDeclaration"]["return type"],
      format!("{}", TypeId::NUM.0)
    )
  }
}
