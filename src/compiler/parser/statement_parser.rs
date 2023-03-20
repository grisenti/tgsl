use super::*;

impl<'src> Parser<'src> {
  fn parse_print_stmt(&mut self) -> StmtRes {
    assert_eq!(self.lookahead, Token::Print);
    self.advance()?;
    let expr = self.parse_expression()?;
    let ret = Ok(self.ast.add_statement(Stmt::Print(expr)));
    self.match_or_err(Token::Basic(';'))?;
    ret
  }

  pub(super) fn parse_unscoped_block(&mut self) -> Result<Vec<StmtHandle>, SourceError> {
    // FIXME: this is very similar to the one below
    self.match_or_err(Token::Basic('{'))?;
    let mut statements = Vec::new();
    let mut errors = Vec::new();
    while self.lookahead != Token::Basic('}') && !self.is_at_end() {
      match self.parse_decl() {
        Ok(stmt) => statements.push(stmt),
        Err(err) => {
          errors.push(err);
          errors = self.syncronize_or_errors(errors)?;
        }
      }
    }
    self.match_or_err(Token::Basic('}'))?;
    if errors.is_empty() {
      Ok(statements)
    } else {
      Err(SourceError::from_err_vec(errors))
    }
  }

  fn parse_block(&mut self) -> StmtRes {
    self.match_or_err(Token::Basic('{'))?;
    self.env.push_scope();
    let mut statements = Vec::new();
    let mut errors = Vec::new();
    while self.lookahead != Token::Basic('}') && !self.is_at_end() {
      match self.parse_decl() {
        Ok(stmt) => statements.push(stmt),
        Err(err) => {
          errors.push(err);
          errors = self.syncronize_or_errors(errors)?;
        }
      }
    }
    let locals = self.env.pop_scope();
    self.match_or_err(Token::Basic('}'))?;
    if errors.is_empty() {
      Ok(self.ast.add_statement(Stmt::Block { statements, locals }))
    } else {
      Err(SourceError::from_err_vec(errors))
    }
  }

  fn parse_expr_stmt(&mut self) -> StmtRes {
    let expr = self.parse_expression()?;
    let ret = self.ast.add_statement(Stmt::Expr(expr));
    self.match_or_err(Token::Basic(';'))?;
    Ok(ret)
  }

  fn parse_if_stmt(&mut self) -> StmtRes {
    assert_eq!(self.lookahead, Token::If);
    let if_info = self.last_token_info();
    self.advance()?;
    self.match_or_err(Token::Basic('('))?;
    let condition = self.parse_expression()?;
    self.match_or_err(Token::Basic(')'))?;
    let true_branch = self.parse_statement()?;
    let else_branch = if (self.match_next(Token::Else)?).is_some() {
      Some(self.parse_statement()?)
    } else {
      None
    };
    Ok(self.ast.add_statement(Stmt::IfBranch {
      if_info,
      condition,
      true_branch,
      else_branch,
    }))
  }

  fn parse_while_stmt(&mut self) -> StmtRes {
    assert_eq!(self.lookahead, Token::While);
    let info = self.last_token_info();
    self.advance()?; // consume while
    self.match_or_err(Token::Basic('('))?;
    let condition = self.parse_expression()?;
    self.match_or_err(Token::Basic(')'))?;
    let loop_body = self.parse_statement()?;
    Ok(self.ast.add_statement(Stmt::While {
      info,
      condition,
      loop_body,
    }))
  }

  fn parse_for_stmt(&mut self) -> StmtRes {
    assert_eq!(self.lookahead, Token::For);
    let info = self.last_token_info();
    self.advance()?; //consume for
    self.match_or_err(Token::Basic('('))?;
    self.env.push_scope(); // for statement scope
    let init = self.parse_decl()?;
    let condition = self.parse_expression()?;
    self.match_or_err(Token::Basic(';'))?;
    let after = self.parse_expression()?;
    self.match_or_err(Token::Basic(')'))?;
    let body = self.parse_statement()?;
    let while_finally = self.ast.add_statement(Stmt::Expr(after));
    let while_body = self.ast.add_statement(Stmt::Block {
      statements: vec![body, while_finally],
      locals: 0,
    });
    let while_loop = self.ast.add_statement(Stmt::While {
      info,
      condition,
      loop_body: while_body,
    });
    let locals = self.env.pop_scope(); // for statement scope
    Ok(self.ast.add_statement(Stmt::Block {
      statements: vec![init, while_loop],
      locals: 0, // FIXME: maybe not 0
    }))
  }

  fn parse_loop_break(&mut self) -> StmtRes {
    assert!(matches!(self.lookahead, Token::Break));
    let info = self.last_token_info();
    self.advance()?;
    self.match_or_err(Token::Basic(';'))?;
    Ok(self.ast.add_statement(Stmt::Break(info)))
  }

  fn parse_function_return(&mut self) -> StmtRes {
    assert!(matches!(self.lookahead, Token::Return));
    let src_info = self.last_token_info();
    self.advance()?;
    let expr = if self.lookahead != Token::Basic(';') {
      Some(self.parse_expression()?)
    } else {
      None
    };
    self.match_or_err(Token::Basic(';'))?;
    Ok(self.ast.add_statement(Stmt::Return { expr, src_info }))
  }

  fn parse_statement(&mut self) -> StmtRes {
    match self.lookahead {
      Token::Print => self.parse_print_stmt(),
      Token::Basic('{') => self.parse_block(),
      Token::If => self.parse_if_stmt(),
      Token::While => self.parse_while_stmt(),
      Token::For => self.parse_for_stmt(),
      Token::Break => self.parse_loop_break(),
      Token::Return => self.parse_function_return(),
      _ => self.parse_expr_stmt(),
    }
  }

  fn parse_var_decl(&mut self) -> StmtRes {
    assert!(self.lookahead == Token::Var);
    self.advance()?;
    let (identifier, id_info) = self.match_id_or_err()?;
    let var_type = self.parse_opt_type_specifier()?;
    self.env.set_type_if_global(identifier, var_type);
    let ret = if self.lookahead == Token::Basic('=') {
      self.advance()?; // consume '='
      Stmt::VarDecl {
        identifier,
        id_info,
        var_type,
        expression: Some(self.parse_expression()?),
      }
    } else {
      Stmt::VarDecl {
        identifier,
        var_type,
        id_info,
        expression: None,
      }
    };
    self.match_or_err(Token::Basic(';'))?;
    Ok(self.ast.add_statement(ret))
  }

  pub(super) fn parse_function_params(
    &mut self,
    call_start: SourceInfo,
  ) -> Result<Vec<TypeId>, SourceError> {
    let mut parameter_types = Vec::new();
    loop {
      self.match_id_or_err()?;
      let mem_type = self.parse_type_specifier_or_err()?;
      parameter_types.push(mem_type.clone());
      if self.matches_alternatives(&[Token::Basic(',')])?.is_none() {
        break;
      }
    }
    if parameter_types.len() > 255 {
      Err(error_from_source_info(
        &call_start,
        "function cannot have more than 255 parameters".to_string(),
      ))
    } else {
      Ok(parameter_types)
    }
  }

  pub(super) fn parse_function_return_type(&mut self) -> Result<TypeId, SourceError> {
    if self.match_next(Token::ThinArrow)?.is_some() {
      self.match_type_name_or_err()
    } else {
      Ok(TypeId::NOTHING)
    }
  }

  fn parse_function_decl(&mut self) -> StmtRes {
    assert_eq!(self.lookahead, Token::Fn);
    self.advance()?; // consume fun
    let (name_id, name_info) = self.match_id_or_err()?;
    let call_start = self.lex.prev_token_info();
    self.env.push_function();
    self.match_or_err(Token::Basic('('))?;
    let parameters = if self.lookahead != Token::Basic(')') {
      self.parse_function_params(call_start)
    } else {
      Ok(Vec::new())
    };
    self.match_or_err(Token::Basic(')'))?;
    let return_type = self.parse_function_return_type()?;
    let body = self.parse_unscoped_block()?;
    let captures = self.env.pop_function();
    let param_types = parameters?;
    let fn_type = self.type_map.get_or_add(Type::Function {
      parameters: param_types.clone(),
      ret: return_type,
    });
    self.env.set_type_if_global(name_id, fn_type);
    Ok(self.ast.add_statement(Stmt::Function {
      id: name_id,
      name_info,
      fn_type,
      parameters: param_types,
      captures,
      body,
      return_type,
    }))
  }

  fn parse_struct_decl(&mut self) -> StmtRes {
    assert_eq!(self.lookahead, Token::Struct);
    self.advance()?;
    let (name_id, name_info) = self.match_id_or_err()?;
    self.match_or_err(Token::Basic('{'))?;
    let mut member_names = Vec::new();
    let mut member_types = Vec::new();
    while self.lookahead != Token::Basic('}') {
      let name = self.id_str_or_err()?;
      let member_type = self.parse_type_specifier_or_err()?;
      member_names.push(name);
      member_types.push(member_type);
      self.match_or_err(Token::Basic(','))?;
    }
    self.match_or_err(Token::Basic('}'))?;
    let (struct_type, constructor_type) =
      self.type_map.add_struct_type(name_id, member_types.clone());
    self.env.set_type_if_global(name_id, constructor_type);
    Ok(self.ast.add_statement(Stmt::Struct {
      name: name_id,
      name_info,
      member_names,
      member_types,
      constructor_type,
      struct_type,
    }))
  }

  fn parse_extern_function(&mut self) -> StmtRes {
    assert_eq!(self.lookahead, Token::Extern);
    if !self.env.in_global_scope() {
      return Err(error_from_lexer_state(
        &self.lex,
        "extern functions can only be declared in the global scope".to_string(),
      ));
    }
    self.advance()?;
    self.match_or_err(Token::Fn)?;
    let (name_id, name_info) = self.match_id_or_err()?;
    let extern_id = self.env.create_extern_id(name_id);
    let parameters = self.parse_function_param_types()?;
    let ret = self.parse_function_return_type()?;
    let fn_type = self.type_map.get_or_add(Type::Function { parameters, ret });
    self.env.set_type_if_global(name_id, fn_type);
    Ok(self.ast.add_statement(Stmt::ExternFunction {
      name_id,
      name_info,
      fn_type,
      extern_id,
    }))
  }

  pub(super) fn parse_decl(&mut self) -> StmtRes {
    let ret = match self.lookahead {
      Token::Var => self.parse_var_decl()?,
      Token::Fn => self.parse_function_decl()?,
      Token::Struct => self.parse_struct_decl()?,
      Token::Extern => self.parse_extern_function()?,
      _ => self.parse_statement()?,
    };
    Ok(ret)
  }
}
