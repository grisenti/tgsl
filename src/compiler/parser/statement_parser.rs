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

  pub(super) fn parse_block(&mut self) -> StmtRes {
    self.match_or_err(Token::Basic('{'))?;
    self.env.push();
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
    self.env.pop();
    self.match_or_err(Token::Basic('}'))?;
    if errors.is_empty() {
      Ok(self.ast.add_statement(Stmt::Block(statements)))
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
    self.env.push(); // for statement scope
    let init = self.parse_decl()?;
    let condition = self.parse_expression()?;
    self.match_or_err(Token::Basic(';'))?;
    let after = self.parse_expression()?;
    self.match_or_err(Token::Basic(')'))?;
    let body = self.parse_statement()?;
    let while_finally = self.ast.add_statement(Stmt::Expr(after));
    let while_body = self
      .ast
      .add_statement(Stmt::Block(vec![body, while_finally]));
    let while_loop = self.ast.add_statement(Stmt::While {
      info,
      condition,
      loop_body: while_body,
    });
    self.env.pop(); // for statement scope
    Ok(self.ast.add_statement(Stmt::Block(vec![init, while_loop])))
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
    let expr = self.parse_expression()?;
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
    let var_type = self.parse_type_specifier_or_err()?;
    self.env.set_type(identifier, var_type.clone());
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
  ) -> Result<(Vec<Identifier>, Vec<Type>), SourceError> {
    let mut parameter_ids = Vec::new();
    let mut parameter_types = Vec::new();
    loop {
      let mem_id = self.match_id_or_err()?.0;
      let mem_type = self.parse_type_specifier_or_err()?;
      parameter_ids.push(mem_id);
      parameter_types.push(mem_type.clone());
      self.env.set_type(mem_id, mem_type);
      if self.matches_alternatives(&[Token::Basic(',')])?.is_none() {
        break;
      }
    }
    if parameter_ids.len() > 255 {
      Err(error_from_source_info(
        &call_start,
        "function cannot have more than 255 parameters".to_string(),
      ))
    } else {
      Ok((parameter_ids, parameter_types))
    }
  }

  pub(super) fn parse_function_return_type(&mut self) -> Result<Type, SourceError> {
    if self.match_next(Token::ThinArrow)?.is_some() {
      self.match_type_name_or_err()
    } else {
      Ok(Type::Nothing)
    }
  }

  fn parse_function_decl(&mut self) -> StmtRes {
    assert_eq!(self.lookahead, Token::Fn);
    self.advance()?; // consume fun
    let (name_id, name_info) = self.match_id_or_err()?;
    let call_start = self.lex.prev_token_info();
    self.env.push();
    self.match_or_err(Token::Basic('('))?;
    let parameters = if self.lookahead != Token::Basic(')') {
      self.parse_function_params(call_start)
    } else {
      Ok((Vec::new(), Vec::new()))
    };
    self.match_or_err(Token::Basic(')'))?;
    let return_type = self.parse_function_return_type()?;
    let block = self.parse_block()?;
    if let Stmt::Block(body) = block.get(&self.ast) {
      self.env.pop();
      let (parameters, mut fn_type) = parameters?;
      fn_type.push(return_type);
      self.env.set_type(name_id, Type::Function(fn_type.clone()));
      Ok(self.ast.add_statement(Stmt::Function {
        id: name_id,
        name_info,
        fn_type,
        parameters,
        body,
      }))
    } else {
      panic!()
    }
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
    let mut constructor_type = member_types.clone();
    constructor_type.push(Type::Struct(name_id));
    self.env.set_type(name_id, Type::Function(constructor_type));
    Ok(self.ast.add_statement(Stmt::Struct {
      name: name_id,
      name_info,
      member_names,
      member_types,
    }))
  }

  fn parse_extern_function(&mut self) -> StmtRes {
    assert_eq!(self.lookahead, Token::Extern);
    self.advance()?;
    self.match_or_err(Token::Fn)?;
    let (name_id, name_info) = self.match_id_or_err()?;
    let mut function_type = self.parse_function_param_types()?;
    function_type.push(self.parse_function_return_type()?);
    self
      .env
      .set_type(name_id, Type::Function(function_type.clone()));
    self.match_or_err(Token::Basic(';'))?;
    Ok(self.ast.add_statement(Stmt::ExternFunction {
      id: name_id,
      name_info,
      fn_type: function_type,
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
