use super::*;

impl<'src> Parser<'src> {
  fn parse_print_stmt(&mut self) -> StmtRes<'src> {
    assert_eq!(self.lookahead, Token::Print);
    self.advance()?;
    let ret = Ok(Stmt::Print {
      expression: *self.parse_expression()?,
    });
    self.match_or_err(Token::Basic(';'))?;
    ret
  }

  fn parse_block(&mut self, in_loop: bool) -> StmtRes<'src> {
    assert_eq!(self.lookahead, Token::Basic('{'));
    self.advance()?;
    let mut statements = Vec::new();
    let mut errors = Vec::new();
    while self.lookahead != Token::Basic('}') && !self.is_at_end() {
      match self.parse_decl(in_loop) {
        Ok(stmt) => statements.push(stmt),
        Err(err) => {
          errors.push(err);
          errors = self.syncronize_or_errors(errors)?;
        }
      }
    }
    self.match_or_err(Token::Basic('}'))?;
    if errors.is_empty() {
      Ok(Stmt::Block(statements))
    } else {
      Err(SourceError::from_err_vec(errors))
    }
  }

  fn parse_expr_stmt(&mut self) -> StmtRes<'src> {
    let ret = Stmt::Expr(*self.parse_expression()?);
    self.match_or_err(Token::Basic(';'))?;
    Ok(ret)
  }

  fn parse_if_stmt(&mut self, in_loop: bool) -> StmtRes<'src> {
    assert_eq!(self.lookahead, Token::If);
    let if_info = self.lex.prev_token_info();
    self.advance()?;
    self.match_or_err(Token::Basic('('))?;
    let condition = *self.parse_expression()?;
    self.match_or_err(Token::Basic(')'))?;
    let true_branch = Box::new(self.parse_statement(in_loop)?);
    let else_branch = if (self.matches_alternatives(&[Token::Else])?).is_some() {
      Some(Box::new(self.parse_statement(in_loop)?))
    } else {
      None
    };
    Ok(Stmt::IfBranch {
      if_info,
      condition,
      true_branch,
      else_branch,
    })
  }

  fn parse_while_stmt(&mut self) -> StmtRes<'src> {
    assert_eq!(self.lookahead, Token::While);
    let info = self.lex.prev_token_info();
    self.advance()?; // consume while
    self.match_or_err(Token::Basic('('))?;
    let condition = *self.parse_expression()?;
    self.match_or_err(Token::Basic(')'))?;
    let loop_body = Box::new(self.parse_statement(true)?);
    Ok(Stmt::While {
      info,
      condition,
      loop_body,
    })
  }

  fn parse_for_stmt(&mut self) -> StmtRes<'src> {
    assert_eq!(self.lookahead, Token::For);
    let info = self.lex.prev_token_info();
    self.advance()?; //consume for
    self.match_or_err(Token::Basic('('))?;
    let init = self.parse_decl(false)?;
    let condition = self.parse_expression()?;
    self.match_or_err(Token::Basic(';'))?;
    let after = self.parse_expression()?;
    self.match_or_err(Token::Basic(')'))?;
    let body = self.parse_statement(true)?;
    Ok(Stmt::Block(vec![
      init,
      Stmt::While {
        info,
        condition: *condition,
        loop_body: Box::new(Stmt::Block(vec![body, Stmt::Expr(*after)])),
      },
    ]))
  }

  fn parse_loop_early_out(&mut self, kind: Stmt<'src>) -> StmtRes<'src> {
    assert!(matches!(self.lookahead, Token::Break));
    self.advance()?;
    self.match_or_err(Token::Basic(';'))?;
    Ok(kind)
  }

  fn parse_statement(&mut self, in_loop: bool) -> StmtRes<'src> {
    match self.lookahead {
      Token::Print => self.parse_print_stmt(),
      Token::Basic('{') => self.parse_block(in_loop),
      Token::If => self.parse_if_stmt(in_loop),
      Token::While => self.parse_while_stmt(),
      Token::For => self.parse_for_stmt(),
      Token::Break if !in_loop => Err(SourceError::from_lexer_state(
        &self.lex,
        format!("cannot use {:?} outside of loops", self.lookahead),
        SourceErrorType::Parsing,
      )),
      Token::Break => self.parse_loop_early_out(Stmt::Break),
      _ => self.parse_expr_stmt(),
    }
  }

  fn parse_var_decl(&mut self) -> StmtRes<'src> {
    assert_eq!(self.lookahead, Token::Var);
    self.advance()?;
    if let Token::Id(id) = self.lookahead {
      let id_info = self.lex.prev_token_info();
      self.advance()?; // consume id
      let ret = if self.lookahead == Token::Basic('=') {
        self.advance()?; // consume '='
        Stmt::VarDecl {
          identifier: id,
          id_info,
          expression: Some(*self.parse_expression()?),
        }
      } else {
        Stmt::VarDecl {
          identifier: id,
          id_info: self.lex.prev_token_info(),
          expression: None,
        }
      };
      self.match_or_err(Token::Basic(';'))?;
      Ok(ret)
    } else {
      Err(SourceError::from_lexer_state(
        &self.lex,
        format!("expected identifier, got {}", self.lookahead),
        SourceErrorType::Parsing,
      ))
    }
  }

  pub(super) fn parse_decl(&mut self, in_loop: bool) -> StmtRes<'src> {
    let ret = match self.lookahead {
      Token::Var => self.parse_var_decl()?,
      _ => self.parse_statement(in_loop)?,
    };
    Ok(ret)
  }
}
