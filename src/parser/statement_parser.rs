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

  fn parse_block(&mut self) -> StmtRes<'src> {
    assert_eq!(self.lookahead, Token::Basic('{'));
    self.advance()?;
    let mut statements = Vec::new();
    while self.lookahead != Token::Basic('}') && !self.is_at_end() {
      statements.push(self.parse_decl()?);
    }
    self.match_or_err(Token::Basic('}'))?;
    Ok(Stmt::Block(statements))
  }

  fn parse_expr_stmt(&mut self) -> StmtRes<'src> {
    let ret = Stmt::Expr(*self.parse_expression()?);
    self.match_or_err(Token::Basic(';'))?;
    Ok(ret)
  }

  fn parse_if_stmt(&mut self) -> StmtRes<'src> {
    assert_eq!(self.lookahead, Token::If);
    let if_info = self.lex.prev_token_info();
    self.advance()?;
    self.match_or_err(Token::Basic('('))?;
    let condition = *self.parse_expression()?;
    self.match_or_err(Token::Basic(')'))?;
    let true_branch = Box::new(self.parse_statement()?);
    let else_branch = if (self.matches_alternatives(&[Token::Else])?).is_some() {
      Some(Box::new(self.parse_statement()?))
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
    let loop_body = Box::new(self.parse_statement()?);
    Ok(Stmt::While {
      info,
      condition,
      loop_body,
    })
  }

  fn parse_statement(&mut self) -> StmtRes<'src> {
    match self.lookahead {
      Token::Print => self.parse_print_stmt(),
      Token::Basic('{') => self.parse_block(),
      Token::If => self.parse_if_stmt(),
      Token::While => self.parse_while_stmt(),
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

  pub(super) fn parse_decl(&mut self) -> StmtRes<'src> {
    let ret = match self.lookahead {
      Token::Var => self.parse_var_decl()?,
      _ => self.parse_statement()?,
    };
    Ok(ret)
  }
}
