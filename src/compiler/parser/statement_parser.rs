use std::collections::HashMap;

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
    let ret = if self.lookahead == Token::Basic('=') {
      self.advance()?; // consume '='
      Stmt::VarDecl {
        identifier,
        id_info,
        expression: Some(self.parse_expression()?),
      }
    } else {
      Stmt::VarDecl {
        identifier,
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
  ) -> Result<Vec<Identifier>, SourceError> {
    let mut parameters = Vec::new();
    loop {
      parameters.push(self.match_id_or_err()?.0);
      if self.matches_alternatives(&[Token::Basic(',')])?.is_none() {
        break;
      }
    }
    if parameters.len() > 255 {
      Err(error_from_source_info(
        &call_start,
        "function cannot have more than 255 parameters".to_string(),
      ))
    } else {
      Ok(parameters)
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
      Ok(Vec::new())
    };
    self.match_or_err(Token::Basic(')'))?;
    let block = self.parse_block()?;
    if let Stmt::Block(body) = self.ast.get_statement(block) {
      self.env.pop();
      Ok(self.ast.add_statement(Stmt::Function {
        id: name_id,
        name_info,
        parameters: parameters?,
        body,
      }))
    } else {
      panic!()
    }
  }

  fn id_str_or_err(&mut self) -> Result<StrHandle, SourceError> {
    if let Token::Id(name) = self.lookahead {
      self.advance()?;
      Ok(self.ast.add_str(name))
    } else {
      Err(error_from_lexer_state(
        &self.lex,
        format!("expected identifier, got {}", self.lookahead),
      ))
    }
  }

  fn parse_struct_decl(&mut self) -> StmtRes {
    assert_eq!(self.lookahead, Token::Struct);
    self.advance()?;
    let (name_id, name_info) = self.match_id_or_err()?;
    self.match_or_err(Token::Basic('{'))?;
    let mut members = Vec::new();
    while self.lookahead != Token::Basic('}') {
      members.push(self.id_str_or_err()?);
      self.match_or_err(Token::Basic(','))?;
    }
    self.match_or_err(Token::Basic('}'))?;
    Ok(self.ast.add_statement(Stmt::Struct {
      name: name_id,
      name_info,
      members,
    }))
  }

  pub(super) fn parse_decl(&mut self) -> StmtRes {
    let ret = match self.lookahead {
      Token::Var => self.parse_var_decl()?,
      Token::Fn => self.parse_function_decl()?,
      Token::Struct => self.parse_struct_decl()?,
      _ => self.parse_statement()?,
    };
    Ok(ret)
  }
}