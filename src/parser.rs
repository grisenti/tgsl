use super::ast::*;
use super::errors::*;
use super::lexer::*;

pub struct Parser<'src> {
  lex: Lexer<'src>,
  lookahead: Token<'src>,
}

type SrcErrVec = Vec<SourceError>;
type TokenPairOpt<'src> = Option<TokenPair<'src>>;
type ExprRes<'src> = Result<Box<Expr<'src>>, SourceError>;
type StmtRes<'src> = Result<Stmt<'src>, SourceError>;

impl<'src> Parser<'src> {
  fn is_at_end(&self) -> bool {
    self.lookahead == Token::EndOfFile
  }

  fn advance(&mut self) -> Result<Token<'src>, SourceError> {
    let next = self.lex.next_token()?;
    self.lookahead = next.clone();
    Ok(next)
  }

  fn unexpected_token(&self, expected: Option<Token>) -> SourceError {
    let msg = if let Some(tok) = expected {
      format!("expected {}, got {}", tok, self.lookahead)
    } else {
      format!("unexpected token {}", self.lookahead)
    };
    SourceError::from_lexer_state(&self.lex, msg, SourceErrorType::Parsing)
  }

  fn matches_alternatives(
    &mut self,
    alternatives: &[Token<'static>],
  ) -> Result<TokenPairOpt<'src>, SourceError> {
    if alternatives.contains(&self.lookahead) {
      let res = TokenPair::new(self.lookahead, self.lex.prev_token_info());
      self.advance()?;
      Ok(Some(res))
    } else {
      Ok(None)
    }
  }

  fn match_or_err(&mut self, token: Token) -> Result<(), SourceError> {
    if self.lookahead == token {
      self.advance()?;
      Ok(())
    } else {
      Err(self.unexpected_token(Some(token)))
    }
  }

  fn syncronize_or_errors(&mut self, mut errors: SrcErrVec) -> Result<SrcErrVec, SrcErrVec> {
    loop {
      if let Err(e) = self.advance() {
        errors.push(e);
        return Err(errors);
      }
      if self.lookahead != Token::Basic(';') {
        break;
      }
    }
    Ok(errors)
  }

  fn parse_primary(&mut self) -> ExprRes<'src> {
    match self.lookahead {
      Token::Number(_) | Token::String(_) | Token::True | Token::False | Token::Null => {
        let ret = Ok(Box::new(Expr::Literal(TokenPair::new(
          self.lookahead,
          self.lex.prev_token_info(),
        ))));
        self.advance()?;
        ret
      }
      Token::Id(id) => {
        let ret = Ok(Box::new(Expr::Variable {
          id,
          id_info: self.lex.prev_token_info(),
        }));
        self.advance()?;
        ret
      }
      Token::Basic(c) if c == '(' => {
        self.advance()?;
        let ret = Ok(self.parse_expression()?);
        self.match_or_err(Token::Basic(')'))?;
        ret
      }
      _ => Err(SourceError::from_lexer_state(
        &self.lex,
        format!("expected literal, got {}", self.lookahead),
        SourceErrorType::Parsing,
      )),
    }
  }

  fn parse_unary(&mut self) -> ExprRes<'src> {
    if let Some(op) = self.matches_alternatives(&[Token::Basic('-'), Token::Basic('!')])? {
      Ok(Box::new(Expr::UnaryExpr {
        operator: op,
        right: self.parse_primary()?,
      }))
    } else {
      Ok(self.parse_primary()?)
    }
  }

  fn parse_factor(&mut self) -> ExprRes<'src> {
    let mut expr = self.parse_unary()?;
    while let Some(op) = self.matches_alternatives(&[Token::Basic('*'), Token::Basic('/')])? {
      let right = self.parse_unary()?;
      expr = Box::new(Expr::BinaryExpr {
        left: expr,
        operator: op,
        right,
      })
    }
    Ok(expr)
  }

  fn parse_term(&mut self) -> ExprRes<'src> {
    let mut expr = self.parse_factor()?;
    while let Some(op) = self.matches_alternatives(&[Token::Basic('+'), Token::Basic('-')])? {
      let right = self.parse_factor()?;
      expr = Box::new(Expr::BinaryExpr {
        left: expr,
        operator: op,
        right,
      })
    }
    Ok(expr)
  }

  fn parse_comparison(&mut self) -> ExprRes<'src> {
    let mut expr = self.parse_term()?;
    while let Some(op) =
      self.matches_alternatives(&[Token::Leq, Token::Geq, Token::Basic('<'), Token::Basic('>')])?
    {
      let right = self.parse_term()?;
      expr = Box::new(Expr::BinaryExpr {
        left: expr,
        operator: op,
        right,
      })
    }
    Ok(expr)
  }

  fn parse_equality(&mut self) -> ExprRes<'src> {
    let mut expr = self.parse_comparison()?;
    while let Some(op) = self.matches_alternatives(&[Token::Same, Token::Different])? {
      let right = self.parse_comparison()?;
      expr = Box::new(Expr::BinaryExpr {
        left: expr,
        operator: op,
        right,
      })
    }
    Ok(expr)
  }

  fn parse_assignment(&mut self) -> ExprRes<'src> {
    let lhs = self.parse_equality()?;
    if let Some(eq) = self.matches_alternatives(&[Token::Basic('=')])? {
      let rhs = self.parse_assignment()?;
      if let Expr::Variable { id, id_info } = *lhs {
        return Ok(Box::new(Expr::Assignment {
          name: id,
          name_info: id_info,
          value: rhs,
        }));
      } else {
        return Err(SourceError::from_token_info(
          eq.info,
          "left hand side of assignment is not a valid target".to_string(),
          SourceErrorType::Parsing,
        ));
      }
    }
    Ok(lhs)
  }

  fn parse_expression(&mut self) -> ExprRes<'src> {
    self.parse_assignment()
  }

  fn parse_print_stmt(&mut self) -> StmtRes<'src> {
    assert_eq!(self.lookahead, Token::Print);
    self.advance()?;
    Ok(Stmt::Print {
      expression: *self.parse_expression()?,
    })
  }

  fn parse_block(&mut self) -> StmtRes<'src> {
    assert_eq!(self.lookahead, Token::Basic('{'));
    self.advance()?;
    let mut statements = Vec::new();
    while self.lookahead != Token::Basic('}') && !self.is_at_end() {
      statements.push(self.parse_decl()?);
    }
    self.match_or_err(Token::Basic('}'));
    Ok(Stmt::Block(statements))
  }

  fn parse_statement(&mut self) -> StmtRes<'src> {
    match self.lookahead {
      Token::Print => self.parse_print_stmt(),
      Token::Basic('{') => self.parse_block(),
      _ => Ok(Stmt::ExprStmt(*self.parse_expression()?)),
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
          id_info: id_info,
          expression: Some(*self.parse_expression()?),
        }
      } else {
        Stmt::VarDecl {
          identifier: id,
          id_info: self.lex.prev_token_info(),
          expression: None,
        }
      };
      Ok(ret)
    } else {
      Err(SourceError::from_lexer_state(
        &self.lex,
        format!("expected identifier, got {}", self.lookahead),
        SourceErrorType::Parsing,
      ))
    }
  }

  fn parse_decl(&mut self) -> StmtRes<'src> {
    let ret = match self.lookahead {
      Token::Var => self.parse_var_decl()?,
      _ => self.parse_statement()?,
    };
    self.match_or_err(Token::Basic(';'))?;
    Ok(ret)
  }

  pub fn new(lex: Lexer<'src>) -> Self {
    Self {
      lex,
      lookahead: Token::EndOfFile,
    }
  }

  pub fn parse(&'src mut self) -> Result<Vec<Stmt<'src>>, SrcErrVec> {
    let mut program = Vec::new();
    let mut errors = Vec::new();
    if let Err(e) = self.advance() {
      errors.push(e)
    }
    while !self.is_at_end() {
      match self.parse_decl() {
        Ok(stmt) => program.push(stmt),
        Err(err) => {
          errors.push(err);
          errors = self.syncronize_or_errors(errors)?;
        }
      }
    }
    if errors.is_empty() {
      Ok(program)
    } else {
      Err(errors)
    }
  }
}
