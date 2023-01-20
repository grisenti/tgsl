use super::ast::*;
use super::errors::*;
use super::lexer::*;

pub struct Parser<'src> {
  lex: Lexer<'src>,
  lookahead: Token<'src>,
  errors: Vec<CompilerError>,
}

type CompErrVec = Vec<CompilerError>;
type TokenPairOpt<'src> = Option<TokenPair<'src>>;
type PRes<Node> = Result<Box<Node>, CompErrVec>;

impl<'src> Parser<'src> {
  fn advance(&mut self) -> Result<Token<'src>, CompErrVec> {
    match self.lex.next_token() {
      Ok(next) => {
        self.lookahead = next.clone();
        Ok(next)
      }
      Err(_) => panic!(),
    }
  }

  fn matches_alternatives(
    &mut self,
    alternatives: &[Token<'static>],
  ) -> Result<TokenPairOpt<'src>, CompErrVec> {
    if alternatives.contains(&self.lookahead) {
      let res = TokenPair::new(self.lookahead, self.lex.prev_token_info());
      self.advance()?;
      Ok(Some(res))
    } else {
      Ok(None)
    }
  }

  fn match_or_err(&mut self, token: Token) -> Result<(), CompErrVec> {
    if self.lookahead == token {
      self.advance()?;
      Ok(())
    } else {
      Err(vec![CompilerError::from_lexer_state(
        &self.lex,
        format!("expected ')' got, {}", self.lookahead),
        ErrorType::Parsing,
      )])
    }
  }

  fn syncronize(&mut self) {
    todo!();
  }

  fn parse_primary(&mut self) -> PRes<Expr<'src>> {
    match self.lookahead {
      Token::Number(_) | Token::String(_) | Token::True | Token::False | Token::Null => {
        let ret = Ok(Box::new(Expr::Literal {
          literal: TokenPair::new(self.lookahead, self.lex.prev_token_info()),
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
      _ => Err(vec![CompilerError::from_lexer_state(
        &self.lex,
        format!("expected literal, got {}", self.lookahead),
        ErrorType::Parsing,
      )]),
    }
  }

  fn parse_unary(&mut self) -> PRes<Expr<'src>> {
    if let Some(op) = self.matches_alternatives(&[Token::Basic('-'), Token::Basic('!')])? {
      Ok(Box::new(Expr::UnaryExpr {
        operator: op,
        right: self.parse_primary()?,
      }))
    } else {
      Ok(self.parse_primary()?)
    }
  }

  fn parse_factor(&mut self) -> PRes<Expr<'src>> {
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

  fn parse_term(&mut self) -> PRes<Expr<'src>> {
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

  fn parse_comparison(&mut self) -> PRes<Expr<'src>> {
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

  fn parse_equality(&mut self) -> PRes<Expr<'src>> {
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

  fn parse_expression(&mut self) -> PRes<Expr<'src>> {
    self.parse_equality()
  }

  pub fn new(lex: Lexer<'src>) -> Self {
    Self {
      lex,
      lookahead: Token::EndOfFile,
      errors: Vec::new(),
    }
  }

  pub fn parse(&'src mut self) -> PRes<Expr<'src>> {
    self.errors.clear();
    self.advance()?;
    self.parse_expression()
  }
}
