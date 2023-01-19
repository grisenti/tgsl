use std::string::ParseError;

use crate::ast::ASTNode;

use super::ast::*;
use super::errors::*;
use super::lexer::*;

pub struct Parser<'src> {
  lex: Lexer<'src>,
  lookahead: Token<'src>,
  errors: Vec<CompilerError>,
}

pub type PRes<NodeType> = Result<Box<NodeType>, Vec<CompilerError>>;

impl<'src> Parser<'src> {
  fn advance(&mut self) -> Result<Token, Vec<CompilerError>> {
    match self.lex.next_token() {
      Ok(next) => {
        self.lookahead = next.clone();
        Ok(next)
      }
      Err(_) => panic!(),
    }
  }

  fn match_alternatives(&mut self, alternatives: &[Token<'static>]) -> Option<Token<'src>> {
    if alternatives.contains(&self.lookahead) {
      let res = self.lookahead.clone();
      self.advance();
      Some(res)
    } else {
      None
    }
  }

  fn syncronize(&mut self) {
    todo!();
  }

  fn parse_primary(&mut self) -> PRes<Expr<'src>> {
    let res = match self.lookahead.kind {
      TokenType::Number => Ok(Box::new(Expr::Literal {
        literal: self.lookahead.clone(),
      })),
      TokenType::String => Ok(Box::new(Expr::Literal {
        literal: self.lookahead.clone(),
      })),
      TokenType::True => Ok(Box::new(Expr::Literal {
        literal: self.lookahead.clone(),
      })),
      TokenType::False => Ok(Box::new(Expr::Literal {
        literal: self.lookahead.clone(),
      })),
      TokenType::Null => Ok(Box::new(Expr::Literal {
        literal: self.lookahead.clone(),
      })),
      _ => Err(vec![CompilerError::from_lexer_state(
        &self.lex,
        format!("expected literal, got {}", self.lookahead),
        ErrorType::Parsing,
      )]),
    }?;
    self.advance();
    Ok(res)
  }

  fn parse_unary(&mut self) -> PRes<Expr<'src>> {
    if let Some(op) = self.match_alternatives(&[Token::basic("-"), Token::basic("!")]) {
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
    while let Some(op) = self.match_alternatives(&[Token::basic("*"), Token::basic("/")]) {
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
    while let Some(op) = self.match_alternatives(&[Token::basic("+"), Token::basic("-")]) {
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
    while let Some(op) = self.match_alternatives(&[
      Token::new(TokenType::Leq, "<="),
      Token::new(TokenType::Geq, ">="),
      Token::basic("<"),
      Token::basic(">"),
    ]) {
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
    while let Some(op) = self.match_alternatives(&[
      Token::new(TokenType::Same, "=="),
      Token::new(TokenType::Different, "!="),
    ]) {
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
      lookahead: Token::eof(),
      errors: Vec::new(),
    }
  }

  pub fn parse(&'src mut self) -> PRes<Expr<'src>> {
    self.errors.clear();
    self.advance();
    self.parse_expression()
  }
}
