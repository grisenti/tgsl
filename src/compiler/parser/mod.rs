mod environment;
mod expression_parser;
mod statement_parser;

use std::collections::HashMap;

use self::environment::Environment;

use super::ast::*;
use super::lexer::*;
use super::*;

pub struct Parser<'src> {
  env: Environment,
  lex: Lexer<'src>,
  lookahead: Token<'src>,
  ast: AST,
}

type SrcErrVec = Vec<SourceError>;
type TokenPairOpt<'src> = Option<(Token<'src>, SourceInfoHandle)>;
type ExprRes = Result<ExprHandle, SourceError>;
type StmtRes = Result<StmtHandle, SourceError>;

impl<'src> Parser<'src> {
  fn is_at_end(&self) -> bool {
    self.lookahead == Token::EndOfFile
  }

  fn match_id_or_err(&mut self) -> Result<(Identifier, SourceInfoHandle), SourceError> {
    if let Token::Id(id) = self.lookahead {
      let info = self.last_token_info();
      self.advance()?;
      Ok((self.env.get_name_or_add_global(id), info))
    } else {
      Err(error_from_lexer_state(
        &self.lex,
        format!("expected identifier, got {}", self.lookahead),
      ))
    }
  }

  fn last_token_info(&mut self) -> SourceInfoHandle {
    self.ast.add_source_info(self.lex.prev_token_info())
  }

  fn advance(&mut self) -> Result<Token<'src>, SourceError> {
    let next = self.lex.next_token()?;
    self.lookahead = next;
    Ok(next)
  }

  fn unexpected_token(&self, expected: Option<Token>) -> SourceError {
    let msg = if let Some(tok) = expected {
      format!("expected {}, got {}", tok, self.lookahead)
    } else {
      format!("unexpected token {}", self.lookahead)
    };
    error_from_lexer_state(&self.lex, msg)
  }

  fn matches_alternatives(
    &mut self,
    alternatives: &[Token<'static>],
  ) -> Result<TokenPairOpt<'src>, SourceError> {
    if alternatives.contains(&self.lookahead) {
      let res = (self.lookahead, self.last_token_info());
      self.advance()?;
      Ok(Some(res))
    } else {
      Ok(None)
    }
  }

  fn match_next(&mut self, tok: Token<'static>) -> Result<TokenPairOpt<'src>, SourceError> {
    self.matches_alternatives(&[tok])
  }

  fn match_or_err(&mut self, token: Token) -> Result<(), SourceError> {
    if self.lookahead == token {
      self.advance()?;
      Ok(())
    } else {
      Err(self.unexpected_token(Some(token)))
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

  fn match_type_name_or_err(&mut self) -> Result<Type, SourceError> {
    if let Token::Id(type_name) = self.lookahead {
      self.advance()?;
      Ok(self.env.get_type_or_add(type_name))
    } else {
      Err(error_from_lexer_state(
        &self.lex,
        format!("expected type name, got {}", self.lookahead),
      ))
    }
  }

  fn parse_type_specifier(&mut self) -> Result<Type, SourceError> {
    if self.lookahead == Token::Basic(':') {
      self.advance()?;
      self.match_type_name_or_err()
    } else {
      Ok(Type::Any)
    }
  }

  fn syncronize_or_errors(&mut self, mut errors: SrcErrVec) -> Result<SrcErrVec, SourceError> {
    let mut stop = false;
    while !stop {
      match self.lookahead {
        Token::Basic(';') | Token::Basic('}') => {
          stop = true;
        }
        Token::EndOfFile => {
          break;
        }
        _ => {}
      }
      if let Err(e) = self.advance() {
        errors.push(e);
        return Err(SourceError::from_err_vec(errors));
      }
    }
    Ok(errors)
  }

  pub fn new(lex: Lexer<'src>) -> Self {
    Self {
      lex,
      lookahead: Token::EndOfFile,
      ast: AST::new(),
      env: Environment::new(),
    }
  }

  pub fn parse(mut self) -> Result<(AST, HashMap<String, Identifier>), SourceError> {
    let mut errors = Vec::new();
    if let Err(e) = self.advance() {
      errors.push(e)
    }
    while !self.is_at_end() {
      match self.parse_decl() {
        Ok(stmt) => self.ast.program_push(stmt),
        Err(err) => {
          errors.push(err);
          errors = self.syncronize_or_errors(errors)?;
        }
      }
    }
    if errors.is_empty() {
      Ok((self.ast, self.env.get_global()))
    } else {
      Err(SourceError::from_err_vec(errors))
    }
  }
}
