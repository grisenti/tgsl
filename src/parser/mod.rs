mod environment;
mod expression_parser;
mod statement_parser;

use self::environment::Environment;

use super::ast::*;
use super::errors::*;
use super::lexer::*;

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

  fn last_token_info(&mut self) -> SourceInfoHandle {
    self.ast.add_source_info(self.lex.prev_token_info())
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
      env: Environment::global(),
    }
  }

  pub fn parse(mut self) -> Result<AST, SourceError> {
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
      Ok(self.ast)
    } else {
      Err(SourceError::from_err_vec(errors))
    }
  }
}

#[cfg(test)]
mod test {
  use crate::{ast::Expr, lexer::Lexer};

  use super::Parser;

  fn create_parser(input: &str) -> Parser {
    let lex = Lexer::new(input);
    let mut parser = Parser::new(lex);
    parser.advance().unwrap();
    parser
  }

  #[test]
  fn assign_to_rvalue() {
    let mut parser = create_parser("var id; id = 2");
    let _declaration = parser.parse_decl().unwrap();
    let assignment = parser.parse_expression().unwrap();
    assert!(matches!(
      parser.ast.get_expression(assignment),
      Expr::Assignment {
        id: _,
        id_info: _,
        value: _
      }
    ));
  }

  #[test]
  #[should_panic]
  fn error_for_undefined_identifier() {
    let mut parser = create_parser("identifier");
    let _identifier = parser.parse_expression().unwrap();
  }

  #[test]
  fn parse_identifier() {
    let mut parser = create_parser("var identifier; identifier");
    let _declaration = parser.parse_decl().unwrap();
    let variable = parser.parse_expression().unwrap();
    match parser.ast.get_expression(variable) {
      Expr::Variable { id, id_info: _ } => {
        assert_eq!(id.0, 0)
      }
      a => panic!("expected variable, got {a:?}"),
    }
  }
}
