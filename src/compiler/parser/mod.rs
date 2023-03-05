mod environment;
mod expression_parser;
mod statement_parser;

use std::collections::HashMap;

use self::environment::Environment;
use self::environment::ReverseGlobalNamesMap;
use self::type_map::ReverseTypeMap;
use self::type_map::TypeMap;

use super::ast::*;
use super::identifier::ExternId;
use super::identifier::Identifier;
use super::lexer::*;
use super::types::Type;
use super::types::TypeId;
use super::*;

pub struct ParseResult {
  pub ast: AST,
  pub global_types: Vec<TypeId>,
  pub type_map: ReverseTypeMap,
  pub name_map: HashMap<String, Identifier>,
  pub extern_map: HashMap<Identifier, ExternId>,
}

pub struct Parser<'src> {
  env: Environment,
  type_map: TypeMap,
  global_types: Vec<TypeId>,
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
      let info = self.lex.prev_token_info();
      self.advance()?;
      Ok((
        self.env.declare_name_or_err(id, info)?,
        self.ast.add_source_info(info),
      ))
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

  fn parse_function_param_types(&mut self) -> Result<Vec<TypeId>, SourceError> {
    self.match_or_err(Token::Basic('('))?;
    let mut result = Vec::new();
    if self.lookahead != Token::Basic(')') {
      loop {
        result.push(self.match_type_name_or_err()?);
        if self.match_next(Token::Basic(','))?.is_none() {
          break;
        }
      }
    }
    self.match_or_err(Token::Basic(')'))?;
    Ok(result)
  }

  fn match_type_name_or_err(&mut self) -> Result<TypeId, SourceError> {
    match self.lookahead {
      Token::Id(type_name) => {
        self.advance()?;
        match type_name {
          "str" => Ok(TypeId::STR),
          "num" => Ok(TypeId::NUM),
          "bool" => Ok(TypeId::BOOL),
          other => {
            let struct_id = self.env.get_name_or_add_global(other);
            Ok(self.type_map.get_or_add(Type::Struct(struct_id)))
          }
        }
      }
      Token::Fn => {
        self.advance()?;
        let mut parameters = self.parse_function_param_types()?;
        self.match_or_err(Token::ThinArrow)?;
        let ret = self.match_type_name_or_err()?;
        Ok(self.type_map.get_or_add(Type::Function { parameters, ret }))
      }
      _ => Err(error_from_lexer_state(
        &self.lex,
        format!("expected type name, got {}", self.lookahead),
      )),
    }
  }

  fn parse_type_specifier_or_err(&mut self) -> Result<TypeId, SourceError> {
    self.match_or_err(Token::Basic(':'))?;
    self.match_type_name_or_err()
  }

  fn parse_opt_type_specifier(&mut self) -> Result<TypeId, SourceError> {
    if self.match_next(Token::Basic(':'))?.is_some() {
      self.match_type_name_or_err()
    } else {
      Ok(TypeId::UNKNOWN)
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
      type_map: TypeMap::new(),
      global_types: Vec::new(),
      lookahead: Token::EndOfFile,
      ast: AST::new(),
      env: Environment::new(),
    }
  }

  pub fn parse(mut self) -> Result<ParseResult, SourceError> {
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
      let (name_map, extern_map) = self.env.finalize();
      Ok(ParseResult {
        ast: self.ast,
        global_types: self.global_types,
        type_map: self.type_map.reverse_map(),
        name_map,
        extern_map,
      })
    } else {
      Err(SourceError::from_err_vec(errors))
    }
  }
}
