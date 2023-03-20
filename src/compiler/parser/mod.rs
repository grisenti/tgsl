mod environment;
mod expression_parser;
mod statement_parser;

use self::environment::Environment;

use super::ast::*;
use super::identifier::GlobalId;
use super::identifier::Identifier;
use super::identifier::ModuleId;
use super::lexer::*;
use super::modules::GlobalNames;
use super::modules::ModuleNames;
use super::types::type_map::TypeMap;
use super::types::Type;
use super::types::TypeId;
use super::*;

pub struct ParseResult {
  pub ast: AST,
  pub module_names: GlobalNames,
  pub module_extern_functions: Vec<GlobalId>,
  pub imports: Vec<ModuleId>,
}

pub struct Parser<'parsing> {
  env: Environment<'parsing>,
  type_map: &'parsing mut TypeMap,
  lex: Lexer<'parsing>,
  lookahead: Token<'parsing>,
  loaded_modules: &'parsing HashMap<String, ModuleId>,
  ast: AST,
}

type SrcErrVec = Vec<SourceError>;
type TokenPairOpt<'parsing> = Option<(Token<'parsing>, SourceInfoHandle)>;
type ExprRes = Result<ExprHandle, SourceError>;
type StmtRes = Result<StmtHandle, SourceError>;

impl<'parsing> Parser<'parsing> {
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

  fn advance(&mut self) -> Result<Token<'parsing>, SourceError> {
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
  ) -> Result<TokenPairOpt<'parsing>, SourceError> {
    if alternatives.contains(&self.lookahead) {
      let res = (self.lookahead, self.last_token_info());
      self.advance()?;
      Ok(Some(res))
    } else {
      Ok(None)
    }
  }

  fn match_next(&mut self, tok: Token<'static>) -> Result<TokenPairOpt<'parsing>, SourceError> {
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

  pub fn parse(
    source: &'parsing str,
    type_map: &'parsing mut TypeMap,
    loaded_names: &'parsing ModuleNames,
    global_types: &'parsing mut Vec<TypeId>,
    loaded_modules: &'parsing HashMap<String, ModuleId>,
  ) -> Result<ParseResult, SourceError> {
    let mut parser = Self {
      lex: Lexer::new(source),
      type_map,
      lookahead: Token::EndOfFile,
      loaded_modules,
      ast: AST::new(),
      env: Environment::new(loaded_names, global_types),
    };
    let mut errors = Vec::new();
    if let Err(e) = parser.advance() {
      errors.push(e)
    }
    while !parser.is_at_end() {
      match parser.parse_decl() {
        Ok(stmt) => parser.ast.program_push(stmt),
        Err(err) => {
          errors.push(err);
          errors = parser.syncronize_or_errors(errors)?;
        }
      }
    }
    if errors.is_empty() {
      let (name_map, extern_map) = parser.env.finalize();
      Ok(ParseResult {
        ast: parser.ast,
        module_names: name_map,
        module_extern_functions: extern_map,
        imports: vec![],
      })
    } else {
      Err(SourceError::from_err_vec(errors))
    }
  }
}
