mod expression_parser;
mod statement_parser;

use super::ast::*;
use super::errors::parser_err;
use super::errors::CompilerError;
use super::lexer::*;
use super::*;
use crate::compiler::ast::parsed_type::{ParsedFunctionType, ParsedType};
use crate::compiler::ast::statement::{stmt};

#[derive(PartialEq, Eq)]
enum ParserState {
  NoErrors,
  WaitingRecovery,
  UnrecoverableError,
}

pub struct Parser<'src> {
  lex: Lexer<'src>,
  lookahead: Token<'src>,
  ast: AST<'src>,
  errors: Vec<CompilerError>,
  state: ParserState,
}

type TokenPairOpt<'parsing> = Option<(Token<'parsing>, SourceRange)>;

#[macro_export]
macro_rules! return_if_err {
  ($s:ident, $val:expr) => {
    if $s.in_panic_state() {
      return $val;
    }
  };
}

#[macro_export]
macro_rules! check_error {
  ($s:ident, $result:expr, $invalid_value:expr) => {
    match $result {
      Ok(value) => value,
      Err(err) => {
        $s.errors.push(err);
        $s.state = ParserState::WaitingRecovery;
        $invalid_value
      }
    }
  };
}

impl<'src> Parser<'src> {
  fn is_at_end(&self) -> bool {
    self.lookahead == Token::EndOfFile
  }

  fn in_panic_state(&self) -> bool {
    self.state != ParserState::NoErrors
  }

  fn emit_error(&mut self, err: CompilerError) {
    self.errors.push(err);
    self.state = ParserState::WaitingRecovery;
  }

  fn recover_from_errors(&mut self) {
    let mut stop = false;
    while !stop && self.state != ParserState::UnrecoverableError {
      match self.lookahead {
        Token::Basic(';') | Token::Basic('}') => {
          stop = true;
        }
        Token::EndOfFile
        | Token::Var
        | Token::Struct
        | Token::Fn
        | Token::If
        | Token::For
        | Token::While => {
          break;
        }
        _ => {}
      }
      self.advance();
    }
    self.state = ParserState::NoErrors;
  }

  fn match_id(&mut self) -> &'src str {
    const ERROR_ID: &'static str = "<ERROR>";
    return_if_err!(self, ERROR_ID);

    if let Token::Id(name) = self.lookahead {
      self.advance();
      name
    } else {
      self.emit_error(parser_err::expected_identifier(&self.lex, self.lookahead));
      ERROR_ID
    }
  }

  fn advance(&mut self) -> Token<'src> {
    match self.lex.next_token() {
      Ok(next) => {
        self.lookahead = next;
        next
      }
      Err(err) => {
        self.emit_error(err);
        self.state = ParserState::UnrecoverableError;
        Token::EndOfFile
      }
    }
  }

  fn unexpected_token(&mut self, expected: Option<Token>) {
    let err = if let Some(tok) = expected {
      parser_err::expected_token(&self.lex, tok, self.lookahead)
    } else {
      parser_err::unexpected_token(&self.lex, self.lookahead)
    };
    self.emit_error(err);
  }

  fn matches_alternatives(&mut self, alternatives: &[Token<'static>]) -> TokenPairOpt<'src> {
    return_if_err!(self, None);
    if alternatives.contains(&self.lookahead) {
      let res = (self.lookahead, self.lex.previous_token_range());
      self.advance();
      Some(res)
    } else {
      None
    }
  }

  fn match_next(&mut self, tok: Token<'static>) -> TokenPairOpt<'src> {
    self.matches_alternatives(&[tok])
  }

  fn match_token(&mut self, token: Token) {
    return_if_err!(self, ());
    if self.lookahead == token {
      self.advance();
    } else {
      self.unexpected_token(Some(token))
    }
  }

  fn parse_function_parameters(&mut self) -> (Vec<TypeHandle>, Vec<&'src str>) {
    return_if_err!(self, (vec![], vec![]));

    let call_start = self.lex.previous_token_range();
    self.match_token(Token::Basic('('));
    let mut types = Vec::new();
    let mut names = Vec::new();
    while self.lookahead != Token::Basic(')') {
      names.push(self.match_id());
      types.push(self.parse_type_specifier());
      if self.match_next(Token::Basic(',')).is_none() {
        break;
      }
    }
    let call_end = self.lex.previous_token_range();
    self.match_token(Token::Basic(')'));
    if types.len() > 255 {
      self.emit_error(parser_err::too_many_function_parameters(
        SourceRange::combine(call_start, call_end),
      ));
      (vec![], vec![])
    } else {
      (types, names)
    }
  }

  fn parse_type_list(&mut self) -> Vec<TypeHandle> {
    return_if_err!(self, vec![]);

    let mut type_list = vec![];
    self.match_token(Token::Basic('('));
    while self.lookahead != Token::Basic(')') {
      let parsed_type = self.match_type_name();
      type_list.push(parsed_type);
      if self.matches_alternatives(&[Token::Basic(',')]).is_none() {
        break;
      }
    }
    self.match_token(Token::Basic(')'));
    type_list
  }

  pub fn parse_function_return_type(&mut self) -> TypeHandle {
    return_if_err!(self, TypeHandle::INVALID);
    if self.match_next(Token::ThinArrow).is_some() {
      self.match_type_name()
    } else {
      TypeHandle::NOTHING
    }
  }

  fn match_type_name(&mut self) -> TypeHandle {
    return_if_err!(self, TypeHandle::INVALID);
    match self.lookahead {
      Token::Id(type_name) => {
        self.advance();
        match type_name {
          "any" => TypeHandle::ANY,
          "str" => TypeHandle::STR,
          "num" => TypeHandle::NUM,
          "bool" => TypeHandle::BOOL,
          other => self.ast.add_parsed_type(ParsedType::Named(other)),
        }
      }
      Token::Fn => {
        self.advance();
        let parameters = self.parse_type_list();
        let return_type = self.parse_function_return_type();
        self
          .ast
          .add_parsed_type(ParsedType::Function(ParsedFunctionType::new(
            parameters,
            return_type,
          )))
      }
      _ => {
        let err = parser_err::expected_type_name(&self.lex, self.lookahead);
        self.emit_error(err);
        TypeHandle::INVALID
      }
    }
  }

  fn parse_type_specifier(&mut self) -> TypeHandle {
    return_if_err!(self, TypeHandle::INVALID);
    self.match_token(Token::Basic(':'));
    self.match_type_name()
  }

  fn parse_opt_type_specifier(&mut self) -> Option<TypeHandle> {
    return_if_err!(self, Some(TypeHandle::INVALID));

    if self.match_next(Token::Basic(':')).is_some() {
      Some(self.match_type_name())
    } else {
      None
    }
  }

  fn parse_module_name(&mut self) {
    let stmt_start = self.lex.previous_token_range();
    if self.match_next(Token::Module).is_some() {
      let module_name = self.match_id();
      let stmt_end = self.lex.previous_token_range();
      self.match_next(Token::Basic(';'));
      let stmt = self.ast.add_statement(
        stmt::ModuleDecl { name: module_name },
        SourceRange::combine(stmt_start, stmt_end),
      );
      self.ast.program_push(stmt);
      if self.in_panic_state() {
        self.recover_from_errors();
      }
    }
  }

  pub fn parse_program(source: &'src str) -> Result<AST<'src>, Vec<CompilerError>> {
    let mut parser = Self {
      lex: Lexer::new(source),
      lookahead: Token::EndOfFile,
      ast: AST::new(),
      errors: Vec::new(),
      state: ParserState::NoErrors,
    };
    parser.advance();
    parser.parse_module_name();
    while !parser.is_at_end() && parser.state != ParserState::UnrecoverableError {
      let stmt = parser.parse_decl();
      parser.ast.program_push(stmt);
      if parser.in_panic_state() {
        parser.recover_from_errors();
      }
    }
    if parser.errors.is_empty() {
      Ok(parser.ast)
    } else {
      Err(parser.errors)
    }
  }
}

#[cfg(test)]
mod test {
  use crate::compiler::ast::json::ASTJSONPrinter;
  use crate::compiler::ast::visitor::{ExprVisitor, StmtVisitor};
  use crate::compiler::ast::AST;
  use crate::compiler::errors::CompilerError;
  
  use crate::compiler::lexer::{Lexer, Token};
  use crate::compiler::parser::{Parser, ParserState};
  
  use json::JsonValue;

  pub struct TestParser {
    parser: Parser<'static>,
  }

  impl TestParser {
    pub fn new(source: &'static str) -> Self {
      let mut parser = Parser {
        lex: Lexer::new(source),
        lookahead: Token::EndOfFile,
        ast: AST::new(),
        errors: Vec::new(),
        state: ParserState::NoErrors,
      };
      parser.advance();
      Self { parser }
    }

    pub fn parse_correct_expression(mut self) -> JsonValue {
      let expr = self.parser.parse_expression();
      if !self.parser.errors.is_empty() {
        panic!("parsing error: {:?}", self.parser.errors);
      }
      let mut printer = ASTJSONPrinter {};
      printer.visit_expr(&self.parser.ast, expr)
    }

    pub fn parse_expression_error(mut self) -> CompilerError {
      self.parser.parse_expression();
      self.parser.errors.first().expect("expected error").clone()
    }

    pub fn parse_correct_statement(mut self) -> JsonValue {
      let stmt = self.parser.parse_decl();
      if !self.parser.errors.is_empty() {
        panic!("parsing error: {:?}", self.parser.errors);
      }
      let mut printer = ASTJSONPrinter {};
      printer.visit_stmt(&self.parser.ast, stmt)
    }
  }
}
