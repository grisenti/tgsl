mod environment;
mod expression_parser;
mod statement_parser;

use self::environment::Environment;
use crate::compiler::parser::environment::ResolvedIdentifier;
use crate::compiler::types::FunctionSignature;

use super::ast::*;
use super::errors::ge_err;
use super::errors::parser_err;
use super::errors::CompilerError;
use super::errors::CompilerResult;
use super::global_env::GlobalEnv;
use super::global_env::Struct;
use super::identifier::GlobalIdentifier;
use super::identifier::Identifier;
use super::identifier::StructId;
use super::identifier::VariableIdentifier;
use super::lexer::*;
use super::types::Type;
use super::*;

#[derive(PartialEq, Eq)]
enum ParserState {
  NoErrors,
  WaitingRecovery,
  UnrecoverableError,
}

pub struct ParsedModule {
  pub module_name: Option<String>,
  pub ast: AST,
  pub global_names: HashMap<String, GlobalIdentifier>,
  pub module_global_variable_types: Vec<Type>,
  pub module_extern_functions_types: Vec<Type>,
  pub module_structs: Vec<Option<Struct>>,
}

pub struct Parser<'parsing> {
  env: Environment<'parsing>,
  lex: Lexer<'parsing>,
  lookahead: Token<'parsing>,
  ast: AST,
  errors: Vec<CompilerError>,
  state: ParserState,
  loop_depth: u32,
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

impl<'parsing> Parser<'parsing> {
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

  fn get_variable_id(&mut self, name: &str, name_sr: SourceRange) -> (VariableIdentifier, Type) {
    check_error!(
      self,
      self
        .env
        .get_variable_id(name, name_sr)
        .map(|(id, type_)| (id, type_.clone())),
      (VariableIdentifier::Invalid, Type::Error)
    )
  }

  fn get_struct_id(&mut self, name: &str, name_sr: SourceRange) -> StructId {
    check_error!(
      self,
      self.env.get_struct_id(name, name_sr),
      StructId::relative(0)
    )
  }

  fn get_id(&mut self, name: &str, name_sr: SourceRange) -> ResolvedIdentifier {
    check_error!(
      self,
      self.env.get_id(name, name_sr),
      ResolvedIdentifier::Error
    )
  }

  fn match_id_or_err(&mut self) -> (&'parsing str, SourceRange) {
    const ERROR_RESULT: (&'static str, SourceRange) = ("<ERROR>", SourceRange::EMPTY);
    return_if_err!(self, ERROR_RESULT);

    if let Token::Id(name) = self.lookahead {
      let name_sr = self.lex.previous_token_range();
      self.advance();
      (name, name_sr)
    } else {
      self.emit_error(parser_err::expected_identifier(&self.lex, self.lookahead));
      ERROR_RESULT
    }
  }

  fn advance(&mut self) -> Token<'parsing> {
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

  fn matches_alternatives(&mut self, alternatives: &[Token<'static>]) -> TokenPairOpt<'parsing> {
    return_if_err!(self, None);
    if alternatives.contains(&self.lookahead) {
      let res = (self.lookahead, self.lex.previous_token_range());
      self.advance();
      Some(res)
    } else {
      None
    }
  }

  fn match_next(&mut self, tok: Token<'static>) -> TokenPairOpt<'parsing> {
    self.matches_alternatives(&[tok])
  }

  fn match_or_err(&mut self, token: Token) {
    return_if_err!(self, ());
    if self.lookahead == token {
      self.advance();
    } else {
      self.unexpected_token(Some(token))
    }
  }

  fn id_str_or_err(&mut self) -> &str {
    return_if_err!(self, "<ERROR>");

    if let Token::Id(name) = self.lookahead {
      self.advance();
      name
    } else {
      self.emit_error(parser_err::expected_identifier(&self.lex, self.lookahead));
      "<ERROR>"
    }
  }

  fn parse_function_param_types(&mut self) -> Vec<Type> {
    return_if_err!(self, vec![]);
    self.match_or_err(Token::Basic('('));
    let mut result = Vec::new();
    if self.lookahead != Token::Basic(')') {
      loop {
        result.push(self.match_type_name_or_err());
        if self.match_next(Token::Basic(',')).is_none() {
          break;
        }
      }
    }
    self.match_or_err(Token::Basic(')'));
    result
  }

  fn match_type_name_or_err(&mut self) -> Type {
    return_if_err!(self, Type::Error);
    match self.lookahead {
      Token::Id(type_name) => {
        self.advance();
        match type_name {
          "any" => Type::Any,
          "str" => Type::Str,
          "num" => Type::Num,
          "bool" => Type::Bool,
          other => {
            let struct_sr = self.lex.previous_token_range();
            let struct_id = self.get_struct_id(other, struct_sr);
            Type::Struct(struct_id)
          }
        }
      }
      Token::Fn => {
        self.advance();
        let parameters = self.parse_function_param_types();
        self.match_or_err(Token::ThinArrow);
        let return_type = self.match_type_name_or_err();
        Type::Function(FunctionSignature::new(parameters, return_type))
      }
      _ => {
        let err = parser_err::expected_type_name(&self.lex, self.lookahead);
        self.emit_error(err);
        Type::Error
      }
    }
  }

  fn parse_type_specifier_or_err(&mut self) -> Type {
    return_if_err!(self, Type::Error);
    self.match_or_err(Token::Basic(':'));
    self.match_type_name_or_err()
  }

  fn parse_opt_type_specifier(&mut self) -> Option<Type> {
    return_if_err!(self, Some(Type::Error));

    if self.match_next(Token::Basic(':')).is_some() {
      Some(self.match_type_name_or_err())
    } else {
      None
    }
  }

  fn parse_module_name(&mut self, global_env: &GlobalEnv) -> Option<String> {
    if self.match_next(Token::Module).is_some() {
      if let Token::Id(module_name) = self.lookahead {
        let id_sr = self.lex.previous_token_range();
        self.advance();
        self.match_next(Token::Basic(';'));
        if global_env.is_module_name_available(module_name) {
          return Some(module_name.to_string());
        } else {
          self.emit_error(ge_err::trying_to_redeclare_a_module(id_sr, module_name));
        }
      } else {
        self.emit_error(parser_err::expected_module_identifier(
          &self.lex,
          self.lookahead,
        ));
      }
    }
    None
  }

  pub fn parse(
    source: &'parsing str,
    global_env: &'parsing GlobalEnv,
  ) -> Result<ParsedModule, Vec<CompilerError>> {
    let mut parser = Self {
      lex: Lexer::new(source),
      lookahead: Token::EndOfFile,
      ast: AST::new(),
      env: Environment::new(global_env),
      errors: Vec::new(),
      state: ParserState::NoErrors,
      loop_depth: 0,
    };
    parser.advance();
    let module_name = parser.parse_module_name(global_env);
    while !parser.is_at_end() && parser.state != ParserState::UnrecoverableError {
      let stmt = parser.parse_decl();
      parser.ast.program_push(stmt.handle);
      if parser.in_panic_state() {
        parser.recover_from_errors();
      }
    }
    if parser.errors.is_empty() {
      Ok(ParsedModule {
        module_name,
        ast: parser.ast,
        global_names: parser.env.global_names,
        module_global_variable_types: parser.env.module_global_variables_types,
        module_extern_functions_types: parser.env.extern_function_types,
        module_structs: parser.env.module_structs,
      })
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
  use crate::compiler::global_env::test::EMPTY_GLOBAL_ENV;
  use crate::compiler::lexer::{Lexer, SourceRange, Token};
  use crate::compiler::parser::environment::Environment;
  use crate::compiler::parser::{Parser, ParserState};
  use crate::compiler::types::Type;
  use json::JsonValue;

  pub struct TestParser {
    parser: Parser<'static>,
  }

  impl TestParser {
    pub fn new(source: &'static str) -> Self {
      let env = Environment::new(&EMPTY_GLOBAL_ENV);
      let mut parser = Parser {
        lex: Lexer::new(source),
        lookahead: Token::EndOfFile,
        ast: AST::new(),
        env,
        errors: Vec::new(),
        state: ParserState::NoErrors,
        loop_depth: 0,
      };
      parser.advance();
      Self { parser }
    }

    pub fn declare_name(mut self, name: &'static str, type_: Type) -> Self {
      self
        .parser
        .env
        .define_variable(name, SourceRange::EMPTY, type_)
        .expect("could not declare name");
      self
    }

    pub fn define_struct(
      mut self,
      name: &'static str,
      member_names: Vec<&str>,
      member_types: Vec<Type>,
    ) -> Self {
      assert_eq!(member_types.len(), member_names.len());

      let member_names = member_names.iter().map(|s| s.to_string()).collect();
      self
        .parser
        .env
        .define_struct(name, SourceRange::EMPTY, member_names, member_types)
        .expect("could not define struct");
      self
    }

    pub fn parse_correct_expression(mut self) -> JsonValue {
      let expr = self.parser.parse_expression();
      if !self.parser.errors.is_empty() {
        panic!("parsing error: {:?}", self.parser.errors);
      }
      let mut printer = ASTJSONPrinter {};
      printer.visit_expr(&self.parser.ast, expr.handle)
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
      printer.visit_stmt(&self.parser.ast, stmt.handle)
    }
  }
}
