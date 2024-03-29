use std::borrow::Cow;
use std::{fmt::Display, str::CharIndices};

use crate::compiler::errors::lex_err;

use super::errors::CompilerResult;

type LexedString<'src> = Cow<'src, str>;

#[derive(Clone, PartialEq, Debug)]
pub enum Token<'src> {
  String(LexedString<'src>),
  Number(f64),
  Id(&'src str),
  Basic(char),

  // wide operators
  Leq,
  Geq,
  Same,
  Different,
  ThinArrow,

  // keywords
  And,
  Or,
  If,
  Else,
  True,
  False,
  Fn,
  Null,
  Return,
  Var,
  While,
  For,
  Break,
  Struct,
  Foreign,
  Import,
  Module,

  EndOfFile,

  Error,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct SourceRange {
  pub start: usize,
  pub end: usize,
}

impl SourceRange {
  pub const EMPTY: Self = Self { start: 0, end: 0 };

  pub fn new(start: usize, end: usize) -> Self {
    Self { start, end }
  }

  pub fn combine(r1: Self, r2: Self) -> Self {
    Self::new(r1.start.min(r2.start), r1.end.max(r2.end))
  }
}

fn indentifier_token(input: &str) -> Token {
  match input {
    "and" => Token::And,
    "or" => Token::Or,
    "if" => Token::If,
    "else" => Token::Else,
    "true" => Token::True,
    "false" => Token::False,
    "fn" => Token::Fn,
    "null" => Token::Null,
    "return" => Token::Return,
    "var" => Token::Var,
    "struct" => Token::Struct,
    "while" => Token::While,
    "for" => Token::For,
    "break" => Token::Break,
    "foreign" => Token::Foreign,
    "import" => Token::Import,
    "module" => Token::Module,
    _ => Token::Id(input),
  }
}

fn is_id_charachter(c: char) -> bool {
  c.is_alphabetic() || c.is_numeric() || c == '_'
}

fn is_first_id_charachter(c: char) -> bool {
  c.is_alphabetic() || c == '_'
}

impl Display for Token<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::String(s) => write!(f, "\"{s}\""),
      Self::Number(num) => write!(f, "{num}"),
      Self::Id(id) => write!(f, "{id}"),
      Self::Basic(c) => write!(f, "'{c}'"),
      _ => write!(f, "{self:?}"),
    }
  }
}

pub struct Lexer<'src> {
  source: &'src str,
  current: CharIndices<'src>,
  lookahead: char,
  total_offset: usize,
  line_no: u32,
  prev_token_start: usize,
}

impl<'src> Lexer<'src> {
  fn is_at_end(&self) -> bool {
    self.total_offset == self.source.len()
  }

  fn advance(&mut self) {
    if let Some((offset, ch)) = self.current.next() {
      self.total_offset = offset;
      self.lookahead = ch;
    } else {
      self.total_offset = self.source.len();
    }
  }

  fn match_wide_or_single_operator(&mut self, alternatives: &[(char, Token<'src>)]) -> Token<'src> {
    if let Some((_, ch)) = self.current.clone().next() {
      if let Some((_, kind)) = alternatives.iter().find(|(c, _)| *c == ch) {
        self.advance();
        self.advance();
        return kind.clone();
      }
    }
    self.match_basic()
  }

  fn match_basic(&mut self) -> Token<'src> {
    let ret = Token::Basic(self.lookahead);
    self.advance();
    ret
  }

  fn skip_line_comment(&mut self) {
    while !self.is_at_end() && self.lookahead != '\n' {
      self.advance()
    }
  }

  fn try_skip_comment(&mut self) -> CompilerResult<bool> {
    if let Some((_, next_ch)) = self.current.clone().next() {
      match next_ch {
        '/' => {
          self.skip_line_comment();
          return Ok(true);
        }
        _ => {}
      }
    }
    Ok(false)
  }

  fn skip_unused(&mut self) -> CompilerResult<()> {
    while !self.is_at_end() {
      match self.lookahead {
        '\n' => {
          self.line_no += 1;
          self.advance();
        }
        '/' => {
          if !self.try_skip_comment()? {
            break;
          }
        }
        '\t' | ' ' | '\r' => {
          self.advance();
        }
        _ => break,
      }
    }
    Ok(())
  }

  fn process_identifier(&mut self) -> Token<'src> {
    assert!(is_first_id_charachter(self.lookahead));
    let tok_start = self.total_offset;
    if let Some((tok_end, last_ch)) = self.current.find(|(_, c)| !is_id_charachter(*c)) {
      self.total_offset = tok_end;
      self.lookahead = last_ch;
      indentifier_token(&self.source[tok_start..tok_end])
    } else {
      self.total_offset = self.source.len();
      self.lookahead = '\0';
      indentifier_token(&self.source[tok_start..])
    }
  }

  fn process_number(&mut self) -> Token<'src> {
    assert!(self.lookahead.is_ascii_digit());
    let mut dot_encountered = false;
    let tok_start = self.total_offset;
    while !self.is_at_end() {
      if self.lookahead.is_ascii_digit() {
        self.advance();
      } else if self.lookahead == '.' && !dot_encountered {
        if let Some((_, c)) = self.current.clone().next() {
          if !c.is_ascii_digit() {
            break;
          }
        }
        dot_encountered = true;
        self.advance();
      } else {
        break;
      }
    }
    Token::Number(self.source[tok_start..self.total_offset].parse().unwrap()) // number already checked
  }

  fn close_string(&mut self) {
    while !self.is_at_end() && self.lookahead != '"' && self.lookahead != '\n' {
      self.advance();
    }
    if self.lookahead == '"' {
      self.advance();
    } else {
      todo!()
    }
  }

  fn process_escaped_string(&mut self, token_start: usize) -> CompilerResult<Token<'src>> {
    let mut escaped_string = self.source[token_start..self.total_offset].to_string();
    while !self.is_at_end() {
      match self.lookahead {
        '\n' => return Err(lex_err::incomplete_string(self as &_)),
        '\\' => {
          self.advance();
          match self.lookahead {
            '\\' => escaped_string.push('\\'),
            '"' => escaped_string.push('"'),
            'n' => escaped_string.push('\n'),
            'r' => escaped_string.push('\r'),
            't' => escaped_string.push('\t'),
            other => {
              let sequence_start = self.total_offset - 1;
              self.close_string();
              return Err(lex_err::invalid_escape_character(
                SourceRange::new(sequence_start, sequence_start + 2),
                other,
              ));
            }
          }
        }
        '\"' => break,
        other => escaped_string.push(other),
      }
      self.advance();
    }
    if self.lookahead == '"' {
      self.advance();
      Ok(Token::String(escaped_string.into()))
    } else {
      Err(lex_err::incomplete_string(self as &_))
    }
  }

  fn process_string(&mut self) -> CompilerResult<Token<'src>> {
    assert_eq!(self.lookahead, '"');
    self.advance();
    let tok_start = self.total_offset;
    let mut escaped = false;
    while !self.is_at_end() {
      // if its '\', we have an escape ch
      match self.lookahead {
        '\n' => return Err(lex_err::incomplete_string(self as &_)),
        '\\' => return self.process_escaped_string(tok_start),
        '\"' => break,
        _ => {}
      }
      self.advance();
    }
    if self.lookahead == '"' {
      let tok_end = self.total_offset;
      self.advance();
      Ok(Token::String(self.source[tok_start..tok_end].into()))
    } else {
      Err(lex_err::incomplete_string(self as &_))
    }
  }

  pub fn new(source: &'src str) -> Self {
    let mut start = source.char_indices();
    if let Some((offset, ch)) = start.next() {
      Self {
        source,
        current: start.clone(),
        total_offset: offset,
        lookahead: ch,
        line_no: 1,
        prev_token_start: 0,
      }
    } else {
      Self {
        source,
        current: source.char_indices(),
        total_offset: 0,
        lookahead: '\0',
        line_no: 0,
        prev_token_start: 0,
      }
    }
  }

  pub fn next_token(&mut self) -> CompilerResult<Token<'src>> {
    self.skip_unused()?;
    self.prev_token_start = self.total_offset;
    if self.is_at_end() {
      return Ok(Token::EndOfFile);
    }
    match self.lookahead {
      '<' => Ok(self.match_wide_or_single_operator(&[('=', Token::Leq)])),
      '>' => Ok(self.match_wide_or_single_operator(&[('=', Token::Geq)])),
      '=' => Ok(self.match_wide_or_single_operator(&[('=', Token::Same)])),
      '!' => Ok(self.match_wide_or_single_operator(&[('=', Token::Different)])),
      '-' => Ok(self.match_wide_or_single_operator(&[('>', Token::ThinArrow)])),
      '"' => self.process_string(),
      c if is_first_id_charachter(c) => Ok(self.process_identifier()),
      c if c.is_ascii_digit() => Ok(self.process_number()),
      _ => Ok(self.match_basic()),
    }
  }

  pub fn line_no(&self) -> u32 {
    self.line_no
  }

  pub fn prev_token_start(&self) -> usize {
    self.prev_token_start
  }

  pub fn prev_token_end(&self) -> usize {
    self.total_offset
  }

  pub fn previous_token_range(&self) -> SourceRange {
    SourceRange::new(self.prev_token_start, self.total_offset)
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn lex_identifiers() {
    let mut lex = Lexer::new("token\r\nt2__2\n\t  __tok4");
    assert_eq!(lex.next_token(), Ok(Token::Id("token")));
    assert_eq!(lex.next_token(), Ok(Token::Id("t2__2")));
    assert_eq!(lex.next_token(), Ok(Token::Id("__tok4")));
  }

  #[test]
  fn lex_operators() {
    let mut lex = Lexer::new("+-*<=>===!=");
    assert_eq!(lex.next_token(), Ok(Token::Basic('+')));
    assert_eq!(lex.next_token(), Ok(Token::Basic('-')));
    assert_eq!(lex.next_token(), Ok(Token::Basic('*')));
    assert_eq!(lex.next_token(), Ok(Token::Leq));
    assert_eq!(lex.next_token(), Ok(Token::Geq));
    assert_eq!(lex.next_token(), Ok(Token::Same));
    assert_eq!(lex.next_token(), Ok(Token::Different));
  }

  #[test]
  fn lex_keywords() {
    let mut lex = Lexer::new("var if else");
    assert_eq!(lex.next_token(), Ok(Token::Var));
    assert_eq!(lex.next_token(), Ok(Token::If));
    assert_eq!(lex.next_token(), Ok(Token::Else));
  }

  #[test]
  fn lex_simple_strings() {
    let mut lex = Lexer::new("\"simple string\"");
    assert_eq!(lex.next_token(), Ok(Token::String("simple string".into())));
  }

  #[test]
  fn lex_escaped_strings() {
    let mut lex = Lexer::new(r#""\tescaped \r\n \"string""#);
    assert_eq!(
      lex.next_token(),
      Ok(Token::String("\tescaped \r\n \"string".into()))
    );
  }

  #[test]
  fn lex_numbers() {
    let mut lex = Lexer::new("1 123 123.33 0.9 1.a");
    assert_eq!(lex.next_token(), Ok(Token::Number(1.0)));
    assert_eq!(lex.next_token(), Ok(Token::Number(123.0)));
    assert_eq!(lex.next_token(), Ok(Token::Number(123.33)));
    assert_eq!(lex.next_token(), Ok(Token::Number(0.9)));
    assert_eq!(lex.next_token(), Ok(Token::Number(1.0)));
  }

  #[test]
  fn skip_comments() {
    let mut lex = Lexer::new("//comment\nid");
    assert_eq!(lex.next_token(), Ok(Token::Id("id")));
  }

  #[test]
  fn skip_comment_without_trailing_newline() {
    let mut lex = Lexer::new("//comment");
    assert_eq!(lex.next_token(), Ok(Token::EndOfFile));
  }

  #[test]
  #[allow(unused)]
  fn token_start() {
    let mut lex = Lexer::new("hello how\nare you");
    lex.next_token();
    assert_eq!(lex.prev_token_start(), 0);
    lex.next_token();
    assert_eq!(lex.prev_token_start(), 6);
    lex.next_token();
    assert_eq!(lex.prev_token_start(), 10);
    lex.next_token();
    assert_eq!(lex.prev_token_start(), 14);
  }

  #[test]
  fn error_for_multiline_strings() {
    let mut lex = Lexer::new("\"hello\n\"");
    assert!(lex.next_token().is_err());
  }

  #[test]
  fn error_for_incomplete_strings() {
    let mut lex = Lexer::new("\"hello");
    assert!(lex.next_token().is_err());
  }
}
