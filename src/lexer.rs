use std::{fmt::Display, str::CharIndices};

use crate::errors::*;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Token<'src> {
  String(&'src str),
  Number(f64),
  Id(&'src str),
  Basic(char),

  // wide operators
  Leq,
  Geq,
  Same,
  Different,

  // keywords
  And,
  Or,
  If,
  Else,
  True,
  False,
  Fun,
  Null,
  Return,
  Var,
  Print,
  While,
  For,
  Break,

  EndOfFile,
}

#[derive(Clone, Copy, Debug)]
pub struct TokenInfo<'src> {
  pub line_no: u32,
  pub line: &'src str,
  pub start: usize,
  pub end: usize,
}

impl<'src> TokenInfo<'src> {
  pub fn union(start: TokenInfo<'src>, end: TokenInfo<'src>) -> Self {
    assert!(start.line_no <= end.line_no);
    Self {
      line_no: start.line_no,
      line: start.line,
      start: start.start,
      end: if start.line_no < end.line_no {
        start.line.len()
      } else {
        end.end
      },
    }
  }
}

#[derive(Clone, Copy, Debug)]
pub struct TokenPair<'src> {
  pub token: Token<'src>,
  pub info: TokenInfo<'src>,
}

impl<'src> TokenPair<'src> {
  pub fn new(token: Token<'src>, info: TokenInfo<'src>) -> Self {
    Self { token, info }
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
    "fun" => Token::Fun,
    "null" => Token::Null,
    "ret" => Token::Return,
    "var" => Token::Var,
    "print" => Token::Print,
    "while" => Token::While,
    "for" => Token::For,
    "break" => Token::Break,
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
      Self::String(s) => write!(f, "\"{}\"", s),
      Self::Number(num) => write!(f, "{}", num),
      Self::Id(id) => write!(f, "{}", id),
      Self::Basic(c) => write!(f, "'{}'", c),
      _ => write!(f, "{:?}", self),
    }
  }
}

pub struct Lexer<'src> {
  source: &'src str,
  current: CharIndices<'src>,
  lookahead: char,
  total_offset: usize,
  line_start: CharIndices<'src>,
  line_no: u32,
  line_start_offset: usize,
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
      if let Some((_, kind)) = alternatives.into_iter().find(|(c, _)| *c == ch) {
        self.advance();
        self.advance();
        return *kind;
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
    if let Some((offset, ch)) = self.current.find(|(_, c)| *c == '\n') {
      self.lookahead = ch;
      self.total_offset = offset;
    }
  }

  fn try_skip_comment(&mut self) -> Result<bool, SourceError> {
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

  fn skip_unused(&mut self) -> Result<(), SourceError> {
    while !self.is_at_end() {
      match self.lookahead {
        '\n' => {
          self.line_no += 1;
          self.line_start_offset = self.total_offset + 1;
          self.line_start = self.current.clone();
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
      if self.lookahead.is_ascii_digit() || (self.lookahead == '.' && !dot_encountered) {
        self.advance();
      } else if self.lookahead == '.' {
        dot_encountered = true;
      } else {
        break;
      }
    }
    Token::Number(self.source[tok_start..self.total_offset].parse().unwrap()) // number already checked
  }

  fn process_string(&mut self) -> Result<Token<'src>, SourceError> {
    assert!(self.lookahead == '"');
    let tok_start = self.total_offset;
    let mut escaped = false;
    self.advance();
    while !self.is_at_end() {
      match self.lookahead {
        '"' => {
          if !escaped {
            break;
          } else {
            escaped = false;
          }
        }
        '\\' => escaped = true,
        '\n' => break,
        _ => {}
      }
      self.advance();
    }
    if self.lookahead == '"' {
      self.advance();
      Ok(Token::String(
        &self.source[tok_start + 1..self.total_offset - 1],
      ))
    } else {
      Err(SourceError::from_lexer_state(
        self,
        "incomplete string".to_string(),
        SourceErrorType::Lexing,
      ))
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
        line_start: source.char_indices(),
        line_no: 1,
        line_start_offset: 0,
        prev_token_start: 0,
      }
    } else {
      Self {
        source,
        current: source.char_indices(),
        total_offset: 0,
        lookahead: '\0',
        line_start: source.char_indices(),
        line_no: 0,
        line_start_offset: 0,
        prev_token_start: 0,
      }
    }
  }

  pub fn next_token(&mut self) -> Result<Token<'src>, SourceError> {
    self.skip_unused()?;
    self.prev_token_start = self.total_offset - self.line_start_offset;
    if self.is_at_end() {
      return Ok(Token::EndOfFile);
    }
    match self.lookahead {
      '<' => Ok(self.match_wide_or_single_operator(&[('=', Token::Leq)])),
      '>' => Ok(self.match_wide_or_single_operator(&[('=', Token::Geq)])),
      '=' => Ok(self.match_wide_or_single_operator(&[('=', Token::Same)])),
      '!' => Ok(self.match_wide_or_single_operator(&[('=', Token::Different)])),
      '"' => self.process_string(),
      c if is_first_id_charachter(c) => Ok(self.process_identifier()),
      c if c.is_ascii_digit() => Ok(self.process_number()),
      _ => Ok(self.match_basic()),
    }
  }

  /// line of the last token
  pub fn line(&self) -> &'src str {
    let mut start = self.line_start.clone();
    if let Some((end, _)) = start.find(|(_, c)| *c == '\n') {
      &self.line_start.as_str()[..(end - self.line_start_offset)]
    } else {
      self.line_start.as_str()
    }
  }

  pub fn line_no(&self) -> u32 {
    self.line_no
  }

  pub fn prev_token_start(&self) -> usize {
    self.prev_token_start
  }

  pub fn prev_token_end(&self) -> usize {
    self.total_offset - self.line_start_offset
  }

  pub fn prev_token_info(&self) -> TokenInfo<'src> {
    TokenInfo {
      line_no: self.line_no,
      line: self.line(),
      start: self.prev_token_start,
      end: self.prev_token_end(),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn get_line() {
    let mut lex = Lexer::new("first\nsecond\nthird");
    assert_eq!(lex.line(), "first");
    assert_eq!(lex.next_token(), Ok(Token::Id("first")));
    assert_eq!(lex.next_token(), Ok(Token::Id("second")));
    assert_eq!(lex.line(), "second");
    assert_eq!(lex.next_token(), Ok(Token::Id("third")));
    assert_eq!(lex.line(), "third");
  }

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
  fn lex_strings() {
    let mut lex = Lexer::new(
      r#"
      "simple" "with \"quotes\""
      "#,
    );
    assert_eq!(lex.next_token(), Ok(Token::String("simple")));
    assert_eq!(lex.next_token(), Ok(Token::String("with \\\"quotes\\\"")));
  }

  #[test]
  fn lex_numbers() {
    let mut lex = Lexer::new("1 123 123.33 0.9");
    assert_eq!(lex.next_token(), Ok(Token::Number(1.0)));
    assert_eq!(lex.next_token(), Ok(Token::Number(123.0)));
    assert_eq!(lex.next_token(), Ok(Token::Number(123.33)));
    assert_eq!(lex.next_token(), Ok(Token::Number(0.9)));
  }

  #[test]
  fn skip_comments() {
    let mut lex = Lexer::new("//comment\nid");
    assert_eq!(lex.next_token(), Ok(Token::Id("id")));
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
    assert_eq!(lex.prev_token_start(), 0);
    lex.next_token();
    assert_eq!(lex.prev_token_start(), 4);
  }

  #[test]
  fn error_for_multiline_strings() {
    let mut lex = Lexer::new("\"hello\n\"");
    if lex.next_token().is_err() {
      let _ = &lex.line()[lex.prev_token_start()..lex.prev_token_end()]; // does not go out of bounds
    } else {
      panic!("no error for multiline string");
    }
  }

  #[test]
  fn error_for_incomplete_strings() {
    let mut lex = Lexer::new("\"hello");
    if lex.next_token().is_err() {
      let _ = &lex.line()[lex.prev_token_start()..lex.prev_token_end()]; // does not go out of bounds
    } else {
      panic!("no error for incomplete string");
    }
  }
}
