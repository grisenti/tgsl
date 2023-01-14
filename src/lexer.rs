use std::{collections::HashMap, str::CharIndices};

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum TokenType {
  Id,
  String,
  Number,

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
}

fn token_type(input: &str) -> TokenType {
  match input {
    "and" => TokenType::And,
    "or" => TokenType::Or,
    "if" => TokenType::If,
    "else" => TokenType::Else,
    "true" => TokenType::True,
    "false" => TokenType::False,
    "fun" => TokenType::Fun,
    "null" => TokenType::Null,
    "ret" => TokenType::Return,
    "var" => TokenType::Var,
    _ => TokenType::Id,
  }
}

fn is_id_charachter(c: char) -> bool {
  c.is_alphabetic() || c.is_numeric() || c == '_'
}

fn is_first_id_charachter(c: char) -> bool {
  c.is_alphabetic() || c == '_'
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Token<'src> {
  pub kind: TokenType,
  pub lexeme: &'src str,
}

impl<'src> Token<'src> {
  fn new(kind: TokenType, lexeme: &'src str) -> Self {
    Self { kind, lexeme }
  }

  fn identifier(lexeme: &'src str) -> Self {
    Self {
      kind: token_type(lexeme),
      lexeme,
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
  char_no: u32,
}

impl<'src> Lexer<'src> {
  fn advance(&mut self) -> bool {
    if let Some((offset, ch)) = self.current.next() {
      self.total_offset = offset;
      self.lookahead = ch;
      return true;
    }
    false
  }

  fn skip_whitespace(&mut self) {
    loop {
      let ch = self.lookahead;
      if ch == '\n' {
        self.line_no += 1;
        self.char_no = 0;
        self.line_start = self.current.clone();
      } else if ch == '\t' || ch == ' ' {
        self.char_no += 1;
      } else {
        break;
      }
      if !self.advance() {
        break;
      }
    }
  }

  fn process_identifier(&mut self) -> Token {
    assert!(is_first_id_charachter(self.lookahead));
    let tok_start = self.total_offset;
    if let Some((tok_end, last_ch)) = self.current.find(|(_, c)| !is_id_charachter(*c)) {
      self.total_offset = tok_end;
      self.lookahead = last_ch;
      Token::identifier(&self.source[tok_start..tok_end])
    } else {
      self.total_offset = self.source.len();
      self.lookahead = '\0';
      Token::identifier(&self.source[tok_start..])
    }
  }

  fn process_number(&mut self) -> Token {
    assert!(self.lookahead.is_ascii_digit());
    let tok_start = self.total_offset;
    if let Some((tok_end, last_ch)) = self.current.find(|(_, c)| !c.is_numeric()) {
      self.total_offset = tok_end;
      self.lookahead = last_ch;
      Token::new(TokenType::Number, &self.source[tok_start..tok_end])
    } else {
      self.total_offset = self.source.len();
      self.lookahead = '\0';
      Token::new(TokenType::Number, &self.source[tok_start..])
    }
  }

  fn process_string(&mut self) -> Token {
    assert!(self.lookahead == '"');
    let tok_start = self.total_offset;
    self.current.next();
    self.current.find(|(_, c)| *c == '"').unwrap();
    if let Some((tok_end, last_ch)) = self.current.next() {
      self.total_offset = tok_end;
      self.lookahead = last_ch;
      Token::new(TokenType::String, &self.source[tok_start..tok_end])
    } else {
      self.total_offset = self.source.len();
      self.lookahead = '\0';
      Token::new(TokenType::String, &self.source[tok_start..])
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
        line_no: 0,
        char_no: 0,
      }
    } else {
      Self {
        source,
        current: source.char_indices(),
        total_offset: 0,
        lookahead: '\0',
        line_start: source.char_indices(),
        line_no: 0,
        char_no: 0,
      }
    }
  }

  pub fn next_token(&mut self) -> Result<Token, String> {
    self.skip_whitespace();
    if is_first_id_charachter(self.lookahead) {
      Ok(self.process_identifier())
    } else if self.lookahead.is_ascii_digit() {
      Ok(self.process_number())
    } else if self.lookahead == '"' {
      Ok(self.process_string())
    } else {
      Err("invalid charachter".to_string())
    }
  }

  /// line of the last token
  pub fn line(&self) -> &'src str {
    let mut start = self.line_start.clone();
    if let Some((end, _)) = start.find(|(_, c)| *c == '\n') {
      &self.line_start.as_str()[..end]
    } else {
      self.line_start.as_str()
    }
  }

  pub fn line_no(&self) -> u32 {
    self.line_no
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn get_line() {
    let mut lex = Lexer::new("first\nsecond line");
    assert_eq!(lex.line(), "first");
    assert_eq!(lex.next_token(), Ok(Token::identifier("first")));
    assert_eq!(lex.next_token(), Ok(Token::identifier("second")));
    assert_eq!(lex.line(), "second line");
  }

  #[test]
  fn lex_identifiers() {
    let mut lex = Lexer::new("token t2__2\n\t  __tok4");
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Id, "token")));
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Id, "t2__2")));
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Id, "__tok4")));
  }

  #[test]
  fn lex_keywords() {}

  #[test]
  fn lex_strings() {}

  #[test]
  fn lex_numbers() {}
}
