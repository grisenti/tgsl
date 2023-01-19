use std::str::CharIndices;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum TokenType {
  Id,
  String,
  Number,
  Basic,

  // operators
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

  EndOfFile,
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
  pub fn new(kind: TokenType, lexeme: &'src str) -> Self {
    Self { kind, lexeme }
  }

  pub fn identifier(lexeme: &'src str) -> Self {
    Self {
      kind: token_type(lexeme),
      lexeme,
    }
  }

  pub fn basic(lexeme: &'src str) -> Self {
    Self {
      kind: TokenType::Basic,
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

  fn match_alternatives_or_basic(&mut self, alternatives: &[(char, TokenType)]) -> Token {
    if let Some((offset, ch)) = self.current.clone().next() {
      let start = self.total_offset;
      if let Some((_, kind)) = alternatives.into_iter().find(|(c, _)| *c == ch) {
        self.advance();
        let ret = Token::new(kind.clone(), &self.source[start..self.total_offset + 1]);
        self.advance();
        return ret;
      }
    }
    self.match_single()
  }

  fn match_single(&mut self) -> Token {
    let ret = Token::basic(&self.source[self.total_offset..self.total_offset + 1]);
    self.advance();
    ret
  }

  fn skip_line_comment(&mut self) {
    if let Some((offset, ch)) = self.current.find(|(_, c)| *c == '\n') {
      self.lookahead = ch;
      self.total_offset = offset;
    }
  }

  fn try_skip_comment(&mut self) -> Result<(), String> {
    if let Some((offset, next_ch)) = self.current.clone().next() {
      match next_ch {
        '/' => self.skip_line_comment(),
        _ => {}
      }
    }
    Ok(())
  }

  fn skip_unused(&mut self) -> Result<(), String> {
    while !self.is_at_end() {
      match self.lookahead {
        '\n' => {
          self.line_no += 1;
          self.char_no = 0;
          self.line_start = self.current.clone();
        }
        '/' => self.try_skip_comment()?,
        '\t' | ' ' => {
          self.char_no += 1;
        }
        _ => break,
      }
      self.advance();
    }
    Ok(())
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
    Token::new(
      TokenType::Number,
      &self.source[tok_start..self.total_offset],
    )
  }

  fn process_string(&mut self) -> Result<Token, String> {
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
        _ => {}
      }
      self.advance();
    }
    if self.lookahead == '"' {
      self.advance();
      Ok(Token::new(
        TokenType::String,
        &self.source[tok_start..self.total_offset],
      ))
    } else {
      Err("incomplete string".to_string())
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
    self.skip_unused()?;
    if self.is_at_end() {
      return Ok(Token::new(TokenType::EndOfFile, ""));
    }
    match self.lookahead {
      '<' => Ok(self.match_alternatives_or_basic(&[('=', TokenType::Leq)])),
      '>' => Ok(self.match_alternatives_or_basic(&[('=', TokenType::Geq)])),
      '=' => Ok(self.match_alternatives_or_basic(&[('=', TokenType::Same)])),
      '!' => Ok(self.match_alternatives_or_basic(&[('=', TokenType::Different)])),
      '"' => self.process_string(),
      _ => {
        if is_first_id_charachter(self.lookahead) {
          Ok(self.process_identifier())
        } else if self.lookahead.is_ascii_digit() {
          Ok(self.process_number())
        } else {
          Ok(self.match_single())
        }
      }
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
  fn lex_operators() {
    let mut lex = Lexer::new("+-*<=>===!=");
    assert_eq!(lex.next_token(), Ok(Token::basic("+")));
    assert_eq!(lex.next_token(), Ok(Token::basic("-")));
    assert_eq!(lex.next_token(), Ok(Token::basic("*")));
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Leq, "<=")));
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Geq, ">=")));
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Same, "==")));
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Different, "!=")));
  }

  #[test]
  fn lex_keywords() {
    let mut lex = Lexer::new("var if else");
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Var, "var")));
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::If, "if")));
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Else, "else")));
  }

  #[test]
  fn lex_strings() {
    let mut lex = Lexer::new(
      r#"
      "simple" "with \"quotes\""
      "#,
    );
    assert_eq!(
      lex.next_token(),
      Ok(Token::new(TokenType::String, "\"simple\""))
    );
    assert_eq!(
      lex.next_token(),
      Ok(Token::new(TokenType::String, r#""with \"quotes\"""#))
    );
  }

  #[test]
  fn lex_numbers() {
    let mut lex = Lexer::new("1 123 123.33 0.9");
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Number, "1")));
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Number, "123")));
    assert_eq!(
      lex.next_token(),
      Ok(Token::new(TokenType::Number, "123.33"))
    );
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Number, "0.9")));
  }

  #[test]
  fn skip_comments() {
    let mut lex = Lexer::new("//comment\nid");
    assert_eq!(lex.next_token(), Ok(Token::identifier("id")));
  }
}
