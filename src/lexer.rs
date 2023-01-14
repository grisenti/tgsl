use std::{collections::HashMap, str::CharIndices};

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum TokenType {
  Id,
  String,
  Number,

  Plus,
  Minus,
  Times,
  Divide,
  Le,
  Gr,
  Leq,
  Geq,
  Eq,
  Same,
  Different,
  Not,

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

  fn match_double_operator(
    &mut self,
    single: TokenType,
    double_requires: char,
    double_kind: TokenType,
  ) -> Token {
    if let Some((offset, ch)) = self.current.clone().next() {
      let start = self.total_offset;
      if ch == double_requires {
        self.advance();
        let ret = Token::new(double_kind, &self.source[start..self.total_offset + 1]);
        self.advance();
        return ret;
      }
    }
    self.match_single_operator(single)
  }

  fn match_single_operator(&mut self, kind: TokenType) -> Token {
    let ret = Token::new(kind, &self.source[self.total_offset..self.total_offset + 1]);
    self.advance();
    ret
  }

  fn skip_whitespace(&mut self) {
    while !self.is_at_end() {
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
      self.advance();
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
    let mut dot_encountered = false;
    let tok_start = self.total_offset;
    while !self.is_at_end() {
      if self.lookahead.is_ascii_digit() || (self.lookahead == '.' && !dot_encountered) {
        self.advance();
      } else {
        break;
      }
    }
    Token::new(
      TokenType::Number,
      &self.source[tok_start..self.total_offset],
    )
  }

  fn process_string(&mut self) -> Token {
    todo!();
    assert!(self.lookahead == '"');
    let tok_start = self.total_offset;
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
    match self.lookahead {
      '<' => Ok(self.match_double_operator(TokenType::Le, '=', TokenType::Leq)),
      '>' => Ok(self.match_double_operator(TokenType::Gr, '=', TokenType::Geq)),
      '=' => Ok(self.match_double_operator(TokenType::Eq, '=', TokenType::Same)),
      '!' => Ok(self.match_double_operator(TokenType::Not, '=', TokenType::Different)),
      '+' => Ok(self.match_single_operator(TokenType::Plus)),
      '-' => Ok(self.match_single_operator(TokenType::Minus)),
      '/' => Ok(self.match_single_operator(TokenType::Divide)),
      '*' => Ok(self.match_single_operator(TokenType::Times)),
      _ => {
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
    let mut lex = Lexer::new("+-*/=!<><=>===!=");
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Plus, "+")));
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Minus, "-")));
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Times, "*")));
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Divide, "/")));
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Eq, "=")));
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Not, "!")));
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Le, "<")));
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Gr, ">")));
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Leq, "<=")));
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Geq, ">=")));
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Same, "==")));
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Different, "!=")));
  }

  #[test]
  fn lex_keywords() {}

  #[test]
  fn lex_strings() {}

  #[test]
  fn lex_numbers() {
    let mut lex = Lexer::new("123 123.33 0.99");
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Number, "123")));
    assert_eq!(
      lex.next_token(),
      Ok(Token::new(TokenType::Number, "123.33"))
    );
    assert_eq!(lex.next_token(), Ok(Token::new(TokenType::Number, "0.99")));
  }
}
