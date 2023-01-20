use crate::{lexer::TokenInfo, Lexer};
use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum SourceErrorType {
  Lexing,
  Parsing,
  Runtime,
}

#[derive(Debug, PartialEq)]
pub struct SourceError {
  line_no: u32,
  token_start: usize,
  token_end: usize,
  line_str: String,
  error_msg: String,
  kind: SourceErrorType,
}

impl SourceError {
  pub fn from_lexer_state(lex: &Lexer, error_msg: String, kind: SourceErrorType) -> Self {
    Self {
      line_no: lex.line_no(),
      token_start: lex.prev_token_start(),
      token_end: lex.prev_token_end(),
      line_str: lex.line().to_string(),
      error_msg,
      kind,
    }
  }

  pub fn from_token_info(info: TokenInfo, error_msg: String, kind: SourceErrorType) -> Self {
    Self {
      line_no: info.line_no,
      token_start: info.start,
      token_end: info.end,
      line_str: info.line.to_string(),
      error_msg,
      kind,
    }
  }
}

impl Display for SourceError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let spaces = self.line_str[0..self.token_start]
      .char_indices()
      .map(|_| ' ')
      .collect::<String>();
    let underlines = self.line_str[self.token_start..self.token_end]
      .char_indices()
      .map(|_| '~')
      .collect::<String>();
    write!(
      f,
      "{:?} error: {}\n  | \n{} | {}\n  | {}{}\n",
      self.kind, self.error_msg, self.line_no, self.line_str, spaces, underlines
    )
  }
}
