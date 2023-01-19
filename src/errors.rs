use crate::Lexer;
use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum ErrorType {
  Lexing,
  Parsing,
}

#[derive(Debug, PartialEq)]
pub struct CompilerError {
  line_no: u32,
  line_str: String,
  error_msg: String,
  kind: ErrorType,
}

impl CompilerError {
  pub fn from_lexer_state(lex: &Lexer, error_msg: String, kind: ErrorType) -> Self {
    Self {
      line_no: lex.line_no(),
      line_str: lex.line().to_string(),
      error_msg,
      kind,
    }
  }
}

impl Display for CompilerError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{:?} error at:\n  | \n{} | {}\n  | \n{}",
      self.kind, self.line_no, self.line_str, self.error_msg
    )
  }
}
