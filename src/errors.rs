use crate::{lexer::TokenInfo, Lexer};
use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum SourceErrorType {
  Lexing,
  Parsing,
  Runtime,
}

#[derive(PartialEq)]
pub enum SourceError {
  One {
    line_no: u32,
    token_start: usize,
    token_end: usize,
    line_str: String,
    error_msg: String,
    kind: SourceErrorType,
  },
  Many(Vec<SourceError>),
}

impl SourceError {
  pub fn from_lexer_state(lex: &Lexer, error_msg: String, kind: SourceErrorType) -> Self {
    Self::One {
      line_no: lex.line_no(),
      token_start: lex.prev_token_start(),
      token_end: lex.prev_token_end(),
      line_str: lex.line().to_string(),
      error_msg,
      kind,
    }
  }

  pub fn from_token_info(info: &TokenInfo, error_msg: String, kind: SourceErrorType) -> Self {
    Self::One {
      line_no: info.line_no,
      token_start: info.start,
      token_end: info.end,
      line_str: info
        .line
        .char_indices()
        .map(|(_, c)| if c == '\t' { ' ' } else { c })
        .collect(),
      error_msg,
      kind,
    }
  }

  pub fn from_err_vec(errs: Vec<SourceError>) -> Self {
    Self::Many(errs)
  }

  fn print_long(&self) -> String {
    match self {
      Self::One {
        line_no,
        token_start,
        token_end,
        line_str,
        error_msg,
        kind,
      } => {
        let spaces = line_no.to_string().chars().map(|_| ' ').collect::<String>();
        let underline_spaces = line_str[0..*token_start]
          .char_indices()
          .map(|_| ' ')
          .collect::<String>();
        let underlines = line_str[*token_start..*token_end]
          .char_indices()
          .map(|_| '~')
          .collect::<String>();
        format!(
          "{kind:?} error: {error_msg}\n{spaces} |\n{line_no} | {line_str}\n{spaces} | {underline_spaces}{underlines}\n"
        )
      }
      Self::Many(others) => {
        let mut res = String::new();
        for err in others {
          res += &err.print_long();
        }
        res
      }
    }
  }
}

impl Display for SourceError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.print_long())
  }
}

impl std::fmt::Debug for SourceError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.print_long())
  }
}
