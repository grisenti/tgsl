use crate::{lexer::SourceInfo, Lexer};
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
    line_start: usize,
    start: usize,
    end: usize,
    error_msg: String,
    kind: SourceErrorType,
  },
  Many(Vec<SourceError>),
}

impl SourceError {
  pub fn from_lexer_state(lex: &Lexer, error_msg: String, kind: SourceErrorType) -> Self {
    Self::One {
      line_no: lex.line_no(),
      line_start: lex.prev_token_line_start(),
      start: lex.prev_token_start(),
      end: lex.prev_token_end(),
      error_msg,
      kind,
    }
  }

  pub fn from_token_info(info: &SourceInfo, error_msg: String, kind: SourceErrorType) -> Self {
    Self::One {
      line_no: info.line_no,
      line_start: info.line_start,
      start: info.start,
      end: info.end,
      error_msg,
      kind,
    }
  }

  pub fn from_err_vec(errs: Vec<SourceError>) -> Self {
    Self::Many(errs)
  }

  pub fn print_long(&self, source: &str) -> String {
    match self {
      Self::One {
        line_no,
        line_start,
        start,
        end,
        error_msg,
        kind,
      } => {
        let line_str: String = source[*line_start..]
          .char_indices()
          .map(|(_, c)| c)
          .take_while(|c| *c != '\n')
          .collect();
        let spaces = line_no.to_string().chars().map(|_| ' ').collect::<String>();
        let underline_spaces = line_str[0..*start - *line_start]
          .char_indices()
          .map(|_| ' ')
          .collect::<String>();
        let underlines = line_str[*start - line_start..*end - line_start]
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
          res += &err.print_long(source);
        }
        res
      }
    }
  }
}
