#[derive(Clone, Copy, Debug)]
pub struct SourceInfo {
  pub line_no: u32,
  pub start: usize,
  pub end: usize,
}

impl SourceInfo {
  pub fn union(start: SourceInfo, end: SourceInfo) -> Self {
    assert!(start.line_no <= end.line_no);
    Self {
      line_no: start.line_no,
      start: start.start,
      end: end.end,
    }
  }

  // TODO: remove after its not needed
  pub fn temporary() -> Self {
    Self {
      line_no: 0,
      start: 0,
      end: 0,
    }
  }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SourceErrorType {
  Compilation,
  Runtime,
}

#[derive(PartialEq, Debug, Clone)]
pub enum SourceError {
  One {
    line_no: u32,
    start: usize,
    end: usize,
    error_msg: String,
    kind: SourceErrorType,
  },
  Many(Vec<SourceError>),
}

fn line_str(source: &str, start: usize) -> (usize, String) {
  let line_start = source
    .char_indices()
    .rev()
    .find(|(offset, c)| *c == '\n' && *offset <= start)
    .map(|(offset, _)| offset + 1) // the +1 skips \n which is 1 byte wide
    .unwrap_or(0);
  let line = source[line_start..]
    .char_indices()
    .map(|(_, c)| c)
    .take_while(|c| *c != '\n')
    .map(|c| if c.is_whitespace() { ' ' } else { c })
    .collect();
  (line_start, line)
}

impl SourceError {
  pub fn new(
    line_no: u32,
    start: usize,
    end: usize,
    error_msg: String,
    kind: SourceErrorType,
  ) -> Self {
    Self::One {
      line_no,
      start,
      end,
      error_msg,
      kind,
    }
  }

  pub fn from_source_info(info: &SourceInfo, error_msg: String, kind: SourceErrorType) -> Self {
    Self::One {
      line_no: info.line_no,
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
        start,
        end,
        error_msg,
        kind,
      } => {
        let (line_start, line_str) = line_str(source, *start);
        let end = if line_str.len() < (*end - line_start) {
          line_str.len()
        } else {
          *end - line_start
        };
        let start = *start - line_start;
        let spaces = line_no.to_string().chars().map(|_| ' ').collect::<String>();
        let underline_spaces = line_str[0..start]
          .char_indices()
          .map(|_| ' ')
          .collect::<String>();
        let underlines = line_str[start..end]
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
