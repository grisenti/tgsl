use std::{borrow::Cow, fmt::Debug, fmt::Write};

use super::lexer::{Lexer, SourceRange};

type ErrorMessage = Cow<'static, str>;

#[derive(Clone, PartialEq)]
pub struct CompilerError {
  code: &'static str,
  msg: ErrorMessage,
  range: SourceRange,
}

impl CompilerError {
  fn new(range: SourceRange, code: &'static str, msg: ErrorMessage) -> Self {
    Self { code, msg, range }
  }

  pub fn code(&self) -> &'static str {
    self.code
  }
}

pub type CompilerResult<T> = Result<T, CompilerError>;

pub trait SourceRangeProvider {
  fn current_range(self) -> SourceRange;
}

impl SourceRangeProvider for SourceRange {
  fn current_range(self) -> SourceRange {
    return self;
  }
}

impl SourceRangeProvider for &Lexer<'_> {
  fn current_range(self) -> SourceRange {
    self.previous_token_range()
  }
}

macro_rules! c_err {
  ($range:expr, $code:expr, $msg:expr) => {
    CompilerError::new($range, $code, $msg.into())
  };
  ($range:expr, $code:expr, $msg:expr, $($args:expr),+) => {
    CompilerError::new($range, $code, format!($msg, $($args),+).into())
  };
}

macro_rules! def_err {
    ($func_name:ident, $code:expr, $msg:expr, $($arg_name:ident : $tp:ty),+) => {
        pub fn $func_name<S: SourceRangeProvider>(provider: S, $($arg_name : $tp),+) -> CompilerError {
          c_err!(provider.current_range(), $code, $msg, $($arg_name),+)
        }
    };
    ($func_name:ident, $code:expr, $msg:expr) => {
        pub fn $func_name<S: SourceRangeProvider>(provider: S) -> CompilerError {
          c_err!(provider.current_range(), $code, $msg)
        }
    };
}

pub mod lex_err {
  use super::{CompilerError, SourceRangeProvider};

  def_err!(incomplete_string, "L001", "incomplete string");
}

pub mod parser_err {
  use super::{CompilerError, SourceRangeProvider};
  use crate::compiler::lexer::Token;

  def_err!(
    expected_identifier,
    "P001",
    "expected identifier, got {}",
    got: Token
  );

  def_err!(
    expected_token,
    "P002",
    "expected {}, got {}",
    expected: Token,
    got: Token
  );

  def_err!(unexpected_token, "P002", "expected token {}", got: Token);

  def_err!(
    expected_type_name,
    "P003",
    "expected type name, got {}",
    got: Token
  );

  def_err!(
    same_scope_name_redeclaration,
    "P004",
    "cannot redeclare name '{}' in the same scope",
    name: &str
  );

  def_err!(too_many_local_names, "P005", "too many local names");

  def_err!(
    too_many_function_parameters,
    "P006",
    "function cannot have more than 255 parameters"
  );

  def_err!(
    expected_primary,
    "P007",
    "expected literal, identifier or '(', got {}",
    got: Token
  );

  def_err!(
    too_many_function_arguments,
    "P008",
    "function cannot have more than 255 arguments"
  );

  def_err!(
    lvalue_assignment,
    "P009",
    "left hand side of assignment cannot be assigned to"
  );

  def_err!(
    import_in_local_scope,
    "P010",
    "import statements can only be in global scope"
  );

  def_err!(
    expected_module_identifier,
    "P012",
    "expected module identifier, got {}",
    got: Token
  );

  def_err!(
    extern_function_in_local_scope,
    "P013",
    "extern functions can only be declared in the global scope"
  );

  def_err!(
    module_declarations_is_not_first_statement,
    "P014",
    "module declaration needs to be the first statement"
  );

  def_err!(
    missing_initialization_at_variable_declaration,
    "P015",
    "variable declaration requires initialization"
  );
}

pub mod ty_err {
  use super::{CompilerError, SourceRangeProvider};
  use crate::compiler::lexer::Token;

  def_err!(
    type_specifier_expression_mismatch,
    "TY001",
    "specified type ({}) is different from the type of the initialization expression ({})",
    specified: String,
    expression: String
  );

  def_err!(
    incorrect_unary_operator,
    "TY002",
    "cannot apply unary operator '{}' to operand {}",
    operator: Token,
    rhs_type: String
  );

  def_err!(
    incorrect_binary_operator,
    "TY003",
    "cannot apply operator {} to operands {} and {}",
    operator: Token,
    lhs_type: String,
    rhs_type: String
  );

  def_err!(
    assignment_of_incompatible_types,
    "TY004",
    "cannot assign value of type {} to identifier of type {}",
    value_type: String,
    var_type: String
  );

  def_err!(cannot_call_type, "TY005", "cannot call type {}", t: String);

  def_err!(
    incorrect_function_argument_number,
    "TY005",
    "incorrect number of arguments for function call (required {}, provided {})",
    required: usize,
    provided: usize
  );

  def_err!(
    incorrect_function_argument_type,
    "TY006",
    "mismatched types in function call. Argument {} (of type {}) should be of type {}",
    argument_number: usize,
    provided_type: String,
    required_type: String
  );

  def_err!(
    not_a_member,
    "TY007",
    "{} is not a member of type {}",
    member_name: &str,
    object_type: String
  );

  def_err!(cannot_access_member_of_non_struct_type,
    "TY008",
    "cannot access member of non struct type {}",
    lhs_type: String
  );

  def_err!(no_member_and_no_function_found,
    "TY009",
    "{} is neither a function member for the struct nor a valid function could be found that has type {} as a first parameter",
    rhs_name: &str,
    lhs_type: String
  );

  def_err!(could_not_find_function_for_dot_call,
    "TY010",
    "could not find function '{}' that takes type '{}' as a first parameter",
    function_name: &str,
    lhs_type: String
  );

  def_err!(
    cannot_assign_to_function,
    "TY011",
    "functions cannot be assigned new values"
  );

  def_err!(
    cannot_assing_to_type,
    "TY012",
    "cannot assign a value to a type"
  );

  def_err!(incorrect_return_type,
    "TY013",
    "expression in return statement (of type {}) does not match the required type ({})",
    expression_type: String,
    required_type: String
  );

  def_err!(
    incorrect_conditional_type,
    "TY014",
    "cannot use value of type {} in a condition",
    conditional_type: String
  );

  def_err!(no_available_oveload, "TY015", "no available overload");
}

pub mod import_err {
  use super::{CompilerError, SourceRangeProvider};

  def_err!(
    not_a_loaded_module,
    "IE001",
    "'{}' is not a loaded module",
    module: &str
  );

  def_err!(
    name_redeclaration,
    "IE002",
    "name '{}' was already defined",
    name: String
  );

  def_err!(
    overload_conflict,
    "IE003",
    "redefinition of overloaded function '{}'",
    name: String
  );

  def_err!(
    module_already_declared,
    "IE004",
    "module name '{}' is already used by another module",
    name: &str
  );
}

pub mod sema_err {
  use super::{CompilerError, SourceRangeProvider};

  def_err!(
    name_already_defined,
    "SE001",
    "name {} was already defined in this scope",
    name: &str
  );

  def_err!(
    too_many_local_names,
    "SE002",
    "the current local scope has more than 255 names"
  );

  def_err!(
    name_not_found,
    "SE003",
    "identifier '{}' could not be found in the current scope",
    name: &str
  );

  def_err!(not_a_variable,
    "SE004",
    "identifier '{}' is not a variable",
    name: &str
  );

  def_err!(
    no_unconditional_return,
    "SE005",
    "function requires one unconditional return type"
  );

  def_err!(
    return_outside_of_function,
    "SE006",
    "cannot have a return statement outside of a function"
  );
}

impl Debug for CompilerError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "({} - {}) error[{}]: {}",
      self.range.start, self.range.end, self.code, self.msg
    )
  }
}

fn line_str(source: &str, start: usize) -> (usize, String) {
  let line_start = source
    .char_indices()
    .rev()
    .find(|(offset, c)| *c == '\n' && *offset <= start)
    .map(|(offset, _)| offset + 1) // the +1 skips \n which is 1 byte wide
    .unwrap_or(0);
  let line = source[line_start..]
    .chars()
    .take_while(|c| *c != '\n')
    .map(|c| if c.is_whitespace() { ' ' } else { c })
    .collect();
  (line_start, line)
}

pub struct ErrorPrinter {
  current_position: usize,
  current_line: usize,
  err_string: String,
}

impl ErrorPrinter {
  fn to_target_line(&mut self, start: usize, source: &str) {
    // FIXME: support going back
    assert!(start >= self.current_position);
    let lines_skipped = source[self.current_position..]
      .char_indices()
      .filter(|(_, c)| *c == '\n')
      .take_while(|(offset, _)| *offset <= start)
      .count();
    self.current_line += lines_skipped;
  }

  fn print_one(&mut self, err: &CompilerError, source: &str) {
    let SourceRange { start, end } = err.range;
    self.to_target_line(start, source);
    // FIXME: very inefficient
    let (line_start, line_str) = line_str(source, start);
    let end = if line_str.len() < (end - line_start) {
      line_str.len()
    } else {
      end - line_start
    };
    let start = start - line_start;
    let spaces = self
      .current_line
      .to_string()
      .chars()
      .map(|_| ' ')
      .collect::<String>();
    let underline_spaces = line_str[0..start]
      .char_indices()
      .map(|_| ' ')
      .collect::<String>();
    let underlines = line_str[start..end]
      .char_indices()
      .map(|_| '~')
      .collect::<String>();
    write!(
      self.err_string,
      "error[{}]: {}\n{spaces} |\n{} | {line_str}\n{spaces} | {underline_spaces}{underlines}\n",
      err.code, err.msg, self.current_line
    )
    .expect("write to string failed");
  }

  pub fn to_string(errs: &[CompilerError], source: &str) -> String {
    let mut printer = Self {
      current_position: 0,
      current_line: 0,
      err_string: String::new(),
    };
    for e in errs {
      printer.print_one(e, source);
    }
    printer.err_string
  }
}
