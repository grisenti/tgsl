use crate::return_if_err;

use super::*;

use ast::expression::*;

const MAX_BIN_OP_PRECEDENCE: usize = 6;

const BIN_OP_PRECEDENCE: [&[Token<'_>]; MAX_BIN_OP_PRECEDENCE] = [
  &[Token::Or, Token::Or],
  &[Token::And, Token::And],
  &[Token::Same, Token::Different],
  &[Token::Basic('<'), Token::Basic('>'), Token::Leq, Token::Geq],
  &[Token::Basic('+'), Token::Basic('-')],
  &[Token::Basic('*'), Token::Basic('/')],
];

impl<'src> Parser<'src> {
  fn parse_arguments(&mut self, call_start: SourceRange, terminator: char) -> Vec<ExprHandle> {
    return_if_err!(self, Vec::new());

    let mut arguments = Vec::new();
    while self.lookahead != Token::Basic(terminator) {
      let expr = self.parse_expression();
      arguments.push(expr);
      if self.matches_alternatives(&[Token::Basic(',')]).is_none() {
        break;
      }
    }
    let call_end = self.lex.previous_token_range();
    if arguments.len() >= 255 {
      let err = parser_err::too_many_function_arguments(SourceRange::combine(call_start, call_end));
      self.emit_error(err);
      Vec::new()
    } else {
      arguments
    }
  }

  fn parse_constructor(&mut self, type_name: &'src str, expr_start: SourceRange) -> ExprHandle {
    assert_eq!(self.lookahead, Token::Basic('{'));
    return_if_err!(self, ExprHandle::INVALID);

    let arguments_start = self.lex.previous_token_range();
    self.advance();
    let arguments = self.parse_arguments(arguments_start, '}');
    let expr_end = self.lex.previous_token_range();
    self.advance();
    self.ast.add_expression(
      expr::Construct {
        type_name,
        arguments,
      },
      SourceRange::combine(expr_start, expr_end),
    )
  }

  fn parse_id(&mut self, id: &'src str) -> ExprHandle {
    let expr_sr = self.lex.previous_token_range();
    self.advance();
    if let Token::Basic('{') = self.lookahead {
      self.parse_constructor(id, expr_sr)
    } else {
      self.ast.add_expression(expr::Id { id }, expr_sr)
    }
  }

  fn parse_paren(&mut self) -> ExprHandle {
    assert_eq!(self.lookahead, Token::Basic('('));

    let expr_start = self.lex.previous_token_range();
    self.advance();
    let inner = self.parse_expression();
    let expr_end = self.lex.previous_token_range();
    self.match_token(Token::Basic(')'));
    self.ast.add_expression(
      expr::Paren { inner },
      SourceRange::combine(expr_start, expr_end),
    )
  }

  fn parse_primary(&mut self) -> ExprHandle {
    return_if_err!(self, ExprHandle::INVALID);

    match self.lookahead {
      Token::Number(_) | Token::String(_) | Token::True | Token::False => {
        let value = self.lookahead;
        let literal_sr = self.lex.previous_token_range();
        self.advance();
        self.ast.add_expression(expr::Literal { value }, literal_sr)
      }
      Token::Id(id) => self.parse_id(id),
      Token::Basic(c) if c == '(' => self.parse_paren(),
      _ => {
        let err = parser_err::expected_primary(&self.lex, self.lookahead);
        self.emit_error(err);
        ExprHandle::INVALID
      }
    }
  }

  fn parse_function_call(&mut self, lhs: ExprHandle, expr_start: SourceRange) -> ExprHandle {
    return_if_err!(self, ExprHandle::INVALID);
    assert_eq!(self.lookahead, Token::Basic('('));

    let call_start = self.lex.previous_token_range();
    self.advance();
    let arguments = self.parse_arguments(call_start, ')');
    let expr_end = self.lex.previous_token_range();
    self.match_token(Token::Basic(')'));
    self.ast.add_expression(
      expr::FnCall {
        func: lhs,
        arguments,
      },
      SourceRange::combine(expr_start, expr_end),
    )
  }

  fn parse_dot(&mut self, lhs: ExprHandle, expr_start: SourceRange) -> ExprHandle {
    return_if_err!(self, ExprHandle::INVALID);
    assert_eq!(self.lookahead, Token::Basic('.'));

    self.advance();
    let expr_end = self.lex.previous_token_range();
    let rhs = self.match_id();
    self.ast.add_expression(
      expr::Dot { lhs, rhs },
      SourceRange::combine(expr_start, expr_end),
    )
  }

  fn parse_call(&mut self) -> ExprHandle {
    return_if_err!(self, ExprHandle::INVALID);

    let expr_start = self.lex.previous_token_range();
    let mut expr = self.parse_primary();
    // if its a unresolved overload, try to get an expression
    loop {
      return_if_err!(self, ExprHandle::INVALID);
      match self.lookahead {
        Token::Basic('(') => {
          expr = self.parse_function_call(expr, expr_start);
        }
        Token::Basic('.') => {
          expr = self.parse_dot(expr, expr_start);
        }
        _ => break,
      }
    }
    expr
  }

  fn parse_unary(&mut self) -> ExprHandle {
    return_if_err!(self, ExprHandle::INVALID);

    let expr_start = self.lex.previous_token_range();
    if let Some((operator, _)) = self.matches_alternatives(&[Token::Basic('-'), Token::Basic('!')])
    {
      let right = self.parse_call();
      let expr_end = right.get_source_range(&self.ast);
      self.ast.add_expression(
        expr::Unary { operator, right },
        SourceRange::combine(expr_start, expr_end),
      )
    } else {
      self.parse_call()
    }
  }

  fn parse_binary_operation(&mut self, prec: usize) -> ExprHandle {
    return_if_err!(self, ExprHandle::INVALID);

    if prec == MAX_BIN_OP_PRECEDENCE {
      self.parse_unary()
    } else {
      let expr_start = self.lex.previous_token_range();
      let mut expr = self.parse_binary_operation(prec + 1);
      while let Some((operator, _)) = self.matches_alternatives(BIN_OP_PRECEDENCE[prec]) {
        let right = self.parse_binary_operation(prec + 1);
        let expr_end = right.get_source_range(&self.ast);
        expr = self.ast.add_expression(
          expr::Binary {
            left: expr,
            operator,
            right,
          },
          SourceRange::combine(expr_start, expr_end),
        );
      }
      expr
    }
  }

  fn parse_assignment(&mut self) -> ExprHandle {
    return_if_err!(self, ExprHandle::INVALID);

    let expr_start = self.lex.previous_token_range();
    let lhs = self.parse_binary_operation(0);
    if self.match_next(Token::Basic('=')).is_some() {
      let rhs = self.parse_binary_operation(0);
      let expr_end = rhs.get_source_range(&self.ast);
      self.ast.add_expression(
        expr::Assignment { lhs, rhs },
        SourceRange::combine(expr_start, expr_end),
      )
    } else {
      lhs
    }
  }

  fn parse_lambda(&mut self) -> ExprHandle {
    return_if_err!(self, ExprHandle::INVALID);
    assert_eq!(self.lookahead, Token::Fn);

    let expr_start = self.lex.previous_token_range();
    self.advance();
    let (parameter_types, parameter_names) = self.parse_function_parameters();
    let return_type = self.parse_function_return_type();
    let (body, expr_end) = self.parse_block_components();

    self.ast.add_expression(
      expr::Lambda {
        parameter_types,
        parameter_names,
        body,
        return_type: return_type.clone(),
      },
      SourceRange::combine(expr_start, expr_end),
    )
  }

  pub fn parse_expression(&mut self) -> ExprHandle {
    return_if_err!(self, ExprHandle::INVALID);

    match self.lookahead {
      Token::Fn => self.parse_lambda(),
      _ => self.parse_assignment(),
    }
  }
}

#[cfg(test)]
mod test {
  use json::JsonValue;

  use crate::compiler::parser::test::TestParser;
  use crate::compiler::types::FunctionSignature;
  use crate::compiler::types::Type;

  fn parse_correct_expression(expr: &'static str) -> JsonValue {
    TestParser::new(expr).parse_correct_expression()
  }

  #[test]
  fn literal_string() {
    let literal = parse_correct_expression("\"str\"");
    assert_eq!(literal["LiteralString"]["value"], "str");
  }

  #[test]
  fn literal_num() {
    let literal = parse_correct_expression("1");
    assert_eq!(literal["LiteralNumber"]["value"], "1");
  }

  #[test]
  fn literal_bool() {
    let literal = parse_correct_expression("true");
    assert_eq!(literal["LiteralBool"]["value"], true);
  }

  #[test]
  fn identifier_type() {
    let id = TestParser::new("x")
      .declare_name("x", Type::Str)
      .parse_correct_expression();
    assert_eq!(Type::Str, id["Id"]["id_type"]);
  }

  #[test]
  fn literal_in_parenthesis() {
    let literal = parse_correct_expression("(1)");
    assert_eq!(literal["Paren"]["expr"]["LiteralNumber"]["value"], "1");
  }

  #[test]
  fn assignment() {}

  #[test]
  fn assign_to_lvalue_error() {
    let err = TestParser::new("1 = 2").parse_expression_error();
    assert_eq!(err.code(), "P009");
  }

  #[test]
  fn binary_op() {
    let bin_op = parse_correct_expression("1 + 1");
    let bin_op = &bin_op["Binary"];
    assert_eq!(bin_op["operator"], "+");
    assert_eq!(bin_op["left"]["LiteralNumber"]["value"], "1");
    assert_eq!(bin_op["right"]["LiteralNumber"]["value"], "1");
  }

  #[test]
  fn logical_op() {
    let logical_op = parse_correct_expression("true and false");
    let logical_op = &logical_op["Logical"];
    assert_eq!(logical_op["operator"], "and");
    assert_eq!(logical_op["left"]["LiteralBool"]["value"], true);
    assert_eq!(logical_op["right"]["LiteralBool"]["value"], false);
  }

  #[test]
  fn parse_empty_function_call() {
    let call = TestParser::new("main()")
      .declare_name("main", FunctionSignature::new(vec![], Type::Str).into())
      .parse_correct_expression();
    let call = &call["FnCall"];
    assert!(!call["function"]["Id"].is_null());
    assert_eq!(call["arguments"], JsonValue::Array(vec![]));
    assert_eq!(Type::Str, call["expr_type"]);
  }

  #[test]
  fn parse_empty_dot_call_on_number() {
    let call = TestParser::new("1.op()")
      .declare_name(
        "op",
        FunctionSignature::new(vec![Type::Num], Type::Nothing).into(),
      )
      .parse_correct_expression();
    let call = &call["DotCall"];
    assert!(!call.is_null());
    assert_eq!(call["lhs"]["LiteralNumber"]["value"], "1");
    assert!(call["arguments"].is_empty());
  }

  #[test]
  fn parse_dot_call_on_number_with_arguments() {
    let call = TestParser::new("1.op(\"1234string\")")
      .declare_name(
        "op",
        FunctionSignature::new(vec![Type::Num, Type::Str], Type::Nothing).into(),
      )
      .parse_correct_expression();
    let call = &call["DotCall"];
    assert!(!call.is_null());
    assert_eq!(call["lhs"]["LiteralNumber"]["value"], "1");
    assert_eq!(call["arguments"].len(), 1);
    assert_eq!(call["arguments"][0]["LiteralString"]["value"], "1234string");
  }

  #[test]
  fn parse_function_call_with_arguments() {
    let call = TestParser::new("main(1, 2, \"str\")")
      .declare_name(
        "main",
        FunctionSignature::new(vec![Type::Num, Type::Num, Type::Str], Type::Num).into(),
      )
      .parse_correct_expression();
    let call = &call["FnCall"];
    assert!(!call["function"]["Id"].is_null());
    assert_eq!(call["arguments"].len(), 3);
    assert_eq!(Type::Num, call["expr_type"]);
  }

  #[test]
  fn parse_empty_constructor() {
    let constructor = TestParser::new("A{}")
      .define_struct("A", vec![], vec![])
      .parse_correct_expression();
    assert!(!constructor["Constructor"].is_null());
    assert!(constructor["Constructor"]["arguments"].is_empty());
  }

  #[test]
  fn parse_non_empty_constructor() {
    let constructor = TestParser::new("A{1, \"hello\"}")
      .define_struct("A", vec!["a", "b"], vec![Type::Num, Type::Str])
      .parse_correct_expression();
    let constructor = &constructor["Constructor"];
    assert!(!constructor.is_null());
    assert_eq!(constructor["arguments"].len(), 2);
    assert_eq!(constructor["arguments"][0]["LiteralNumber"]["value"], "1");
    assert_eq!(
      constructor["arguments"][1]["LiteralString"]["value"],
      "hello"
    );
  }
}
