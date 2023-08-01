use crate::{compiler::errors::ty_err, return_if_err};

use super::*;
use ast::expression::*;

const MAX_BIN_OP_PRECEDENCE: usize = 4;

const BIN_OP_PRECEDENCE: [&[Token<'_>]; MAX_BIN_OP_PRECEDENCE] = [
  &[Token::Same, Token::Different],
  &[Token::Basic('<'), Token::Basic('>'), Token::Leq, Token::Geq],
  &[Token::Basic('+'), Token::Basic('-')],
  &[Token::Basic('*'), Token::Basic('/')],
];

const MAX_LOGICAL_OP_PRECEDENCE: usize = 2;

const LOGICAL_OP_PRECEDENCE: [&[Token<'_>]; MAX_LOGICAL_OP_PRECEDENCE] =
  [&[Token::Or, Token::Or], &[Token::And, Token::And]];

pub struct ParsedExpression {
  pub handle: ExprHandle,
  pub type_id: TypeId,
}

impl ParsedExpression {
  const INVALID: Self = Self {
    handle: ExprHandle::INVALID,
    type_id: TypeId::ERROR,
  };
}

impl<'src> Parser<'src> {
  fn parse_primary(&mut self) -> ParsedExpression {
    return_if_err!(self, ParsedExpression::INVALID);

    match self.lookahead {
      Token::Number(num) => {
        let value_sr = self.lex.previous_token_range();
        self.advance();
        ParsedExpression {
          handle: self.ast.add_expression(expr::LiteralNumber {
            value: num,
            value_sr,
          }),
          type_id: TypeId::NUM,
        }
      }
      Token::String(str) => {
        let value_sr = self.lex.previous_token_range();
        self.advance();
        let handle = self.ast.add_str(str);
        ParsedExpression {
          handle: self
            .ast
            .add_expression(expr::LiteralString { handle, value_sr }),
          type_id: TypeId::STR,
        }
      }
      Token::True | Token::False => {
        let value = self.lookahead == Token::True;
        let value_sr = self.lex.previous_token_range();
        self.advance();
        ParsedExpression {
          handle: self
            .ast
            .add_expression(expr::LiteralBool { value, value_sr }),
          type_id: TypeId::BOOL,
        }
      }
      Token::Id(id) => {
        let id_sr = self.lex.previous_token_range();
        let (id, id_type) = self.get_id(id, id_sr);
        self.advance();
        ParsedExpression {
          handle: self.ast.add_expression(expr::Id { id, id_type, id_sr }),
          type_id: id_type,
        }
      }
      Token::Basic(c) if c == '(' => {
        self.advance();
        let parsed_expression = self.parse_expression();
        self.match_or_err(Token::Basic(')'));
        ParsedExpression {
          handle: self.ast.add_expression(expr::Paren {
            inner: parsed_expression.handle,
          }),
          type_id: parsed_expression.type_id,
        }
      }
      _ => {
        let err = parser_err::expected_primary(&self.lex, self.lookahead);
        self.emit_error(err);
        ParsedExpression::INVALID
      }
    }
  }

  fn parse_arguments(&mut self, call_start: SourceRange) -> Vec<ParsedExpression> {
    return_if_err!(self, vec![]);
    let mut arguments = Vec::new();
    loop {
      arguments.push(self.parse_expression());
      if self.matches_alternatives(&[Token::Basic(',')]).is_none() {
        break;
      }
    }
    if arguments.len() >= 255 {
      let err = parser_err::too_many_function_arguments(call_start);
      self.emit_error(err);
      vec![]
    } else {
      arguments
    }
  }

  fn parse_call(&mut self) -> ParsedExpression {
    return_if_err!(self, ParsedExpression::INVALID);
    /*
    let ParsedExpression {
      type_id,
      handle: mut expr,
    } =
    */
    self.parse_primary()

    /*
    loop {
      match self.lookahead {
        Token::Basic('(') => {
          let call_start_sr = self.lex.previous_token_range();
          self.advance();
          let arguments = if self.lookahead != Token::Basic(')') {
            self.parse_arguments(call_start_sr)
          } else {
            vec![]
          };
          let call_end_sr = self.lex.previous_token_range();
          self.match_or_err(Token::Basic(')'));
          let call_sr = SourceRange::combine(call_start_sr, call_end_sr);
          expr = self.ast.add_expression(expr::FnCall {
            func: expr,
            call_sr,
            arguments: arguments.into_iter().map(|expr| expr.handle).collect(),
            expr_type: TypeId::UNKNOWN,
          })
        }
        Token::Basic('.') => {
          self.advance();
          if let Token::Id(rhs_name) = self.lookahead {
            let rhs_sr = self.lex.previous_token_range();
            let (rhs_id, rhs_type) = self.get_opt_name(rhs_name, rhs_sr);
            self.advance();
            let rhs_name = self.ast.add_str(rhs_name);
            expr = self.ast.add_expression(expr::Dot {
              lhs: expr,
              rhs_id,
              rhs_name,
              rhs_sr,
            });
          }
        }
        _ => break,
      }
    }
        ParsedExpression {
      type_id: TypeId::UNKNOWN,
      handle: expr,
    }
    */
  }

  fn check_unary(&mut self, sr: SourceRange, op: Operator, rhs: TypeId) -> TypeId {
    const OPERATORS: &[(Operator, TypeId, TypeId)] = &[
      (Operator::Basic('-'), TypeId::NUM, TypeId::NUM),
      (Operator::Basic('!'), TypeId::BOOL, TypeId::BOOL),
    ];
    let expr_type = OPERATORS
      .iter()
      .find(|e| e.0 == op && e.1 == rhs)
      .map(|e| e.2);
    if let Some(expr_type) = expr_type {
      expr_type
    } else {
      self.emit_error(ty_err::incorrect_unary_operator(
        sr,
        op,
        self.type_map.type_to_string(rhs),
      ));
      TypeId::ERROR
    }
  }

  fn parse_unary(&mut self) -> ParsedExpression {
    return_if_err!(self, ParsedExpression::INVALID);

    if let Some((op, op_sr)) = self.matches_alternatives(&[Token::Basic('-'), Token::Basic('!')]) {
      let right = self.parse_call();
      let operator = to_operator(op);
      let expr_type = self.check_unary(op_sr, operator, right.type_id);
      ParsedExpression {
        handle: self.ast.add_expression(expr::Unary {
          operator: to_operator(op),
          operator_sr: op_sr,
          right: right.handle,
          expr_type,
        }),
        type_id: expr_type,
      }
    } else {
      self.parse_call()
    }
  }

  fn check_binary(&mut self, sr: SourceRange, op: Operator, lhs: TypeId, rhs: TypeId) -> TypeId {
    const OPERATORS: &[(Operator, TypeId, TypeId, TypeId)] = &[
      // number
      (Operator::Basic('+'), TypeId::NUM, TypeId::NUM, TypeId::NUM),
      (Operator::Basic('-'), TypeId::NUM, TypeId::NUM, TypeId::NUM),
      (Operator::Basic('*'), TypeId::NUM, TypeId::NUM, TypeId::NUM),
      (Operator::Basic('/'), TypeId::NUM, TypeId::NUM, TypeId::NUM),
      (Operator::Basic('<'), TypeId::NUM, TypeId::NUM, TypeId::BOOL),
      (Operator::Basic('>'), TypeId::NUM, TypeId::NUM, TypeId::BOOL),
      (Operator::Leq, TypeId::NUM, TypeId::NUM, TypeId::BOOL),
      (Operator::Geq, TypeId::NUM, TypeId::NUM, TypeId::BOOL),
      (Operator::Same, TypeId::NUM, TypeId::NUM, TypeId::BOOL),
      (Operator::Different, TypeId::NUM, TypeId::NUM, TypeId::BOOL),
      // string operator
      (Operator::Basic('+'), TypeId::STR, TypeId::STR, TypeId::STR),
      (Operator::Basic('<'), TypeId::STR, TypeId::STR, TypeId::BOOL),
      (Operator::Basic('>'), TypeId::STR, TypeId::STR, TypeId::BOOL),
      (Operator::Leq, TypeId::STR, TypeId::STR, TypeId::BOOL),
      (Operator::Geq, TypeId::STR, TypeId::STR, TypeId::BOOL),
      (Operator::Same, TypeId::STR, TypeId::STR, TypeId::BOOL),
      (Operator::Different, TypeId::STR, TypeId::STR, TypeId::BOOL),
    ];

    let expr_type = OPERATORS
      .iter()
      .filter(|bin_op| bin_op.0 == op)
      .find(|bin_op| bin_op.1 == lhs && bin_op.2 == rhs)
      .map(|e| e.3);
    if let Some(expr_type) = expr_type {
      expr_type
    } else {
      self.emit_error(ty_err::incorrect_binary_operator(
        sr,
        op,
        self.type_map.type_to_string(lhs),
        self.type_map.type_to_string(rhs),
      ));
      TypeId::ERROR
    }
  }

  fn parse_binary_operation(&mut self, prec: usize) -> ParsedExpression {
    return_if_err!(self, ParsedExpression::INVALID);

    if prec == MAX_BIN_OP_PRECEDENCE {
      self.parse_unary()
    } else {
      let ParsedExpression {
        handle: mut expr,
        type_id: mut expr_type,
      } = self.parse_binary_operation(prec + 1);
      while let Some((op, op_sr)) = self.matches_alternatives(BIN_OP_PRECEDENCE[prec]) {
        let right = self.parse_binary_operation(prec + 1);
        let operator = to_operator(op);
        expr_type = self.check_binary(op_sr, operator, expr_type, right.type_id);
        expr = self.ast.add_expression(expr::Binary {
          left: expr,
          operator: to_operator(op),
          operator_sr: op_sr,
          right: right.handle,
          expr_type: TypeId::UNKNOWN,
        })
      }
      ParsedExpression {
        handle: expr,
        type_id: expr_type,
      }
    }
  }

  fn check_logical(&mut self, sr: SourceRange, op: Operator, lhs: TypeId, rhs: TypeId) -> TypeId {
    const OPERATORS: &[(Operator, TypeId, TypeId, TypeId)] = &[
      (Operator::And, TypeId::BOOL, TypeId::BOOL, TypeId::BOOL),
      (Operator::Or, TypeId::BOOL, TypeId::BOOL, TypeId::BOOL),
    ];

    let expr_type = OPERATORS
      .iter()
      .find(|e| e.0 == op && e.1 == lhs && e.2 == rhs)
      .map(|e| e.3);
    if let Some(expr_type) = expr_type {
      expr_type
    } else {
      self.emit_error(ty_err::incorrect_binary_operator(
        sr,
        op,
        self.type_map.type_to_string(lhs),
        self.type_map.type_to_string(rhs),
      ));
      TypeId::ERROR
    }
  }

  fn parse_logical_operation(&mut self, prec: usize) -> ParsedExpression {
    return_if_err!(self, ParsedExpression::INVALID);

    if prec == MAX_LOGICAL_OP_PRECEDENCE {
      self.parse_binary_operation(0)
    } else {
      let ParsedExpression {
        handle: mut expr,
        type_id: mut expr_type,
      } = self.parse_logical_operation(prec + 1);
      while let Some((op, op_sr)) = self.matches_alternatives(LOGICAL_OP_PRECEDENCE[prec]) {
        let right = self.parse_logical_operation(prec + 1);
        let operator = to_operator(op);
        expr_type = self.check_logical(op_sr, operator, expr_type, right.type_id);
        expr = self.ast.add_expression(expr::Logical {
          left: expr,
          operator: to_operator(op),
          operator_sr: op_sr,
          right: right.handle,
          expr_type: expr_type,
        })
      }
      ParsedExpression {
        handle: expr,
        type_id: expr_type,
      }
    }
  }

  fn check_assignment(&mut self, id: expr::Id, value: ParsedExpression) -> ParsedExpression {
    if let Identifier::Variable(var_id) = id.id {
      if id.id_type == value.type_id {
        ParsedExpression {
          handle: self.ast.add_expression(expr::Assignment {
            id: var_id,
            id_sr: id.id_sr,
            value: value.handle,
            type_id: id.id_type,
          }),
          type_id: id.id_type,
        }
      } else {
        self.emit_error(ty_err::assignment_of_incompatible_types(
          id.id_sr,
          self.type_map.type_to_string(id.id_type),
          self.type_map.type_to_string(value.type_id),
        ));
        ParsedExpression::INVALID
      }
    } else {
      // FIXME: make this into a proper error
      panic!("cannot assing to non-variable")
    }
  }

  fn parse_assignment(&mut self) -> ParsedExpression {
    return_if_err!(self, ParsedExpression::INVALID);

    let ParsedExpression {
      handle: lhs,
      type_id,
    } = self.parse_logical_operation(0);
    if let Some((_, eq_src_rc)) = self.match_next(Token::Basic('=')) {
      let rhs = self.parse_logical_operation(0);
      // TODO: maybe consider making simple nodes copy
      match lhs.get(&self.ast).clone() {
        Expr::Id(id) => self.check_assignment(id, rhs),
        Expr::Dot(expr::Dot {
          lhs: object,
          rhs_name: name,
          rhs_sr,
          ..
        }) => ParsedExpression {
          handle: self.ast.add_expression(expr::Set {
            object,
            member_name: name,
            member_name_sr: rhs_sr,
            value: rhs.handle,
          }),
          type_id: TypeId::UNKNOWN,
        },
        _ => {
          let err = parser_err::lvalue_assignment(eq_src_rc);
          self.emit_error(err);
          ParsedExpression::INVALID
        }
      }
    } else {
      ParsedExpression {
        handle: lhs,
        type_id,
      }
    }
  }

  fn parse_lambda(&mut self) -> ParsedExpression {
    return_if_err!(self, ParsedExpression::INVALID);

    let parameters_sr_start = self.lex.previous_token_range();
    self.env.push_function();
    self.match_or_err(Token::Basic('('));
    let parameters = if self.lookahead != Token::Basic(')') {
      self.parse_function_params(parameters_sr_start)
    } else {
      Vec::new()
    };
    self.match_or_err(Token::Basic(')'));
    let return_type = self.parse_function_return_type();
    let parameters_sr_end = self.lex.previous_token_range();
    let body = self.parse_unscoped_block();
    let captures = self.env.pop_function();
    let parameter_types = parameters;
    let function_type_id = self.type_map.get_or_add(Type::Function {
      parameters: parameter_types.clone(),
      ret: return_type,
    });
    let parameters_sr = SourceRange::combine(parameters_sr_start, parameters_sr_end);

    ParsedExpression {
      handle: self.ast.add_expression(expr::Lambda {
        parameters_sr,
        captures,
        parameter_types,
        body,
        function_type_id,
        return_type,
      }),
      type_id: function_type_id,
    }
  }

  pub(super) fn parse_expression(&mut self) -> ParsedExpression {
    return_if_err!(self, ParsedExpression::INVALID);

    if self.match_next(Token::Fn).is_some() {
      self.parse_lambda()
    } else {
      self.parse_assignment()
    }
  }
}

#[cfg(disable)]
mod test {
  use json::{array, JsonValue};

  use crate::compiler::{
    ast::{json::ASTJSONPrinter, visitor::ExprVisitor, AST},
    errors::CompilerError,
    global_env::GlobalEnv,
    lexer::{Lexer, SourceRange, Token},
    parser::{environment::Environment, Parser, ParserState},
    types::{type_map::TypeMap, TypeId},
  };

  fn parse_expression_with_declared_names(
    expr: &str,
    names: &[&str],
  ) -> Result<JsonValue, Vec<CompilerError>> {
    let mut empty_type_map = TypeMap::new();
    let mut empty_global_env = GlobalEnv::new();
    let mut env = Environment::new(&mut empty_global_env);
    for name in names {
      env
        .define_variable(name, SourceRange::EMPTY)
        .expect("name redeclarations");
    }
    let mut parser = Parser {
      lex: Lexer::new(expr),
      type_map: &mut empty_type_map,
      lookahead: Token::EndOfFile,
      ast: AST::new(),
      env,
      errors: Vec::new(),
      state: ParserState::NoErrors,
    };
    parser.advance();
    let expr = parser.parse_expression();
    if !parser.errors.is_empty() {
      Err(parser.errors)
    } else {
      let mut printer = ASTJSONPrinter {};
      Ok(printer.visit_expr(&parser.ast, expr.handle))
    }
  }

  fn parse_expression(expr: &str) -> Result<JsonValue, Vec<CompilerError>> {
    parse_expression_with_declared_names(expr, &[])
  }

  #[test]
  fn literal_num() {
    let literal = parse_expression("1").expect("parsing error");
    assert_eq!(literal["LiteralNumber"]["value"], "1");
  }

  #[test]
  fn literal_string() {
    let literal = parse_expression("\"str\"").expect("parsing error");
    assert_eq!(literal["LiteralString"]["value"], "\"str\"");
  }

  #[test]
  fn literal_in_parenthesis() {
    let literal = parse_expression("(1)").expect("parsing error");
    assert_eq!(literal["Paren"]["expr"]["LiteralNumber"]["value"], "1");
  }

  #[test]
  fn parse_empty_function_call() {
    let call = parse_expression_with_declared_names("main()", &["main"]).expect("parsing error");
    assert!(!call["FnCall"]["function"]["Variable"].is_null());
    assert_eq!(call["FnCall"]["arguments"], JsonValue::Array(vec![]));
  }

  #[test]
  fn parse_function_call_with_arguments() {
    let call = parse_expression_with_declared_names("main(1, 1 + 1, \"hello\")", &["main"])
      .expect("parsing error");
    assert!(!call["FnCall"]["function"]["Variable"].is_null());
    assert_eq!(call["FnCall"]["arguments"].len(), 3);
  }

  #[test]
  fn parse_dot() {
    let dot_expr =
      parse_expression_with_declared_names("object.member", &["object"]).expect("parsing error");
    assert!(!dot_expr["Dot"]["lhs"]["Variable"].is_null());
    assert_eq!(dot_expr["Dot"]["rhs_id"], JsonValue::Null);
  }

  #[test]
  fn binary_op() {
    let bin_op = parse_expression("1 + 1").expect("parsing error");
    assert_eq!(bin_op["Binary"]["operator"], "+");
    assert_eq!(bin_op["Binary"]["left"]["Literal"]["value"], "1");
    assert_eq!(bin_op["Binary"]["right"]["Literal"]["value"], "1");
  }

  #[test]
  fn logical_op() {
    let logical_op = parse_expression("1 and 1").expect("parsing error");
    assert_eq!(logical_op["Logical"]["operator"], "and");
    assert_eq!(logical_op["Logical"]["left"]["LiteralNumber"]["value"], "1");
    assert_eq!(
      logical_op["Logical"]["right"]["LiteralNumber"]["value"],
      "1"
    );
  }

  #[test]
  fn assign_to_lvalue_error() {
    let err = parse_expression("1 = 2")
      .expect_err("can assign to lvalue")
      .first()
      .unwrap()
      .clone();
    assert_eq!(err.code(), "P009");
  }

  #[test]
  fn assign_to_rvalue() {
    let assignment =
      parse_expression_with_declared_names("id = 2", &["id"]).expect("parsing error");
    assert_eq!(
      assignment["Assignment"]["value"]["LiteralNumber"]["value"],
      "2"
    );
  }

  #[test]
  fn parse_identifier() {
    let identifier =
      parse_expression_with_declared_names("identifier", &["identifier"]).expect("parsing error");
    assert!(!identifier["Variable"]["id"].is_null());
  }

  #[test]
  fn operator_precedece() {
    let expr_no_parens = parse_expression("1 * -1 + 1 < 1 == 1").expect("parsing error");
    let expr_parens = parse_expression("(((1 * -1) + 1) < 1) == 1").expect("parsing error");
    assert_eq!(expr_no_parens, expr_parens);
  }

  #[test]
  fn parse_lambda_no_captures() {
    let lambda = parse_expression("fn (n: num, s: str) -> num {}").expect("parsing error");
    assert_eq!(lambda["Lambda"]["captures"], JsonValue::Array(vec![]));
    assert_eq!(
      lambda["Lambda"]["return_type"],
      format!("{}", TypeId::NUM.0)
    );
    assert_eq!(
      lambda["Lambda"]["parameter_types"],
      array![TypeId::NUM, TypeId::STR]
    );
  }
}
