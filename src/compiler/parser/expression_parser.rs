use crate::{compiler::errors::ty_err, return_if_err};

use super::*;
use crate::compiler::ast::expression::expr::DotCall;
use crate::compiler::types::FunctionSignature;
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
  pub type_: Type,
}

impl ParsedExpression {
  const INVALID: Self = Self {
    handle: ExprHandle::INVALID,
    type_: Type::Error,
  };
}

enum MemberGetResult {
  NotAStruct,
  NotAMember,
  Valid(ParsedExpression),
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
          type_: Type::Num,
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
          type_: Type::Str,
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
          type_: Type::Bool,
        }
      }
      Token::Id(id) => {
        let id_sr = self.lex.previous_token_range();
        let (id, id_type) = self.get_id(id, id_sr);
        let id_type = id_type.clone();
        self.advance();
        ParsedExpression {
          handle: self.ast.add_expression(expr::Id {
            id,
            id_type: id_type.clone(),
            id_sr,
          }),
          type_: id_type,
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
          type_: parsed_expression.type_,
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

  fn check_arguments(
    &mut self,
    parameters: &[Type],
    arguments: &[ParsedExpression],
    call_sr: SourceRange,
  ) -> Vec<ExprHandle> {
    if parameters.len() != arguments.len() {
      self.emit_error(ty_err::incorrect_function_argument_number(
        call_sr,
        parameters.len(),
        arguments.len(),
      ));
      return vec![];
    }

    let mut invalid_call = false;
    let mut argument_exprs = Vec::new();
    for (index, (param, argument)) in parameters.iter().zip(arguments).enumerate() {
      if *param != argument.type_ {
        invalid_call = true;
        self.emit_error(ty_err::incorrect_function_argument_type(
          call_sr,
          index + 1,
          argument.type_.print_pretty(),
          param.print_pretty(),
        ));
      } else {
        argument_exprs.push(argument.handle);
      }
    }
    if invalid_call {
      vec![]
    } else {
      argument_exprs
    }
  }

  fn parse_function_call(&mut self, expr: ParsedExpression) -> ParsedExpression {
    return_if_err!(self, ParsedExpression::INVALID);
    assert_eq!(self.lookahead, Token::Basic('('));

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

    match expr.type_ {
      Type::Function(signature) => {
        let (parameters, return_type) = signature.into_parts();
        let arguments = self.check_arguments(&parameters, &arguments, call_sr);
        let handle = self.ast.add_expression(expr::FnCall {
          func: expr.handle,
          call_sr,
          arguments,
          expr_type: return_type.clone(),
        });
        ParsedExpression {
          handle,
          type_: return_type,
        }
      }
      Type::Struct(id) => {
        let s = self.env.get_struct(id).unwrap();
        let member_types = s.get_member_types().to_vec(); // FIXME: REMOVE THIS!!!
        let arguments = self.check_arguments(&member_types, &arguments, call_sr);
        let handle = self.ast.add_expression(expr::FnCall {
          func: expr.handle,
          call_sr,
          arguments,
          expr_type: Type::Struct(id),
        });
        ParsedExpression {
          handle,
          type_: Type::Struct(id),
        }
      }
      _ => {
        self.emit_error(ty_err::cannot_call_type(call_sr, expr.type_.print_pretty()));
        ParsedExpression::INVALID
      }
    }
  }

  fn try_parse_member_get(
    &mut self,
    expr: &ParsedExpression,
    member_name: &str,
    member_name_sr: SourceRange,
  ) -> MemberGetResult {
    if let Type::Struct(struct_id) = expr.type_ {
      let s = self.env.get_struct(struct_id).unwrap();
      if let Some(member_index) = s.get_member_index(member_name) {
        let member_type = s.member_info(member_index).1.clone();
        let handle = self.ast.add_expression(expr::MemberGet {
          lhs: expr.handle,
          rhs_sr: member_name_sr,
          member_index,
        });
        MemberGetResult::Valid(ParsedExpression {
          handle,
          type_: member_type,
        })
      } else {
        MemberGetResult::NotAMember
      }
    } else {
      MemberGetResult::NotAStruct
    }
  }

  fn try_resolve_dot_call(
    &mut self,
    expr: ParsedExpression,
    name: &str,
    name_sr: SourceRange,
    member_get_result: MemberGetResult,
  ) -> ParsedExpression {
    if let Token::Basic('(') = self.lookahead {
      let (id, id_type) = self.get_id(name, name_sr);
      if let Type::Function(signature) = id_type {
        let (parameters, return_type) = signature.into_parts();
        let call_start = self.lex.previous_token_range();
        self.advance();
        if parameters.first().is_some_and(|p1| *p1 == expr.type_) {
          let parsed_arguments = self.parse_arguments(call_start);
          let call_end = self.lex.previous_token_range();
          let call_sr = SourceRange::combine(call_start, call_end);
          let arguments =
            self.check_arguments(&parameters[1..parameters.len()], &parsed_arguments, call_sr);
          let handle = self.ast.add_expression(DotCall {
            function: id,
            lhs: expr.handle,
            arguments,
            call_sr,
          });
          ParsedExpression {
            handle,
            type_: return_type,
          }
        } else {
          panic!()
        }
      } else {
        panic!()
      }
    } else if let MemberGetResult::NotAMember = member_get_result {
      panic!();
    } else
    /* member_get_result == MemberGetResult::NotAStruct*/
    {
      panic!()
    }
  }

  fn parse_dot(&mut self, expr: ParsedExpression) -> ParsedExpression {
    return_if_err!(self, ParsedExpression::INVALID);
    assert_eq!(self.lookahead, Token::Basic('.'));
    // if its lhs is an object and name is a member, get the member
    // if name is a function, try to call the function

    self.advance();
    let (name, name_sr) = self.match_id_or_err();
    match self.try_parse_member_get(&expr, name, name_sr) {
      MemberGetResult::Valid(result) => result,
      other => self.try_resolve_dot_call(expr, name, name_sr, other),
    }
  }

  fn parse_call(&mut self) -> ParsedExpression {
    return_if_err!(self, ParsedExpression::INVALID);

    let mut expr = self.parse_primary();

    loop {
      match self.lookahead {
        Token::Basic('(') => {
          expr = self.parse_function_call(expr);
        }
        Token::Basic('.') => {
          expr = self.parse_dot(expr);
        }
        _ => break,
      }
    }
    expr
  }

  fn check_unary(&mut self, sr: SourceRange, op: Operator, rhs: Type) -> Type {
    const OPERATORS: &[(Operator, Type, Type)] = &[
      (Operator::Basic('-'), Type::Num, Type::Num),
      (Operator::Basic('!'), Type::Bool, Type::Bool),
    ];
    let expr_type = OPERATORS
      .iter()
      .find(|e| e.0 == op && e.1 == rhs)
      .map(|e| e.2.clone());
    if let Some(expr_type) = expr_type {
      expr_type
    } else {
      self.emit_error(ty_err::incorrect_unary_operator(sr, op, rhs.print_pretty()));
      Type::Error
    }
  }

  fn parse_unary(&mut self) -> ParsedExpression {
    return_if_err!(self, ParsedExpression::INVALID);

    if let Some((op, op_sr)) = self.matches_alternatives(&[Token::Basic('-'), Token::Basic('!')]) {
      let right = self.parse_call();
      let operator = to_operator(op);
      let expr_type = self.check_unary(op_sr, operator, right.type_);
      ParsedExpression {
        handle: self.ast.add_expression(expr::Unary {
          operator: to_operator(op),
          operator_sr: op_sr,
          right: right.handle,
          expr_type: expr_type.clone(),
        }),
        type_: expr_type,
      }
    } else {
      self.parse_call()
    }
  }

  fn check_binary(&mut self, sr: SourceRange, op: Operator, lhs: &Type, rhs: &Type) -> Type {
    const OPERATORS: &[(Operator, Type, Type, Type)] = &[
      // number
      (Operator::Basic('+'), Type::Num, Type::Num, Type::Num),
      (Operator::Basic('-'), Type::Num, Type::Num, Type::Num),
      (Operator::Basic('*'), Type::Num, Type::Num, Type::Num),
      (Operator::Basic('/'), Type::Num, Type::Num, Type::Num),
      (Operator::Basic('<'), Type::Num, Type::Num, Type::Bool),
      (Operator::Basic('>'), Type::Num, Type::Num, Type::Bool),
      (Operator::Leq, Type::Num, Type::Num, Type::Bool),
      (Operator::Geq, Type::Num, Type::Num, Type::Bool),
      (Operator::Same, Type::Num, Type::Num, Type::Bool),
      (Operator::Different, Type::Num, Type::Num, Type::Bool),
      // string operator
      (Operator::Basic('+'), Type::Str, Type::Str, Type::Str),
      (Operator::Basic('<'), Type::Str, Type::Str, Type::Bool),
      (Operator::Basic('>'), Type::Str, Type::Str, Type::Bool),
      (Operator::Leq, Type::Str, Type::Str, Type::Bool),
      (Operator::Geq, Type::Str, Type::Str, Type::Bool),
      (Operator::Same, Type::Str, Type::Str, Type::Bool),
      (Operator::Different, Type::Str, Type::Str, Type::Bool),
    ];

    let expr_type = OPERATORS
      .iter()
      .filter(|bin_op| bin_op.0 == op)
      .find(|bin_op| bin_op.1 == *lhs && bin_op.2 == *rhs)
      .map(|e| e.3.clone());
    if let Some(expr_type) = expr_type {
      expr_type
    } else {
      self.emit_error(ty_err::incorrect_binary_operator(
        sr,
        op,
        lhs.print_pretty(),
        rhs.print_pretty(),
      ));
      Type::Error
    }
  }

  fn parse_binary_operation(&mut self, prec: usize) -> ParsedExpression {
    return_if_err!(self, ParsedExpression::INVALID);

    if prec == MAX_BIN_OP_PRECEDENCE {
      self.parse_unary()
    } else {
      let ParsedExpression {
        handle: mut expr,
        type_: mut expr_type,
      } = self.parse_binary_operation(prec + 1);
      while let Some((op, op_sr)) = self.matches_alternatives(BIN_OP_PRECEDENCE[prec]) {
        let right = self.parse_binary_operation(prec + 1);
        let operator = to_operator(op);
        expr_type = self.check_binary(op_sr, operator, &expr_type, &right.type_);
        expr = self.ast.add_expression(expr::Binary {
          left: expr,
          operator: to_operator(op),
          operator_sr: op_sr,
          right: right.handle,
          expr_type: Type::Unknown,
        })
      }
      ParsedExpression {
        handle: expr,
        type_: expr_type,
      }
    }
  }

  fn check_logical(&mut self, sr: SourceRange, op: Operator, lhs: &Type, rhs: &Type) -> Type {
    const OPERATORS: &[(Operator, Type, Type, Type)] = &[
      (Operator::And, Type::Bool, Type::Bool, Type::Bool),
      (Operator::Or, Type::Bool, Type::Bool, Type::Bool),
    ];

    let expr_type = OPERATORS
      .iter()
      .find(|e| e.0 == op && e.1 == *lhs && e.2 == *rhs)
      .map(|e| e.3.clone());
    if let Some(expr_type) = expr_type {
      expr_type
    } else {
      self.emit_error(ty_err::incorrect_binary_operator(
        sr,
        op,
        lhs.print_pretty(),
        rhs.print_pretty(),
      ));
      Type::Error
    }
  }

  fn parse_logical_operation(&mut self, prec: usize) -> ParsedExpression {
    return_if_err!(self, ParsedExpression::INVALID);

    if prec == MAX_LOGICAL_OP_PRECEDENCE {
      self.parse_binary_operation(0)
    } else {
      let ParsedExpression {
        handle: mut expr,
        type_: mut expr_type,
      } = self.parse_logical_operation(prec + 1);
      while let Some((op, op_sr)) = self.matches_alternatives(LOGICAL_OP_PRECEDENCE[prec]) {
        let right = self.parse_logical_operation(prec + 1);
        let operator = to_operator(op);
        expr_type = self.check_logical(op_sr, operator, &expr_type, &right.type_);
        expr = self.ast.add_expression(expr::Logical {
          left: expr,
          operator: to_operator(op),
          operator_sr: op_sr,
          right: right.handle,
          expr_type: expr_type.clone(),
        })
      }
      ParsedExpression {
        handle: expr,
        type_: expr_type,
      }
    }
  }

  fn check_assignment(&mut self, id: expr::Id, value: ParsedExpression) -> ParsedExpression {
    if let Identifier::Variable(var_id) = id.id {
      if id.id_type == value.type_ {
        ParsedExpression {
          handle: self.ast.add_expression(expr::Assignment {
            id: var_id,
            id_sr: id.id_sr,
            value: value.handle,
            type_: id.id_type.clone(),
          }),
          type_: id.id_type,
        }
      } else {
        self.emit_error(ty_err::assignment_of_incompatible_types(
          id.id_sr,
          id.id_type.print_pretty(),
          value.type_.print_pretty(),
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

    let ParsedExpression { handle: lhs, type_ } = self.parse_logical_operation(0);
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
          type_: Type::Unknown,
        },
        _ => {
          let err = parser_err::lvalue_assignment(eq_src_rc);
          self.emit_error(err);
          ParsedExpression::INVALID
        }
      }
    } else {
      ParsedExpression { handle: lhs, type_ }
    }
  }

  fn parse_lambda(&mut self) -> ParsedExpression {
    return_if_err!(self, ParsedExpression::INVALID);

    let parameters_sr_start = self.lex.previous_token_range();
    self.env.push_function();
    self.match_or_err(Token::Basic('('));
    let parameter_types = if self.lookahead != Token::Basic(')') {
      self.parse_function_params(parameters_sr_start)
    } else {
      Vec::new()
    };
    self.match_or_err(Token::Basic(')'));
    let return_type = self.parse_function_return_type();
    let parameters_sr_end = self.lex.previous_token_range();
    let body = self.parse_unscoped_block();
    let captures = self.env.pop_function();
    let parameters_sr = SourceRange::combine(parameters_sr_start, parameters_sr_end);
    let function = FunctionSignature::new(parameter_types.clone(), return_type.clone());

    ParsedExpression {
      handle: self.ast.add_expression(expr::Lambda {
        parameters_sr,
        captures,
        parameter_types,
        body,
        return_type: return_type.clone(),
      }),
      type_: function.into(),
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
    types::{type_map::TypeMap, Type},
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
    assert_eq!(lambda["Lambda"]["return_type"], format!("{}", Type::Num.0));
    assert_eq!(
      lambda["Lambda"]["parameter_types"],
      array![Type::Num, Type::Str]
    );
  }
}
