use crate::{compiler::errors::ty_err, return_if_err};

use super::*;
use crate::compiler::ast::expression::expr::DotCall;
use crate::compiler::identifier::OverloadId;
use crate::compiler::operators::{BinaryOperator, LogicalOperator, UnaryOperator};
use crate::compiler::parser::statement_parser::ReturnKind;
use crate::compiler::types::FunctionSignature;
use crate::compiler::types::Type::Struct;
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

enum MemberGetError {
  NotAStruct,
  NotAMember,
}

enum ParsedPrimary {
  Expr(ParsedExpression),
  UnresolvedOverload(OverloadId),
  Error,
}

impl From<ParsedExpression> for ParsedPrimary {
  fn from(value: ParsedExpression) -> Self {
    ParsedPrimary::Expr(value)
  }
}

impl ParsedPrimary {
  fn get_type(&self) -> &Type {
    match self {
      ParsedPrimary::Expr(expr) => &expr.type_,
      ParsedPrimary::UnresolvedOverload(_) => &Type::UnresolvedOverload,
      ParsedPrimary::Error => &Type::Error,
    }
  }
}

#[derive(Default)]
struct ParsedArguments {
  expressions: Vec<ExprHandle>,
  types: Vec<Type>,
}

fn check_arguments(
  errors: &mut Vec<CompilerError>,
  parameters: &[Type],
  arguments: &[Type],
  call_sr: SourceRange,
) {
  if parameters.len() != arguments.len() {
    errors.push(ty_err::incorrect_function_argument_number(
      call_sr,
      parameters.len(),
      arguments.len(),
    ));
    return;
  }

  for (index, (param, argument)) in parameters.iter().zip(arguments).enumerate() {
    if *param != *argument && *param != Type::Any {
      errors.push(ty_err::incorrect_function_argument_type(
        call_sr,
        index + 1,
        argument.print_pretty(),
        param.print_pretty(),
      ));
    }
  }
}

impl<'src> Parser<'src> {
  fn parse_arguments(&mut self, call_start: SourceRange, terminator: char) -> ParsedArguments {
    return_if_err!(self, ParsedArguments::default());

    let mut arguments = ParsedArguments::default();
    while self.lookahead != Token::Basic(terminator) {
      let expr = self.parse_expression();
      arguments.expressions.push(expr.handle);
      arguments.types.push(expr.type_);
      if self.matches_alternatives(&[Token::Basic(',')]).is_none() {
        break;
      }
    }
    if arguments.expressions.len() >= 255 {
      let err = parser_err::too_many_function_arguments(call_start);
      self.emit_error(err);
      ParsedArguments::default()
    } else {
      arguments
    }
  }

  fn parse_constructor(&mut self, id: &str, id_sr: SourceRange) -> ParsedExpression {
    assert_eq!(self.lookahead, Token::Basic('{'));

    let struct_id = self.get_struct_id(id, id_sr);
    return_if_err!(self, ParsedExpression::INVALID);
    let arguments_start = self.lex.previous_token_range();
    self.advance();
    let arguments = self.parse_arguments(arguments_start, '}');
    let arguments_end = self.lex.previous_token_range();
    let arguments_sr = SourceRange::combine(arguments_start, arguments_end);
    self.match_or_err(Token::Basic('}'));
    if let Some(struct_) = self.env.get_struct(struct_id) {
      check_arguments(
        &mut self.errors,
        struct_.get_member_types(),
        &arguments.types,
        arguments_sr,
      );
      ParsedExpression {
        handle: self.ast.add_expression(expr::Construct {
          arguments: arguments.expressions,
          struct_id,
        }),
        type_: Struct(struct_id),
      }
    } else {
      panic!(); // cannot construct
    }
  }

  fn parse_primary(&mut self) -> ParsedPrimary {
    return_if_err!(self, ParsedPrimary::Error);

    match self.lookahead {
      Token::Number(num) => {
        self.advance();
        ParsedExpression {
          handle: self.ast.add_expression(expr::LiteralNumber { value: num }),
          type_: Type::Num,
        }
        .into()
      }
      Token::String(str) => {
        self.advance();
        let handle = self.ast.add_str(str);
        ParsedExpression {
          handle: self.ast.add_expression(expr::LiteralString { handle }),
          type_: Type::Str,
        }
        .into()
      }
      Token::True | Token::False => {
        let value = self.lookahead == Token::True;
        self.advance();
        ParsedExpression {
          handle: self.ast.add_expression(expr::LiteralBool { value }),
          type_: Type::Bool,
        }
        .into()
      }
      Token::Id(id) => {
        let id_sr = self.lex.previous_token_range();
        self.advance();
        if let Token::Basic('{') = self.lookahead {
          ParsedPrimary::Expr(self.parse_constructor(id, id_sr))
        } else {
          let resolved_id = self.get_id(id, id_sr);
          match resolved_id {
            ResolvedIdentifier::ResolvedIdentifier { id, type_ } => ParsedExpression {
              handle: self.ast.add_expression(expr::Id {
                id,
                id_type: type_.clone(),
              }),
              type_: type_.clone(),
            }
            .into(),
            ResolvedIdentifier::UnresolvedOverload(overload_id) => {
              ParsedPrimary::UnresolvedOverload(overload_id)
            }
            _ => ParsedPrimary::Error,
          }
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
        .into()
      }
      _ => {
        let err = parser_err::expected_primary(&self.lex, self.lookahead);
        self.emit_error(err);
        ParsedPrimary::Error
      }
    }
  }

  fn parse_function_call(&mut self, expr: ParsedPrimary) -> ParsedPrimary {
    return_if_err!(self, ParsedExpression::INVALID.into());
    assert_eq!(self.lookahead, Token::Basic('('));

    let call_start_sr = self.lex.previous_token_range();
    self.advance();
    let arguments = self.parse_arguments(call_start_sr, ')');
    let call_end_sr = self.lex.previous_token_range();
    self.match_or_err(Token::Basic(')'));
    let call_sr = SourceRange::combine(call_start_sr, call_end_sr);
    match expr {
      ParsedPrimary::Expr(expr) => {
        if let Type::Function(signature) = expr.type_ {
          let (parameters, return_type) = signature.into_parts();
          check_arguments(&mut self.errors, &parameters, &arguments.types, call_sr);
          let handle = self.ast.add_expression(expr::FnCall {
            func: expr.handle,
            arguments: arguments.expressions,
            expr_type: return_type.clone(),
          });
          ParsedExpression {
            handle,
            type_: return_type,
          }
          .into()
        } else {
          self.emit_error(ty_err::cannot_call_type(call_sr, expr.type_.print_pretty()));
          ParsedPrimary::Error
        }
      }
      ParsedPrimary::UnresolvedOverload(overload_id) => {
        let resolved_function = self.env.resolve_overload(overload_id, &arguments.types);
        if let Some(resolved_function) = resolved_function {
          let id_expr_node_handle = self.ast.add_expression(expr::Id {
            id: Identifier::Function(resolved_function.function_id),
            id_type: resolved_function.function_signature.clone().into(),
          });
          let return_type = resolved_function.function_signature.get_return_type();
          ParsedExpression {
            handle: self.ast.add_expression(expr::FnCall {
              func: id_expr_node_handle,
              arguments: arguments.expressions,
              expr_type: return_type.clone(),
            }),
            type_: return_type.clone(),
          }
          .into()
        } else {
          todo!() // no valid overload
        }
      }
      ParsedPrimary::Error => ParsedPrimary::Error,
    }
  }

  fn try_parse_member_get(
    &mut self,
    primary: &ParsedPrimary,
    member_name: &str,
  ) -> Result<ParsedPrimary, MemberGetError> {
    let expr = if let ParsedPrimary::Expr(expr) = primary {
      expr
    } else {
      return Err(MemberGetError::NotAStruct);
    };
    if let Type::Struct(struct_id) = expr.type_ {
      let s = self.env.get_struct(struct_id).unwrap();
      if let Some(member_index) = s.get_member_index(member_name) {
        let member_type = s.member_info(member_index).1.clone();
        let handle = self.ast.add_expression(expr::MemberGet {
          lhs: expr.handle,
          member_index,
        });
        Ok(
          ParsedExpression {
            handle,
            type_: member_type,
          }
          .into(),
        )
      } else {
        Err(MemberGetError::NotAMember)
      }
    } else {
      Err(MemberGetError::NotAStruct)
    }
  }

  fn try_resolve_dot_call(
    &mut self,
    primary: ParsedPrimary,
    name: &str,
    name_sr: SourceRange,
    member_get_error: MemberGetError,
  ) -> ParsedPrimary {
    if let Token::Basic('(') = self.lookahead {
      let (id, id_type) = self.get_variable_id(name, name_sr);
      return_if_err!(self, ParsedPrimary::Error);

      if let Type::Function(signature) = id_type {
        let (parameters, return_type) = signature.into_parts();
        let call_start = self.lex.previous_token_range();
        self.advance();
        if parameters
          .first()
          .is_some_and(|p1| *p1 == *primary.get_type())
        {
          let expr = if let ParsedPrimary::Expr(expr) = primary {
            expr
          } else {
            return ParsedPrimary::Error;
          };
          let parsed_arguments = self.parse_arguments(call_start, ')');
          let call_end = self.lex.previous_token_range();
          self.match_or_err(Token::Basic(')'));
          let call_sr = SourceRange::combine(call_start, call_end);
          check_arguments(
            &mut self.errors,
            &parameters[1..parameters.len()],
            &parsed_arguments.types,
            call_sr,
          );
          let handle = self.ast.add_expression(DotCall {
            function: id,
            lhs: expr.handle,
            arguments: parsed_arguments.expressions,
          });
          ParsedExpression {
            handle,
            type_: return_type,
          }
          .into()
        } else {
          match member_get_error {
            MemberGetError::NotAStruct => self.emit_error(ty_err::no_member_and_no_function_found(
              name_sr,
              name,
              primary.get_type().print_pretty(),
            )),
            MemberGetError::NotAMember => {
              self.emit_error(ty_err::could_not_find_function_for_dot_call(
                name_sr,
                name,
                primary.get_type().print_pretty(),
              ))
            }
          }
          ParsedPrimary::Error
        }
      } else {
        self.emit_error(ty_err::cannot_call_type(name_sr, id_type.print_pretty()));
        ParsedPrimary::Error
      }
    } else {
      match member_get_error {
        MemberGetError::NotAStruct => {
          self.emit_error(ty_err::cannot_access_member_of_non_struct_type(
            name_sr,
            primary.get_type().print_pretty(),
          ))
        }
        MemberGetError::NotAMember => self.emit_error(ty_err::not_a_member(
          name_sr,
          name,
          primary.get_type().print_pretty(),
        )),
      }
      ParsedPrimary::Error
    }
  }

  fn parse_dot(&mut self, primary: ParsedPrimary) -> ParsedPrimary {
    return_if_err!(self, ParsedPrimary::Error);
    assert_eq!(self.lookahead, Token::Basic('.'));
    // if its lhs is an object and name is a member, get the member
    // if name is a function, try to call the function

    self.advance();
    let (name, name_sr) = self.match_id_or_err();
    match self.try_parse_member_get(&primary, name) {
      Ok(result) => result,
      Err(err) => self.try_resolve_dot_call(primary, name, name_sr, err),
    }
  }

  fn parse_call(&mut self) -> ParsedExpression {
    return_if_err!(self, ParsedExpression::INVALID);

    let mut expr = self.parse_primary();
    // if its a unresolved overload, try to get an expression
    loop {
      return_if_err!(self, ParsedExpression::INVALID);
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
    match expr {
      ParsedPrimary::Expr(expr) => expr,
      ParsedPrimary::UnresolvedOverload(_) => ParsedExpression {
        handle: ExprHandle::INVALID,
        type_: Type::UnresolvedOverload,
      },
      ParsedPrimary::Error => ParsedExpression::INVALID,
    }
  }

  fn check_unary(&mut self, sr: SourceRange, op: Token, rhs: Type) -> (Type, UnaryOperator) {
    #[rustfmt::skip]
    const OPERATORS: &[(Token, Type, Type, UnaryOperator)] = &[
      (Token::Basic('-'), Type::Num, Type::Num, UnaryOperator::NegNum),
      (Token::Basic('!'), Type::Bool, Type::Bool, UnaryOperator::NotBool),
    ];
    let result = OPERATORS
      .iter()
      .find(|e| e.0 == op && e.1 == rhs)
      .map(|e| (e.2.clone(), e.3));
    if let Some(result) = result {
      result
    } else {
      self.emit_error(ty_err::incorrect_unary_operator(sr, op, rhs.print_pretty()));
      (Type::Error, UnaryOperator::Invalid)
    }
  }

  fn parse_unary(&mut self) -> ParsedExpression {
    return_if_err!(self, ParsedExpression::INVALID);

    if let Some((op, op_sr)) = self.matches_alternatives(&[Token::Basic('-'), Token::Basic('!')]) {
      let right = self.parse_call();
      let (result_type, operator) = self.check_unary(op_sr, op, right.type_);
      ParsedExpression {
        handle: self.ast.add_expression(expr::Unary {
          operator,
          right: right.handle,
          expr_type: result_type.clone(),
        }),
        type_: result_type,
      }
    } else {
      self.parse_call()
    }
  }

  fn check_binary(
    &mut self,
    sr: SourceRange,
    op: Token,
    lhs: &Type,
    rhs: &Type,
  ) -> (Type, BinaryOperator) {
    #[rustfmt::skip]
    const OPERATORS: &[(Token, Type, Type, Type, BinaryOperator)] = &[
      // number
      (Token::Basic('+'), Type::Num, Type::Num, Type::Num, BinaryOperator::AddNum),
      (Token::Basic('-'), Type::Num, Type::Num, Type::Num, BinaryOperator::SubNum),
      (Token::Basic('*'), Type::Num, Type::Num, Type::Num, BinaryOperator::MulNum),
      (Token::Basic('/'), Type::Num, Type::Num, Type::Num, BinaryOperator::DivNum),
      (Token::Basic('<'), Type::Num, Type::Num, Type::Bool, BinaryOperator::LeNum),
      (Token::Basic('>'), Type::Num, Type::Num, Type::Bool, BinaryOperator::GeNum),
      (Token::Leq, Type::Num, Type::Num, Type::Bool, BinaryOperator::LeqNum),
      (Token::Geq, Type::Num, Type::Num, Type::Bool, BinaryOperator::GeqNum),
      (Token::Same, Type::Num, Type::Num, Type::Bool, BinaryOperator::SameNum),
      (Token::Different, Type::Num, Type::Num, Type::Bool, BinaryOperator::DiffNum),
      // string Token
      (Token::Basic('+'), Type::Str, Type::Str, Type::Str, BinaryOperator::AddStr),
      (Token::Basic('<'), Type::Str, Type::Str, Type::Bool, BinaryOperator::LeStr),
      (Token::Basic('>'), Type::Str, Type::Str, Type::Bool, BinaryOperator::GeStr),
      (Token::Leq, Type::Str, Type::Str, Type::Bool, BinaryOperator::LeqStr),
      (Token::Geq, Type::Str, Type::Str, Type::Bool, BinaryOperator::GeqStr),
      (Token::Same, Type::Str, Type::Str, Type::Bool, BinaryOperator::SameStr),
      (Token::Different, Type::Str, Type::Str, Type::Bool, BinaryOperator::DiffStr),
      // bool
      (Token::Same, Type::Bool, Type::Bool, Type::Bool, BinaryOperator::SameBool),
      (Token::Different, Type::Bool, Type::Bool, Type::Bool, BinaryOperator::DiffBool),
    ];

    let result = OPERATORS
      .iter()
      .filter(|bin_op| bin_op.0 == op)
      .find(|bin_op| bin_op.1 == *lhs && bin_op.2 == *rhs)
      .map(|e| (e.3.clone(), e.4));
    if let Some(result) = result {
      result
    } else {
      self.emit_error(ty_err::incorrect_binary_operator(
        sr,
        op,
        lhs.print_pretty(),
        rhs.print_pretty(),
      ));
      (Type::Error, BinaryOperator::Invalid)
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
        let (result_type, operator) = self.check_binary(op_sr, op, &expr_type, &right.type_);
        expr_type = result_type;
        expr = self.ast.add_expression(expr::Binary {
          left: expr,
          operator,
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

  fn check_logical(
    &mut self,
    sr: SourceRange,
    op: Token,
    lhs: &Type,
    rhs: &Type,
  ) -> (Type, LogicalOperator) {
    #[rustfmt::skip]
    const OPERATORS: &[(Token, Type, Type, Type, LogicalOperator)] = &[
      (Token::And, Type::Bool, Type::Bool, Type::Bool, LogicalOperator::And),
      (Token::Or, Type::Bool, Type::Bool, Type::Bool, LogicalOperator::Or),
    ];

    let result = OPERATORS
      .iter()
      .find(|e| e.0 == op && e.1 == *lhs && e.2 == *rhs)
      .map(|e| (e.3.clone(), e.4));
    if let Some(result) = result {
      result
    } else {
      self.emit_error(ty_err::incorrect_binary_operator(
        sr,
        op,
        lhs.print_pretty(),
        rhs.print_pretty(),
      ));
      (Type::Error, LogicalOperator::Invalid)
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
        let (resuls_type, operator) = self.check_logical(op_sr, op, &expr_type, &right.type_);
        expr_type = resuls_type;
        expr = self.ast.add_expression(expr::Logical {
          left: expr,
          operator,
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

  fn check_assignment(
    &mut self,
    id: expr::Id,
    value: ParsedExpression,
    eq_sr: SourceRange,
  ) -> ParsedExpression {
    match id.id {
      Identifier::Variable(var_id) => {
        if id.id_type == value.type_ {
          return ParsedExpression {
            handle: self.ast.add_expression(expr::Assignment {
              id: var_id,
              value: value.handle,
              type_: id.id_type.clone(),
            }),
            type_: id.id_type,
          };
        } else {
          self.emit_error(ty_err::assignment_of_incompatible_types(
            eq_sr,
            id.id_type.print_pretty(),
            value.type_.print_pretty(),
          ));
        }
      }
      Identifier::ExternFunction(_) | Identifier::Function(_) => {
        self.emit_error(ty_err::cannot_assign_to_function(eq_sr))
      }
      Identifier::Struct(_) => self.emit_error(ty_err::cannot_assing_to_type(eq_sr)),
      Identifier::Invalid => {} // error already handled
    }
    ParsedExpression::INVALID
  }

  fn parse_assignment(&mut self) -> ParsedExpression {
    return_if_err!(self, ParsedExpression::INVALID);

    let ParsedExpression { handle: lhs, type_ } = self.parse_logical_operation(0);
    if let Some((_, eq_sr)) = self.match_next(Token::Basic('=')) {
      let rhs = self.parse_logical_operation(0);
      // TODO: maybe consider making simple nodes copy
      match lhs.get(&self.ast).clone() {
        Expr::Id(id) => self.check_assignment(id, rhs, eq_sr),
        Expr::MemberGet(member) => ParsedExpression {
          handle: self.ast.add_expression(expr::MemberSet {
            member: member.clone(),
            value: rhs.handle,
          }),
          type_: rhs.type_.clone(),
        },
        _ => {
          let err = parser_err::lvalue_assignment(eq_sr);
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
    let parameter_types = self.parse_function_params(parameters_sr_start);
    self.match_or_err(Token::Basic(')'));
    let return_type = self.parse_function_return_type();
    self.env.define_function_return_type(return_type.clone());
    let body = self.parse_unscoped_block();
    let captures = self.env.pop_function();
    let function = FunctionSignature::new(parameter_types.clone(), return_type.clone());

    if (body.1 == ReturnKind::Conditional || body.1 == ReturnKind::None)
      && return_type != Type::Nothing
    {
      self.emit_error(ty_err::no_unconditional_return(SourceRange::EMPTY)); //FIXME: provide proper sr
      return ParsedExpression::INVALID;
    }
    let id = self.env.new_function_id();

    ParsedExpression {
      handle: self.ast.add_expression(expr::Lambda {
        id,
        captures,
        parameter_types,
        body: body.0,
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
