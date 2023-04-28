use crate::return_if_err;

use super::*;

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

impl<'src> Parser<'src> {
  fn parse_primary(&mut self) -> ExprHandle {
    return_if_err!(self, ExprHandle::INVALID);
    match self.lookahead {
      Token::Number(_) | Token::String(_) | Token::True | Token::False | Token::Null => {
        let value = literal_from_token(self.lookahead, &mut self.ast);
        let value_sr = self.lex.previous_token_range();
        self.advance();
        self.ast.add_expression(Expr::Literal { value, value_sr })
      }
      Token::Id(id) => {
        let id_sr = self.lex.previous_token_range();
        let id = self.get_name_or_add_global(id, id_sr);
        self.advance();
        self.ast.add_expression(Expr::Variable { id, id_sr })
      }
      Token::Basic(c) if c == '(' => {
        self.advance();
        let ret = self.parse_expression();
        self.match_or_err(Token::Basic(')'));
        ret
      }
      _ => {
        let err = parser_err::expected_primary(&self.lex, self.lookahead);
        self.emit_error(err);
        ExprHandle::INVALID
      }
    }
  }

  fn parse_arguments(&mut self, call_start: SourceRange) -> Vec<ExprHandle> {
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

  fn parse_call(&mut self) -> ExprHandle {
    return_if_err!(self, ExprHandle::INVALID);
    let mut expr = self.parse_primary();
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
          expr = self.ast.add_expression(Expr::FnCall {
            func: expr,
            call_sr,
            arguments,
          })
        }
        Token::Basic('.') => {
          self.advance();
          if let Token::Id(rhs_name) = self.lookahead {
            let rhs_sr = self.lex.previous_token_range();
            let rhs_id = self.get_name(rhs_name, rhs_sr);
            self.advance();
            let rhs_name = self.ast.add_str(rhs_name);
            expr = self.ast.add_expression(Expr::Dot {
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
    expr
  }

  fn parse_unary(&mut self) -> ExprHandle {
    return_if_err!(self, ExprHandle::INVALID);
    if let Some((op, op_sr)) = self.matches_alternatives(&[Token::Basic('-'), Token::Basic('!')]) {
      let right = self.parse_call();
      self.ast.add_expression(Expr::Unary {
        operator: to_operator(op),
        operator_sr: op_sr,
        right,
      })
    } else {
      self.parse_call()
    }
  }

  fn parse_binary_operation(&mut self, prec: usize) -> ExprHandle {
    return_if_err!(self, ExprHandle::INVALID);
    if prec == MAX_BIN_OP_PRECEDENCE {
      self.parse_unary()
    } else {
      let mut expr = self.parse_binary_operation(prec + 1);
      while let Some((op, op_sr)) = self.matches_alternatives(BIN_OP_PRECEDENCE[prec]) {
        let right = self.parse_binary_operation(prec + 1);
        expr = self.ast.add_expression(Expr::Binary {
          left: expr,
          operator: to_operator(op),
          operator_sr: op_sr,
          right,
        })
      }
      expr
    }
  }

  fn parse_logical_operation(&mut self, prec: usize) -> ExprHandle {
    return_if_err!(self, ExprHandle::INVALID);
    if prec == MAX_LOGICAL_OP_PRECEDENCE {
      self.parse_binary_operation(0)
    } else {
      let mut expr = self.parse_logical_operation(prec + 1);
      while let Some((op, op_sr)) = self.matches_alternatives(LOGICAL_OP_PRECEDENCE[prec]) {
        let right = self.parse_logical_operation(prec + 1);
        expr = self.ast.add_expression(Expr::Logical {
          left: expr,
          operator: to_operator(op),
          operator_sr: op_sr,
          right,
        })
      }
      expr
    }
  }

  fn parse_assignment(&mut self) -> ExprHandle {
    return_if_err!(self, ExprHandle::INVALID);
    let lhs = self.parse_logical_operation(0);
    if let Some((_, eq_src_rc)) = self.match_next(Token::Basic('=')) {
      let rhs = self.parse_logical_operation(0);
      match lhs.get(&self.ast) {
        &Expr::Variable { id, id_sr } => self.ast.add_expression(Expr::Assignment {
          id,
          id_sr,
          value: rhs,
        }),
        &Expr::Dot {
          lhs: object,
          rhs_name: name,
          rhs_sr,
          ..
        } => self.ast.add_expression(Expr::Set {
          object,
          member_name: name,
          member_name_sr: rhs_sr,
          value: rhs,
        }),
        _ => {
          let err = parser_err::lvalue_assignment(eq_src_rc);
          self.emit_error(err);
          ExprHandle::INVALID
        }
      }
    } else {
      lhs
    }
  }

  fn parse_lambda(&mut self) -> ExprHandle {
    return_if_err!(self, ExprHandle::INVALID);
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
    self.ast.add_expression(Expr::Lambda {
      parameters_sr,
      captures,
      parameter_types,
      body,
      function_type_id,
      return_type,
    })
  }

  pub(super) fn parse_expression(&mut self) -> ExprHandle {
    return_if_err!(self, ExprHandle::INVALID);
    if self.match_next(Token::Fn).is_some() {
      self.parse_lambda()
    } else {
      self.parse_assignment()
    }
  }
}

#[cfg(test)]
mod test {
  use json::{array, JsonValue};

  use crate::compiler::{
    ast::AST,
    errors::CompilerError,
    global_env::GlobalEnv,
    lexer::{Lexer, Token},
    parser::{environment::Environment, Parser, ParserState},
    types::{type_map::TypeMap, TypeId},
  };

  use std::collections::HashMap;

  fn parse_expression(expr: &str) -> Result<JsonValue, Vec<CompilerError>> {
    let mut empty_type_map = TypeMap::new();
    let empty_loaded_modules = HashMap::new();
    let mut empty_global_env = GlobalEnv::new();
    let mut parser = Parser {
      lex: Lexer::new(expr),
      type_map: &mut empty_type_map,
      lookahead: Token::EndOfFile,
      loaded_modules: &empty_loaded_modules,
      ast: AST::new(),
      env: Environment::new(&mut empty_global_env),
      errors: Vec::new(),
      state: ParserState::NoErrors,
    };
    parser.advance();
    let handle = parser.parse_expression();
    if parser.errors.len() > 0 {
      Err(parser.errors)
    } else {
      Ok(handle.to_json(&parser.ast))
    }
  }

  #[test]
  fn literal_num() {
    let literal = parse_expression("1").expect("parsing error");
    assert_eq!(literal["Literal"]["value"], "1");
  }

  #[test]
  fn literal_string() {
    let literal = parse_expression("\"str\"").expect("parsing error");
    assert_eq!(literal["Literal"]["value"], "\"str\"");
  }

  #[test]
  fn literal_in_parenthesis() {
    let literal = parse_expression("(1)").expect("parsing error");
    assert_eq!(literal["Literal"]["value"], "1");
  }

  #[test]
  fn parse_empty_function_call() {
    let call = parse_expression("main()").expect("parsing error");
    assert_eq!(call["FnCall"]["function"]["Variable"]["id"], "Global(0)");
    assert_eq!(call["FnCall"]["arguments"], JsonValue::Array(vec![]));
  }

  #[test]
  fn parse_function_call_with_arguments() {
    let call = parse_expression("main(1, 1 + 1, \"hello\")").expect("parsing error");
    assert_eq!(call["FnCall"]["function"]["Variable"]["id"], "Global(0)");
    assert_eq!(call["FnCall"]["arguments"].len(), 3);
  }

  #[test]
  fn parse_dot() {
    let dot_expr = parse_expression("object.member").expect("parsing error");
    assert_eq!(dot_expr["Dot"]["lhs"]["Variable"]["id"], "Global(0)");
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
    assert_eq!(logical_op["Logical"]["left"]["Literal"]["value"], "1");
    assert_eq!(logical_op["Logical"]["right"]["Literal"]["value"], "1");
  }

  #[test]
  fn assign_to_lvalue_error() {
    let err = parse_expression("1 = 2")
      .expect_err("can assign to lvalue")
      .first()
      .unwrap()
      .clone();
    assert_eq!(err.code, "P009");
  }

  #[test]
  fn assign_to_rvalue() {
    let assignment = parse_expression("id = 2").expect("parsing error");
    assert_eq!(assignment["Assignment"]["id"], "Global(0)");
    assert_eq!(assignment["Assignment"]["value"]["Literal"]["value"], "2");
  }

  #[test]
  fn parse_identifier() {
    let identifier = parse_expression("identifier").expect("parsing error");
    assert_eq!(identifier["Variable"]["id"], "Global(0)");
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
