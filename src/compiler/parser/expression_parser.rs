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
  fn parse_primary(&mut self) -> ExprRes {
    match self.lookahead {
      Token::Number(_) | Token::String(_) | Token::True | Token::False | Token::Null => {
        let value = literal_from_token(self.lookahead, &mut self.ast);
        let info = self.last_token_info();
        let ret = Ok(self.ast.add_expression(Expr::Literal { value, info }));
        self.advance()?;
        ret
      }
      Token::Id(id) => {
        let id_info = self.last_token_info();
        let id = self
          .env
          .get_name_or_add_global(id, id_info.get(&self.ast))?;
        let ret = Ok(self.ast.add_expression(Expr::Variable { id, id_info }));
        self.advance()?;
        ret
      }
      Token::Basic(c) if c == '(' => {
        self.advance()?;
        let ret = Ok(self.parse_expression()?);
        self.match_or_err(Token::Basic(')'))?;
        ret
      }
      _ => Err(error_from_lexer_state(
        &self.lex,
        format!("expected literal, got {}", self.lookahead),
      )),
    }
  }

  fn parse_arguments(&mut self, call_start: SourceInfo) -> Result<Vec<ExprHandle>, SourceError> {
    let mut arguments = Vec::new();
    loop {
      arguments.push(self.parse_expression()?);
      if self.matches_alternatives(&[Token::Basic(',')])?.is_none() {
        break;
      }
    }
    if arguments.len() >= 255 {
      Err(error_from_source_info(
        &call_start,
        "function cannot have more than 255 arguments".to_string(),
      ))
    } else {
      Ok(arguments)
    }
  }

  fn parse_call(&mut self) -> ExprRes {
    let mut expr = self.parse_primary()?;
    loop {
      match self.lookahead {
        Token::Basic('(') => {
          let call_start = self.lex.prev_token_info();
          self.advance()?;
          let arguments = if self.lookahead != Token::Basic(')') {
            self.parse_arguments(call_start)
          } else {
            Ok(Vec::new())
          };
          let call_end = self.lex.prev_token_info();
          self.match_or_err(Token::Basic(')'))?;
          let call_info = self
            .ast
            .add_source_info(SourceInfo::union(call_start, call_end));
          expr = self.ast.add_expression(Expr::FnCall {
            func: expr,
            call_info,
            arguments: arguments?,
          })
        }
        Token::Basic('.') => {
          self.advance()?;
          if let Token::Id(name) = self.lookahead {
            let id = self
              .env
              .get_name_or_add_global(name, self.lex.prev_token_info())?;
            let name_info = self.last_token_info();
            self.advance()?;
            let name = self.ast.add_str(name);
            expr = self.ast.add_expression(Expr::Dot {
              lhs: expr,
              rhs_id: id,
              rhs_name: name,
              rhs_info: name_info,
            });
          }
        }
        _ => break,
      }
    }
    Ok(expr)
  }

  fn parse_unary(&mut self) -> ExprRes {
    if let Some((op, src_info)) =
      self.matches_alternatives(&[Token::Basic('-'), Token::Basic('!')])?
    {
      let right = self.parse_call()?;
      Ok(self.ast.add_expression(Expr::Unary {
        operator: OperatorPair::new(to_operator(op), src_info),
        right,
      }))
    } else {
      Ok(self.parse_call()?)
    }
  }

  fn parse_binary_operation(&mut self, prec: usize) -> ExprRes {
    if prec == MAX_BIN_OP_PRECEDENCE {
      self.parse_unary()
    } else {
      let mut expr = self.parse_binary_operation(prec + 1)?;
      while let Some((op, src_info)) = self.matches_alternatives(BIN_OP_PRECEDENCE[prec])? {
        let right = self.parse_binary_operation(prec + 1)?;
        expr = self.ast.add_expression(Expr::Binary {
          left: expr,
          operator: OperatorPair::new(to_operator(op), src_info),
          right,
        })
      }
      Ok(expr)
    }
  }

  fn parse_logical_operation(&mut self, prec: usize) -> ExprRes {
    if prec == MAX_LOGICAL_OP_PRECEDENCE {
      self.parse_binary_operation(0)
    } else {
      let mut expr = self.parse_logical_operation(prec + 1)?;
      while let Some((op, src_info)) = self.matches_alternatives(LOGICAL_OP_PRECEDENCE[prec])? {
        let right = self.parse_logical_operation(prec + 1)?;
        expr = self.ast.add_expression(Expr::Logical {
          left: expr,
          operator: OperatorPair::new(to_operator(op), src_info),
          right,
        })
      }
      Ok(expr)
    }
  }

  fn parse_assignment(&mut self) -> ExprRes {
    let lhs = self.parse_logical_operation(0)?;
    if let Some((_, eq_src_info)) = self.match_next(Token::Basic('='))? {
      let rhs = self.parse_logical_operation(0)?;
      match lhs.get(&self.ast) {
        &Expr::Variable { id, id_info } => Ok(self.ast.add_expression(Expr::Assignment {
          id,
          id_info,
          value: rhs,
        })),
        &Expr::Dot {
          lhs: object,
          rhs_name: name,
          rhs_id: _,
          rhs_info: name_info,
        } => Ok(self.ast.add_expression(Expr::Set {
          object,
          member_name: name,
          member_name_info: name_info,
          value: rhs,
        })),
        _ => Err(error_from_source_info(
          &eq_src_info.get(&self.ast),
          "left hand side of assignment is not a valid target".to_string(),
        )),
      }
    } else {
      Ok(lhs)
    }
  }

  fn parse_lambda(&mut self) -> ExprRes {
    let parameters_sloc_start = self.lex.prev_token_info();
    self.env.push_function();
    self.match_or_err(Token::Basic('('))?;
    let parameters = if self.lookahead != Token::Basic(')') {
      self.parse_function_params(parameters_sloc_start)
    } else {
      Ok(Vec::new())
    };
    self.match_or_err(Token::Basic(')'))?;
    let return_type = self.parse_function_return_type()?;
    let parameters_sloc_end = self.lex.prev_token_info();
    let body = self.parse_unscoped_block()?;
    let captures = self.env.pop_function();
    let parameter_types = parameters?;
    let function_type_id = self.type_map.get_or_add(Type::Function {
      parameters: parameter_types.clone(),
      ret: return_type,
    });
    let info = self.ast.add_source_info(SourceInfo::union(
      parameters_sloc_start,
      parameters_sloc_end,
    ));
    Ok(self.ast.add_expression(Expr::Lambda {
      captures,
      info,
      parameter_types,
      body,
      function_type_id,
      return_type,
    }))
  }

  pub(super) fn parse_expression(&mut self) -> ExprRes {
    if self.match_next(Token::Fn)?.is_some() {
      self.parse_lambda()
    } else {
      self.parse_assignment()
    }
  }
}

#[cfg(test)]
mod test {
  use json::JsonValue;

  use crate::{
    compiler::{
      ast::AST,
      global_env::GlobalEnv,
      lexer::{Lexer, Token},
      parser::{environment::Environment, Parser},
      types::type_map::TypeMap,
    },
    errors::SourceError,
  };

  use std::collections::HashMap;

  fn parse_expression(expr: &str) -> Result<JsonValue, SourceError> {
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
    };
    parser.advance()?;
    let handle = parser.parse_expression()?;
    Ok(handle.to_json(&parser.ast))
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
    assert_eq!(dot_expr["Dot"]["rhs_id"], "Global(1)");
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
    parse_expression("1 = 2").expect_err("can assign to lvalue");
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
    let expr_no_parens = parse_expression("1 * 1 + 1 < 1 == 1").expect("parsing error");
    let expr_parens = parse_expression("(((1 * 1) + 1) < 1) == 1").expect("parsing error");
    assert_eq!(expr_no_parens, expr_parens);
  }
}
