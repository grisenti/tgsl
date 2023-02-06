use super::*;

const MAX_PRECEDENCE: usize = 6;

const BIN_OP_PRECEDENCE: [[Token<'_>; 2]; MAX_PRECEDENCE] = [
  [Token::Or, Token::Or],
  [Token::And, Token::And],
  [Token::Same, Token::Different],
  [Token::Basic('<'), Token::Basic('>')],
  [Token::Basic('+'), Token::Basic('-')],
  [Token::Basic('*'), Token::Basic('/')],
];

impl<'src> Parser<'src> {
  fn parse_primary(&mut self) -> ExprRes {
    match self.lookahead {
      Token::Number(_) | Token::String(_) | Token::True | Token::False | Token::Null => {
        let literal = literal_from_token(self.lookahead, &mut self.ast);
        let info = self.last_token_info();
        let ret = Ok(self.ast.add_expression(Expr::Literal { literal, info }));
        self.advance()?;
        ret
      }
      Token::Id(id) => {
        let id_info = self.last_token_info();
        let id = self.env.get_name_or_add_global(id);
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
            let id = self.env.get_name_or_add_global(name);
            let name_info = self.last_token_info();
            self.advance()?;
            let name = self.ast.add_str(name);
            expr = self.ast.add_expression(Expr::Dot {
              lhs: expr,
              identifier: id,
              name,
              name_info,
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
    if prec == MAX_PRECEDENCE {
      self.parse_unary()
    } else {
      let mut expr = self.parse_binary_operation(prec + 1)?;
      while let Some((op, src_info)) = self.matches_alternatives(&BIN_OP_PRECEDENCE[prec])? {
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

  fn parse_assignment(&mut self) -> ExprRes {
    let lhs = self.parse_binary_operation(0)?;
    if let Some((_, eq_src_info)) = self.match_next(Token::Basic('='))? {
      let rhs = self.parse_binary_operation(0)?;
      match self.ast.get_expression(lhs) {
        Expr::Variable { id, id_info } => Ok(self.ast.add_expression(Expr::Assignment {
          id,
          id_info,
          value: rhs,
        })),
        Expr::Dot {
          lhs: object,
          name,
          identifier: _,
          name_info,
        } => Ok(self.ast.add_expression(Expr::Set {
          object,
          name,
          name_info,
          value: rhs,
        })),
        _ => Err(error_from_source_info(
          &self.ast.get_source_info(eq_src_info),
          "left hand side of assignment is not a valid target".to_string(),
        )),
      }
    } else {
      Ok(lhs)
    }
  }

  fn parse_closure(&mut self) -> ExprRes {
    let call_start = self.lex.prev_token_info();
    self.env.push();
    self.match_or_err(Token::Basic('('))?;
    let parameters = if self.lookahead != Token::Basic(')') {
      self.parse_function_params(call_start)
    } else {
      Ok(Vec::new())
    };
    self.match_or_err(Token::Basic(')'))?;
    let block = self.parse_block()?;
    if let Stmt::Block(body) = self.ast.get_statement(block) {
      self.env.pop();
      Ok(self.ast.add_expression(Expr::Closure {
        parameters: parameters?,
        body,
      }))
    } else {
      panic!()
    }
  }

  pub(super) fn parse_expression(&mut self) -> ExprRes {
    if self.match_next(Token::Fn)?.is_some() {
      self.parse_closure()
    } else {
      self.parse_assignment()
    }
  }
}

#[cfg(test)]
mod test {
  use crate::compiler::{
    ast::{Expr, Literal, Operator},
    lexer::Lexer,
    parser::Parser,
  };

  fn create_parser(input: &str) -> Parser {
    let lex = Lexer::new(input);
    let mut parser = Parser::new(lex);
    parser.advance().unwrap();
    parser
  }

  #[test]
  fn parse_literal_num() {
    let mut parser = create_parser("1");
    let literal = parser.parse_expression().unwrap();
    assert!(matches!(
      parser.ast.get_expression(literal),
      Expr::Literal {
        literal: Literal::Number(n),
        info: _
      } if n == 1.0
    ))
  }

  #[test]
  fn parse_literal_string() {
    let mut parser = create_parser("\"str\"");
    let literal = parser.parse_expression().unwrap();
    assert!(matches!(
      parser.ast.get_expression(literal),
      Expr::Literal {
        literal: Literal::String(s),
        info: _
      } if parser.ast.get_str(s.clone()) == "str"
    ))
  }

  #[test]
  fn binary_op() {
    let mut parser = create_parser("1 + 1");
    let binexp = parser.parse_expression().unwrap();
    assert!(
      matches!(parser.ast.get_expression(binexp), Expr::Binary{left: _, operator, right: _} if operator.op == Operator::Basic('+'))
    );
  }

  #[test]
  fn operator_precedece() {
    let mut parser = create_parser("1 * 1 + 1 < 1 == 1 and 1 or 1");
    let operators = [
      Operator::Or,
      Operator::And,
      Operator::Same,
      Operator::Basic('<'),
      Operator::Basic('+'),
      Operator::Basic('*'),
    ];
    let mut binexp = parser.parse_expression().unwrap();
    for op in operators {
      match parser.ast.get_expression(binexp) {
        Expr::Binary {
          left,
          operator,
          right: _,
        } if operator.op == op => {
          binexp = left;
        }
        _ => panic!("wrong precedence"),
      }
    }
  }

  #[test]
  fn assign_to_lvalue_error() {
    let mut parser = create_parser("1 = 2");
    let assignment = parser.parse_expression();
    assert!(matches!(assignment, Err(_)));
  }

  #[test]
  fn assign_to_rvalue() {
    let mut parser = create_parser("id = 2");
    let assignment = parser.parse_expression().unwrap();
    assert!(matches!(
      parser.ast.get_expression(assignment),
      Expr::Assignment {
        id: _,
        id_info: _,
        value: _
      }
    ));
  }

  #[test]
  fn parse_identifier() {
    let mut parser = create_parser("identifier");
    let variable = parser.parse_expression().unwrap();
    match parser.ast.get_expression(variable) {
      Expr::Variable { id, id_info: _ } => {
        assert_eq!(id.0, 0)
      }
      a => panic!("expected variable, got {a:?}"),
    }
  }
}
