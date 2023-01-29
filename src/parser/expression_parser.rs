use super::*;

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
        let id = self.ast.add_str(id);
        let id_info = self.last_token_info();
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
      _ => Err(SourceError::from_lexer_state(
        &self.lex,
        format!("expected literal, got {}", self.lookahead),
        SourceErrorType::Parsing,
      )),
    }
  }

  fn parse_call(&mut self) -> ExprRes {
    let mut expr = self.parse_primary()?;
    let mut too_many_arguments = None;
    loop {
      match self.lookahead {
        Token::Basic('(') => {
          let call_start = self.lex.prev_token_info();
          self.advance()?;
          let mut arguments = Vec::new();
          if self.lookahead != Token::Basic(')') {
            loop {
              if arguments.len() == 255 {
                too_many_arguments = Some(SourceError::from_token_info(
                  &call_start,
                  "function cannot have more than 255 arguments".to_string(),
                  SourceErrorType::Runtime,
                ));
              }
              arguments.push(self.parse_expression()?);
              if self.matches_alternatives(&[Token::Basic(',')])?.is_none() {
                break;
              }
            }
          }
          let call_end = self.lex.prev_token_info();
          self.match_or_err(Token::Basic(')'))?;
          let call_info = self
            .ast
            .add_source_info(SourceInfo::union(call_start, call_end));
          expr = self.ast.add_expression(Expr::FnCall {
            func: expr,
            call_info,
            arguments,
          })
        }
        _ => break,
      }
    }
    if let Some(err) = too_many_arguments {
      Err(err)
    } else {
      Ok(expr)
    }
  }

  fn parse_unary(&mut self) -> ExprRes {
    if let Some((op, src_info)) =
      self.matches_alternatives(&[Token::Basic('-'), Token::Basic('!')])?
    {
      let right = self.parse_call()?;
      Ok(self.ast.add_expression(Expr::UnaryExpr {
        operator: OperatorPair::new(to_operator(op), src_info),
        right,
      }))
    } else {
      Ok(self.parse_call()?)
    }
  }

  fn parse_factor(&mut self) -> ExprRes {
    let mut expr = self.parse_unary()?;
    while let Some((op, src_info)) =
      self.matches_alternatives(&[Token::Basic('*'), Token::Basic('/')])?
    {
      let right = self.parse_unary()?;
      expr = self.ast.add_expression(Expr::BinaryExpr {
        left: expr,
        operator: OperatorPair::new(to_operator(op), src_info),
        right,
      })
    }
    Ok(expr)
  }

  fn parse_term(&mut self) -> ExprRes {
    let mut expr = self.parse_factor()?;
    while let Some((op, src_info)) =
      self.matches_alternatives(&[Token::Basic('+'), Token::Basic('-')])?
    {
      let right = self.parse_factor()?;
      expr = self.ast.add_expression(Expr::BinaryExpr {
        left: expr,
        operator: OperatorPair::new(to_operator(op), src_info),
        right,
      })
    }
    Ok(expr)
  }

  fn parse_comparison(&mut self) -> ExprRes {
    let mut expr = self.parse_term()?;
    while let Some((op, src_info)) =
      self.matches_alternatives(&[Token::Leq, Token::Geq, Token::Basic('<'), Token::Basic('>')])?
    {
      let right = self.parse_term()?;
      expr = self.ast.add_expression(Expr::BinaryExpr {
        left: expr,
        operator: OperatorPair::new(to_operator(op), src_info),
        right,
      })
    }
    Ok(expr)
  }

  fn parse_equality(&mut self) -> ExprRes {
    let mut expr = self.parse_comparison()?;
    while let Some((op, src_info)) = self.matches_alternatives(&[Token::Same, Token::Different])? {
      let right = self.parse_comparison()?;
      expr = self.ast.add_expression(Expr::BinaryExpr {
        left: expr,
        operator: OperatorPair::new(to_operator(op), src_info),
        right,
      })
    }
    Ok(expr)
  }

  fn parse_logical_and(&mut self) -> ExprRes {
    let mut lhs = self.parse_equality()?;
    while let Some((_, src_info)) = self.matches_alternative(Token::And)? {
      let rhs = self.parse_equality()?;
      lhs = self.ast.add_expression(Expr::Logical {
        left: lhs,
        operator: OperatorPair::new(Operator::And, src_info),
        right: rhs,
      });
    }
    Ok(lhs)
  }

  fn parse_logical_or(&mut self) -> ExprRes {
    let mut lhs = self.parse_logical_and()?;
    while let Some((_, src_info)) = self.matches_alternative(Token::Or)? {
      let rhs = self.parse_logical_and()?;
      lhs = self.ast.add_expression(Expr::Logical {
        left: lhs,
        operator: OperatorPair::new(Operator::Or, src_info),
        right: rhs,
      });
    }
    Ok(lhs)
  }

  fn parse_assignment(&mut self) -> ExprRes {
    let lhs = self.parse_logical_or()?;
    if let Some((eq, eq_src_info)) = self.matches_alternative(Token::Basic('='))? {
      let rhs = self.parse_assignment()?;
      if let Expr::Variable { id, id_info } = self.ast.get_expression(lhs) {
        return Ok(self.ast.add_expression(Expr::Assignment {
          name: id,
          name_info: id_info,
          value: rhs,
        }));
      } else {
        return Err(SourceError::from_token_info(
          &self.ast.get_source_info(eq_src_info),
          "left hand side of assignment is not a valid target".to_string(),
          SourceErrorType::Parsing,
        ));
      }
    }
    Ok(lhs)
  }

  pub(super) fn parse_expression(&mut self) -> ExprRes {
    self.parse_assignment()
  }
}

#[cfg(test)]
mod test {
  use crate::{
    ast::Expr,
    lexer::{Lexer, Token, TokenPair},
    parser::{self, Parser},
  };

  fn create_parser(input: &str) -> Parser {
    let lex = Lexer::new(input);
    let mut parser = Parser::new(lex);
    parser.advance().unwrap();
    parser
  }

  #[test]
  fn parse_literals() {
    let literals = [
      ("1", Token::Number(1.0)),
      ("\"string\"", Token::String("string")),
      ("true", Token::True),
    ];
    for (src, tok) in literals {
      let mut parser = create_parser(src);
      let literal = *parser.parse_expression().unwrap();
      assert!(matches!(literal, Expr::Literal(TokenPair{token, info: _ }) if token == tok));
    }
  }

  #[test]
  fn parse_identifier() {
    let mut parser = create_parser("id");
    let literal = *parser.parse_expression().unwrap();
    assert!(matches!(literal, Expr::Variable{id, id_info: _} if id == "id"));
  }

  #[test]
  fn binary_op() {
    let mut parser = create_parser("1 + 1");
    let binexp = *parser.parse_expression().unwrap();
    assert!(
      matches!(binexp, Expr::BinaryExpr{left: _, operator, right: _} if operator.token == Token::Basic('+'))
    );
  }

  #[test]
  fn operator_precedece() {
    let mut parser = create_parser("1 * 1 + 1 < 1 == 1 and 1 or 1");
    let operators = [
      Token::Or,
      Token::And,
      Token::Same,
      Token::Basic('<'),
      Token::Basic('+'),
      Token::Basic('*'),
    ];
    let mut binexp = *parser.parse_expression().unwrap();
    for op in operators {
      match binexp {
        Expr::BinaryExpr {
          left,
          operator,
          right: _,
        } if operator.token == op => {
          binexp = *left;
        }
        Expr::Logical {
          left,
          operator,
          right: _,
        } if operator.token == op => {
          binexp = *left;
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
    let assignment = *parser.parse_expression().unwrap();
    assert!(matches!(
      assignment,
      Expr::Assignment {
        name: _,
        name_info: _,
        value: _
      }
    ));
  }
}
