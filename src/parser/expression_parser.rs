use super::*;

impl<'src> Parser<'src> {
  fn parse_primary(&mut self) -> ExprRes<'src> {
    match self.lookahead {
      Token::Number(_) | Token::String(_) | Token::True | Token::False | Token::Null => {
        let ret = Ok(Box::new(Expr::Literal(TokenPair::new(
          self.lookahead,
          self.lex.prev_token_info(),
        ))));
        self.advance()?;
        ret
      }
      Token::Id(id) => {
        let ret = Ok(Box::new(Expr::Variable {
          id,
          id_info: self.lex.prev_token_info(),
        }));
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

  fn parse_call(&mut self) -> ExprRes<'src> {
    let mut expr = self.parse_primary()?;
    let mut too_many_arguments = None;
    loop {
      match self.lookahead {
        Token::Basic('(') => {
          let call_start = self.lex.prev_token_info();
          self.advance()?;
          let mut arguments = Vec::new();
          loop {
            if arguments.len() == 255 {
              too_many_arguments = Some(SourceError::from_token_info(
                &call_start,
                "function cannot have more than 255 arguments".to_string(),
                SourceErrorType::Runtime,
              ));
            }
            arguments.push(*self.parse_expression()?);
            if self.matches_alternatives(&[Token::Basic(',')])?.is_none() {
              break;
            }
          }
          let call_end = self.lex.prev_token_info();
          self.match_or_err(Token::Basic(')'))?;
          expr = Box::new(Expr::FnCall {
            func: expr,
            call_info: TokenInfo::union(call_start, call_end),
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

  fn parse_unary(&mut self) -> ExprRes<'src> {
    if let Some(op) = self.matches_alternatives(&[Token::Basic('-'), Token::Basic('!')])? {
      Ok(Box::new(Expr::UnaryExpr {
        operator: op,
        right: self.parse_call()?,
      }))
    } else {
      Ok(self.parse_call()?)
    }
  }

  fn parse_factor(&mut self) -> ExprRes<'src> {
    let mut expr = self.parse_unary()?;
    while let Some(op) = self.matches_alternatives(&[Token::Basic('*'), Token::Basic('/')])? {
      let right = self.parse_unary()?;
      expr = Box::new(Expr::BinaryExpr {
        left: expr,
        operator: op,
        right,
      })
    }
    Ok(expr)
  }

  fn parse_term(&mut self) -> ExprRes<'src> {
    let mut expr = self.parse_factor()?;
    while let Some(op) = self.matches_alternatives(&[Token::Basic('+'), Token::Basic('-')])? {
      let right = self.parse_factor()?;
      expr = Box::new(Expr::BinaryExpr {
        left: expr,
        operator: op,
        right,
      })
    }
    Ok(expr)
  }

  fn parse_comparison(&mut self) -> ExprRes<'src> {
    let mut expr = self.parse_term()?;
    while let Some(op) =
      self.matches_alternatives(&[Token::Leq, Token::Geq, Token::Basic('<'), Token::Basic('>')])?
    {
      let right = self.parse_term()?;
      expr = Box::new(Expr::BinaryExpr {
        left: expr,
        operator: op,
        right,
      })
    }
    Ok(expr)
  }

  fn parse_equality(&mut self) -> ExprRes<'src> {
    let mut expr = self.parse_comparison()?;
    while let Some(op) = self.matches_alternatives(&[Token::Same, Token::Different])? {
      let right = self.parse_comparison()?;
      expr = Box::new(Expr::BinaryExpr {
        left: expr,
        operator: op,
        right,
      })
    }
    Ok(expr)
  }

  fn parse_logical_and(&mut self) -> ExprRes<'src> {
    let mut lhs = self.parse_equality()?;
    while let Some(and) = self.matches_alternatives(&[Token::And])? {
      let rhs = self.parse_equality()?;
      lhs = Box::new(Expr::Logical {
        left: lhs,
        operator: and,
        right: rhs,
      });
    }
    Ok(lhs)
  }

  fn parse_logical_or(&mut self) -> ExprRes<'src> {
    let mut lhs = self.parse_logical_and()?;
    while let Some(or) = self.matches_alternatives(&[Token::Or])? {
      let rhs = self.parse_logical_and()?;
      lhs = Box::new(Expr::Logical {
        left: lhs,
        operator: or,
        right: rhs,
      });
    }
    Ok(lhs)
  }

  fn parse_assignment(&mut self) -> ExprRes<'src> {
    let lhs = self.parse_logical_or()?;
    if let Some(eq) = self.matches_alternatives(&[Token::Basic('=')])? {
      let rhs = self.parse_assignment()?;
      if let Expr::Variable { id, id_info } = *lhs {
        return Ok(Box::new(Expr::Assignment {
          name: id,
          name_info: id_info,
          value: rhs,
        }));
      } else {
        return Err(SourceError::from_token_info(
          &eq.info,
          "left hand side of assignment is not a valid target".to_string(),
          SourceErrorType::Parsing,
        ));
      }
    }
    Ok(lhs)
  }

  pub(super) fn parse_expression(&mut self) -> ExprRes<'src> {
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
