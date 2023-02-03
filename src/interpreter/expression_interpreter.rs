use super::*;

impl Interpreter {
  fn handle_binary_expression(
    &mut self,
    left: ExprHandle,
    op: Operator,
    op_src_info: SourceInfo,
    right: ExprHandle,
  ) -> ExprResult {
    let lhs = self.interpret_expression(left)?;
    match op {
      Operator::And => Ok(ExprValue::Boolean(
        check_bool(lhs, op_src_info)?
          && check_bool(self.interpret_expression(right)?, op_src_info)?,
      )),
      Operator::Or => Ok(ExprValue::Boolean(
        check_bool(lhs, op_src_info)?
          || check_bool(self.interpret_expression(right)?, op_src_info)?,
      )),
      other => {
        let rhs = self.interpret_expression(right)?;
        match other {
          Operator::Basic('+') => add(lhs, rhs, op_src_info),
          Operator::Basic('-') => binary_num(lhs, rhs, op_src_info, |x, y| x - y),
          Operator::Basic('*') => binary_num(lhs, rhs, op_src_info, |x, y| x * y),
          Operator::Basic('/') => binary_num(lhs, rhs, op_src_info, |x, y| x / y),
          Operator::Basic('<') => compare(lhs, rhs, op_src_info, |x, y| x < y, |x, y| x < y),
          Operator::Basic('>') => compare(lhs, rhs, op_src_info, |x, y| x > y, |x, y| x > y),
          Operator::Leq => compare(lhs, rhs, op_src_info, |x, y| x <= y, |x, y| x <= y),
          Operator::Geq => compare(lhs, rhs, op_src_info, |x, y| x >= y, |x, y| x >= y),
          Operator::Same => equal(lhs, rhs, op_src_info),
          Operator::Different => unary_not(equal(lhs, rhs, op_src_info)?, op_src_info),
          _ => unreachable!(),
        }
      }
    }
  }

  fn handle_literal_expression(&self, literal: Literal) -> ExprResult {
    match literal {
      Literal::Number(num) => Ok(ExprValue::Num(num)),
      Literal::String(s) => Ok(ExprValue::Str(self.ast.get_str(s).to_string())),
      Literal::True => Ok(ExprValue::Boolean(true)),
      Literal::False => Ok(ExprValue::Boolean(false)),
      Literal::Null => Ok(ExprValue::Null),
    }
  }

  fn handle_unary_expression(
    &mut self,
    op: Operator,
    op_src_info: SourceInfo,
    right: ExprHandle,
  ) -> ExprResult {
    let rhs = self.interpret_expression(right)?;
    match op {
      Operator::Basic('-') => unary_minus(rhs, op_src_info),
      Operator::Basic('!') => unary_not(rhs, op_src_info),
      _ => panic!(),
    }
  }

  fn handle_function_call(
    &mut self,
    func: ExprHandle,
    call_info: SourceInfoHandle,
    arguments: Vec<ExprHandle>,
  ) -> ExprResult {
    let mut argument_values = Vec::new();
    for arg in arguments {
      argument_values.push(self.interpret_expression(arg)?);
    }
    let func_val = self.interpret_expression(func)?;
    if let ExprValue::Func(func) = func_val {
      if func.arity as usize == argument_values.len() {
        func.callable.call(self, argument_values)
      } else {
        Err(SourceError::from_token_info(
          &self.ast.get_source_info(call_info),
          format!(
            "incorrect number of function arguments (arguments: {} - arity: {})",
            argument_values.len(),
            func.arity
          ),
          SourceErrorType::Runtime,
        ))
      }
    } else {
      Err(SourceError::from_token_info(
        &self.ast.get_source_info(call_info),
        format!("cannot call {func_val:?}, only functions"),
        SourceErrorType::Runtime,
      ))
    }
  }

  fn class_instance_or_err(
    &mut self,
    expr: ExprHandle,
    info: SourceInfoHandle,
    msg: &str,
  ) -> Result<ClassInstance, SourceError> {
    if let ExprValue::ClassInstance(instace) = self.interpret_expression(expr)? {
      Ok(instace)
    } else {
      Err(SourceError::from_token_info(
        &self.ast.get_source_info(info),
        msg.to_string(),
        SourceErrorType::Runtime,
      ))
    }
  }

  fn handle_get(
    &mut self,
    object: ExprHandle,
    name: StrHandle,
    name_info: SourceInfoHandle,
  ) -> ExprResult {
    let instace = self.class_instance_or_err(
      object,
      name_info,
      "cannot access prorety of a primitive object",
    )?;
    instace.get(self.ast.get_str(name), &self.ast.get_source_info(name_info))
  }

  fn handle_set(
    &mut self,
    object: ExprHandle,
    name: StrHandle,
    name_info: SourceInfoHandle,
    value: ExprHandle,
  ) -> ExprResult {
    let mut instance = self.class_instance_or_err(
      object,
      name_info,
      "cannot set prorety of a primitive object",
    )?;
    let value = self.interpret_expression(value)?;
    Ok(instance.set(self.ast.get_str(name), value))
  }

  pub(super) fn interpret_expression(&mut self, exp: ExprHandle) -> ExprResult {
    match self.ast.get_expression(exp) {
      Expr::Literal { literal, info: _ } => self.handle_literal_expression(literal),
      Expr::BinaryExpr {
        left,
        operator,
        right,
      } => self.handle_binary_expression(
        left,
        operator.op,
        self.ast.get_source_info(operator.src_info),
        right,
      ),
      Expr::UnaryExpr { operator, right } => self.handle_unary_expression(
        operator.op,
        self.ast.get_source_info(operator.src_info),
        right,
      ),
      Expr::Variable { id, id_info } => self.env.get_or_err(id, self.ast.get_source_info(id_info)),
      Expr::Assignment { id, id_info, value } => {
        let rhs = self.interpret_expression(value)?;
        self
          .env
          .update_value_or_err(id, self.ast.get_source_info(id_info), rhs)
      }
      Expr::FnCall {
        func,
        call_info,
        arguments,
      } => self.handle_function_call(func, call_info, arguments),
      Expr::Get {
        object,
        name,
        name_info,
      } => self.handle_get(object, name, name_info),
      Expr::Set {
        object,
        name,
        name_info,
        value,
      } => self.handle_set(object, name, name_info, value),
    }
  }
}

fn unary_minus(rhs: ExprValue, op_info: SourceInfo) -> ExprResult {
  if let ExprValue::Num(x) = rhs {
    Ok(ExprValue::Num(-x))
  } else {
    Err(SourceError::from_token_info(
      &op_info,
      format!("unary - cannot be applyed to operand {rhs:?}"),
      SourceErrorType::Runtime,
    ))
  }
}

fn unary_not(rhs: ExprValue, op_info: SourceInfo) -> ExprResult {
  if let ExprValue::Boolean(x) = rhs {
    Ok(ExprValue::Boolean(!x))
  } else {
    Err(SourceError::from_token_info(
      &op_info,
      format!("unary ! cannot be applyed to operand {rhs:?}"),
      SourceErrorType::Runtime,
    ))
  }
}

fn binary_num<F>(lhs: ExprValue, rhs: ExprValue, op_info: SourceInfo, op: F) -> ExprResult
where
  F: Fn(f64, f64) -> f64,
{
  match (lhs, rhs) {
    (ExprValue::Num(l), ExprValue::Num(r)) => Ok(ExprValue::Num(op(l, r))),
    (lhs, rhs) => Err(SourceError::from_token_info(
      &op_info,
      format!("operation only works for numbers, not supported for operands {lhs:?} {rhs:?}"),
      SourceErrorType::Runtime,
    )),
  }
}

fn add(lhs: ExprValue, rhs: ExprValue, op_info: SourceInfo) -> ExprResult {
  match (lhs, rhs) {
    (ExprValue::Num(l), ExprValue::Num(r)) => Ok(ExprValue::Num(l + r)),
    (ExprValue::Str(l), ExprValue::Str(r)) => Ok(ExprValue::Str(l + &r)),
    (lhs, rhs) => Err(SourceError::from_token_info(
      &op_info,
      format!("cannot add {lhs:?} and {rhs:?}"),
      SourceErrorType::Runtime,
    )),
  }
}

fn equal(lhs: ExprValue, rhs: ExprValue, op_info: SourceInfo) -> ExprResult {
  match (lhs, rhs) {
    (ExprValue::Num(l), ExprValue::Num(r)) => Ok(ExprValue::Boolean(l == r)),
    (ExprValue::Str(l), ExprValue::Str(r)) => Ok(ExprValue::Boolean(l == r)),
    (ExprValue::Boolean(l), ExprValue::Boolean(r)) => Ok(ExprValue::Boolean(l == r)),
    (lhs, rhs) => Err(SourceError::from_token_info(
      &op_info,
      format!("cannot determine if {lhs:?} and {rhs:?} are equal"),
      SourceErrorType::Runtime,
    )),
  }
}

fn compare<Nc, Sc>(
  lhs: ExprValue,
  rhs: ExprValue,
  op_info: SourceInfo,
  num_cmp: Nc,
  str_cmp: Sc,
) -> ExprResult
where
  Sc: Fn(&str, &str) -> bool,
  Nc: Fn(f64, f64) -> bool,
{
  match (lhs, rhs) {
    (ExprValue::Num(l), ExprValue::Num(r)) => Ok(ExprValue::Boolean(num_cmp(l, r))),
    (ExprValue::Str(l), ExprValue::Str(r)) => Ok(ExprValue::Boolean(str_cmp(&l, &r))),
    (lhs, rhs) => Err(SourceError::from_token_info(
      &op_info,
      format!("operation not supported for operands {lhs:?}, {rhs:?}"),
      SourceErrorType::Runtime,
    )),
  }
}

fn check_bool(val: ExprValue, info: SourceInfo) -> Result<bool, SourceError> {
  if let ExprValue::Boolean(value) = val {
    Ok(value)
  } else {
    Err(SourceError::from_token_info(
      &info,
      format!("binary operation cannot be applied to type {val:?}, only to booleans"),
      SourceErrorType::Runtime,
    ))
  }
}
