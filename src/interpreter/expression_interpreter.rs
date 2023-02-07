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
    let func_val = self.interpret_expression(func)?;
    let (mut argument_values, func) = match func_val {
      ExprValue::Func(func) => (Vec::new(), func),
      ExprValue::PartialCall { func, args } => (args, func),
      _ => {
        return Err(Self::runtime_error(
          &self.ast.get_source_info(call_info),
          format!("cannot call {func_val:?}, only functions"),
        ))
      }
    };
    for arg in arguments {
      argument_values.push(self.interpret_expression(arg)?);
    }
    if func.arity as usize == argument_values.len() {
      func.callable.call(self, argument_values)
    } else {
      Err(Self::runtime_error(
        &self.ast.get_source_info(call_info),
        format!(
          "incorrect number of function arguments (arguments: {} - arity: {})",
          argument_values.len(),
          func.arity
        ),
      ))
    }
  }

  fn dot_call(
    &mut self,
    lhs: ExprValue,
    name: StrHandle,
    id: Identifier,
    name_info: SourceInfoHandle,
  ) -> ExprResult {
    if let Some(ExprValue::Func(func)) = self.env.borrow().get(id) {
      Ok(ExprValue::PartialCall {
        func,
        args: vec![lhs],
      })
    } else {
      Err(Self::runtime_error(
        &self.ast.get_source_info(name_info),
        format!(
          "{} is neither an object member nor a viable function",
          self.ast.get_str(name)
        ),
      ))
    }
  }

  fn handle_dot(
    &mut self,
    object: ExprHandle,
    name: StrHandle,
    id: Identifier,
    name_info: SourceInfoHandle,
  ) -> ExprResult {
    let lhs = self.interpret_expression(object)?;
    if let ExprValue::ClassInstance(instance) = lhs {
      if let Some(val) = instance.get(
        self.ast.get_str(name.clone()),
        &self.ast.get_source_info(name_info),
      ) {
        Ok(val)
      } else {
        self.dot_call(ExprValue::ClassInstance(instance), name, id, name_info)
      }
    } else {
      self.dot_call(lhs, name, id, name_info)
    }
  }

  fn class_instance_or_err(
    &mut self,
    expr: ExprHandle,
    info: SourceInfoHandle,
  ) -> Result<ClassInstance, SourceError> {
    match self.interpret_expression(expr)? {
      ExprValue::ClassInstance(instance) => Ok(instance),
      other => Err(Self::runtime_error(
        &self.ast.get_source_info(info),
        format!("cannot access member of {other:?}"),
      )),
    }
  }

  fn handle_set(
    &mut self,
    object: ExprHandle,
    name: StrHandle,
    name_info: SourceInfoHandle,
    value: ExprHandle,
  ) -> ExprResult {
    let mut instance = self.class_instance_or_err(object, name_info)?;
    let value = self.interpret_expression(value)?;
    instance.set(
      self.ast.get_str(name),
      &self.ast.get_source_info(name_info),
      value,
    )
  }

  fn create_closure(&mut self, parameters: Vec<Identifier>, body: Vec<StmtHandle>) -> ExprResult {
    let arity = parameters.len() as u32;
    let func = NativeFn {
      body,
      parameters,
      capture: self.env.clone(),
    };
    let interpreter_fn = InterpreterFn {
      arity,
      callable: Box::new(func),
    };
    Ok(ExprValue::Func(Rc::new(interpreter_fn)))
  }

  pub(super) fn interpret_expression(&mut self, exp: ExprHandle) -> ExprResult {
    match self.ast.get_expression(exp) {
      Expr::Literal { literal, info: _ } => self.handle_literal_expression(literal),
      Expr::Binary {
        left,
        operator,
        right,
      } => self.handle_binary_expression(
        left,
        operator.op,
        self.ast.get_source_info(operator.src_info),
        right,
      ),
      Expr::Unary { operator, right } => self.handle_unary_expression(
        operator.op,
        self.ast.get_source_info(operator.src_info),
        right,
      ),
      Expr::Variable { id, id_info } => self
        .env
        .borrow()
        .get_or_err(id, self.ast.get_source_info(id_info)),
      Expr::Assignment { id, id_info, value } => {
        let rhs = self.interpret_expression(value)?;
        self
          .env
          .borrow_mut()
          .update_value_or_err(id, self.ast.get_source_info(id_info), rhs)
      }
      Expr::Closure {
        parameters, body, ..
      } => self.create_closure(parameters, body),
      Expr::FnCall {
        func,
        call_info,
        arguments,
      } => self.handle_function_call(func, call_info, arguments),
      Expr::Dot {
        lhs: object,
        name,
        identifier,
        name_info,
      } => self.handle_dot(object, name, identifier, name_info),
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
    Err(Interpreter::runtime_error(
      &op_info,
      format!("unary - cannot be applyed to operand {rhs:?}"),
    ))
  }
}

fn unary_not(rhs: ExprValue, op_info: SourceInfo) -> ExprResult {
  if let ExprValue::Boolean(x) = rhs {
    Ok(ExprValue::Boolean(!x))
  } else {
    Err(Interpreter::runtime_error(
      &op_info,
      format!("unary ! cannot be applyed to operand {rhs:?}"),
    ))
  }
}

fn binary_num<F>(lhs: ExprValue, rhs: ExprValue, op_info: SourceInfo, op: F) -> ExprResult
where
  F: Fn(f64, f64) -> f64,
{
  match (lhs, rhs) {
    (ExprValue::Num(l), ExprValue::Num(r)) => Ok(ExprValue::Num(op(l, r))),
    (lhs, rhs) => Err(Interpreter::runtime_error(
      &op_info,
      format!("operation only works for numbers, not supported for operands {lhs:?} {rhs:?}"),
    )),
  }
}

fn add(lhs: ExprValue, rhs: ExprValue, op_info: SourceInfo) -> ExprResult {
  match (lhs, rhs) {
    (ExprValue::Num(l), ExprValue::Num(r)) => Ok(ExprValue::Num(l + r)),
    (ExprValue::Str(l), ExprValue::Str(r)) => Ok(ExprValue::Str(l + &r)),
    (lhs, rhs) => Err(Interpreter::runtime_error(
      &op_info,
      format!("cannot add {lhs:?} and {rhs:?}"),
    )),
  }
}

fn equal(lhs: ExprValue, rhs: ExprValue, op_info: SourceInfo) -> ExprResult {
  match (lhs, rhs) {
    (ExprValue::Num(l), ExprValue::Num(r)) => Ok(ExprValue::Boolean(l == r)),
    (ExprValue::Str(l), ExprValue::Str(r)) => Ok(ExprValue::Boolean(l == r)),
    (ExprValue::Boolean(l), ExprValue::Boolean(r)) => Ok(ExprValue::Boolean(l == r)),
    (ExprValue::Null, ExprValue::Null) => Ok(ExprValue::Boolean(true)),
    (ExprValue::Null, _) | (_, ExprValue::Null) => Ok(ExprValue::Boolean(false)),
    (lhs, rhs) => Err(Interpreter::runtime_error(
      &op_info,
      format!("cannot determine if {lhs:?} and {rhs:?} are equal"),
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
    (lhs, rhs) => Err(Interpreter::runtime_error(
      &op_info,
      format!("operation not supported for operands {lhs:?}, {rhs:?}"),
    )),
  }
}

fn check_bool(val: ExprValue, info: SourceInfo) -> Result<bool, SourceError> {
  if let ExprValue::Boolean(value) = val {
    Ok(value)
  } else {
    Err(Interpreter::runtime_error(
      &info,
      format!("binary operation cannot be applied to type {val:?}, only to booleans"),
    ))
  }
}
