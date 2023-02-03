use super::{class::NativeClass, *};

impl Interpreter {
  fn interpret_if_branch(
    &mut self,
    if_info: SourceInfoHandle,
    condition: ExprHandle,
    true_branch: StmtHandle,
    false_branch: Option<StmtHandle>,
  ) -> StmtRes {
    if let ExprValue::Boolean(value) = self.interpret_expression(condition)? {
      if value {
        self.interpret_statement(true_branch)
      } else if let Some(branch) = false_branch {
        self.interpret_statement(branch)
      } else {
        Ok(None)
      }
    } else {
      Err(SourceError::from_token_info(
        &self.ast.get_source_info(if_info),
        "if condition has to evaluate to boolean".to_string(),
        SourceErrorType::Runtime,
      ))
    }
  }

  fn bool_or_err(
    &mut self,
    formula: ExprHandle,
    info: SourceInfoHandle,
  ) -> Result<bool, SourceError> {
    match self.interpret_expression(formula.clone())? {
      ExprValue::Boolean(val) => Ok(val),
      val => Err(SourceError::from_token_info(
        &self.ast.get_source_info(info),
        format!("while condition has to evaluate to a boolean, got {val:?}"),
        SourceErrorType::Runtime,
      )),
    }
  }

  fn interpret_while_loop(
    &mut self,
    info: SourceInfoHandle,
    condition: ExprHandle,
    body: StmtHandle,
  ) -> StmtRes {
    while self.bool_or_err(condition.clone(), info)? {
      match self.interpret_statement(body.clone())? {
        Some(EarlyOut::Break) => break,
        Some(EarlyOut::Return(val)) => return Ok(Some(EarlyOut::Return(val))),
        _ => {}
      }
    }
    Ok(None)
  }

  pub(super) fn interpret_function(
    &mut self,
    parameters: &[Identifier],
    body: &[StmtHandle],
    arguments: Vec<ExprValue>,
  ) -> InterpreterFnResult {
    for (param, arg) in parameters.iter().zip(arguments) {
      self.env.set(*param, arg);
    }
    for stmt in body.iter().cloned() {
      if let Some(EarlyOut::Return(val)) = self.interpret_statement(stmt).or(Err(()))? {
        return Ok(val);
      };
    }
    Ok(ExprValue::Null)
  }

  fn add_function(
    &mut self,
    id: Identifier,
    name_info: SourceInfoHandle,
    parameters: Vec<Identifier>,
    body: Vec<StmtHandle>,
  ) -> StmtRes {
    let arity = parameters.len() as u32;
    let func = NativeFn { body, parameters };
    let interpreter_fn = InterpreterFn {
      arity,
      callable: Box::new(func),
    };
    self.env.set_if_none(
      id,
      self.ast.get_source_info(name_info),
      ExprValue::Func(interpreter_fn),
    )?;
    Ok(None)
  }

  pub(super) fn interpret_statement(&mut self, stmt: StmtHandle) -> StmtRes {
    match self.ast.get_statement(stmt) {
      Stmt::VarDecl {
        identifier,
        id_info,
        expression,
      } => self.install_identifier(identifier, id_info, expression)?,
      Stmt::Print(expression) => {
        println!("{:?}", self.interpret_expression(expression)?);
      }
      Stmt::Expr(expr) => {
        self.interpret_expression(expr)?;
      }
      Stmt::Block(stmts) => {
        for stmt in stmts {
          let res = self.interpret_statement(stmt)?;
          if res.is_some() {
            return Ok(res);
          }
        }
      }
      Stmt::IfBranch {
        if_info,
        condition,
        true_branch,
        else_branch,
      } => {
        self.interpret_if_branch(if_info, condition, true_branch, else_branch)?;
      }
      Stmt::While {
        info,
        condition,
        loop_body,
      } => {
        self.interpret_while_loop(info, condition, loop_body)?;
      }
      Stmt::Break(_) => return Ok(Some(EarlyOut::Break)),
      Stmt::Function {
        id,
        name_info,
        parameters,
        body,
      } => {
        self.add_function(id, name_info, parameters, body)?;
      }
      Stmt::Return { expr, src_info: _ } => {
        return Ok(Some(EarlyOut::Return(self.interpret_expression(expr)?)))
      }
      Stmt::Class {
        name,
        name_info,
        methods,
      } => {
        self.env.set_if_none(
          name,
          self.ast.get_source_info(name_info),
          ExprValue::Func(InterpreterFn {
            arity: 0,
            callable: Box::new(NativeClass::new(&self.ast, &methods)),
          }),
        )?;
      }
      _ => panic!(),
    };
    Ok(None)
  }
}
