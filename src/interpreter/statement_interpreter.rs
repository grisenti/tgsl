use super::{class::NativeStruct, *};

impl Interpreter {
  fn bool_or_err(
    &mut self,
    formula: ExprHandle,
    info: SourceInfoHandle,
  ) -> Result<bool, SourceError> {
    match self.interpret_expression(formula)? {
      ExprValue::Boolean(val) => Ok(val),
      val => Err(Self::runtime_error(
        &self.ast.get_source_info(info),
        format!("expected boolean, got {val:?}"),
      )),
    }
  }

  fn interpret_if_branch(
    &mut self,
    if_info: SourceInfoHandle,
    condition: ExprHandle,
    true_branch: StmtHandle,
    false_branch: Option<StmtHandle>,
  ) -> StmtRes {
    if self.bool_or_err(condition, if_info)? {
      self.interpret_statement(true_branch)
    } else if let Some(branch) = false_branch {
      self.interpret_statement(branch)
    } else {
      Ok(None)
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
    environment: EnvRef,
  ) -> InterpreterFnResult {
    let old = self.set_env(environment);
    self.new_scope();
    for (param, arg) in parameters.iter().zip(arguments) {
      self.env.borrow_mut().set(*param, arg);
    }
    for stmt in body.iter().cloned() {
      if let Some(EarlyOut::Return(val)) = self.interpret_statement(stmt)? {
        self.set_env(old);
        return Ok(val);
      };
    }
    self.set_env(old);
    Ok(ExprValue::Null)
  }

  fn add_function(
    &mut self,
    id: Identifier,
    parameters: Vec<Identifier>,
    body: Vec<StmtHandle>,
  ) -> StmtRes {
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
    self
      .env
      .borrow_mut()
      .set(id, ExprValue::Func(interpreter_fn));
    Ok(None)
  }

  pub(super) fn interpret_statement(&mut self, stmt: StmtHandle) -> StmtRes {
    match self.ast.get_statement(stmt) {
      Stmt::VarDecl {
        identifier,
        id_info: _,
        expression,
      } => {
        self.install_identifier(identifier, expression)?;
        Ok(None)
      }
      Stmt::Print(expression) => {
        println!("{:?}", self.interpret_expression(expression)?);
        Ok(None)
      }
      Stmt::Expr(expr) => {
        self.interpret_expression(expr)?;
        Ok(None)
      }
      Stmt::Block(stmts) => {
        self.new_scope();
        for stmt in stmts {
          let res = self.interpret_statement(stmt)?;
          if res.is_some() {
            self.pop_scope();
            return Ok(res);
          }
        }
        self.pop_scope();
        Ok(None)
      }
      Stmt::IfBranch {
        if_info,
        condition,
        true_branch,
        else_branch,
      } => self.interpret_if_branch(if_info, condition, true_branch, else_branch),
      Stmt::While {
        info,
        condition,
        loop_body,
      } => self.interpret_while_loop(info, condition, loop_body),
      Stmt::Break(_) => Ok(Some(EarlyOut::Break)),
      Stmt::Function {
        id,
        name_info: _,
        parameters,
        body,
      } => {
        self.add_function(id, parameters, body)?;
        Ok(None)
      }
      Stmt::Return { expr, src_info: _ } => {
        Ok(Some(EarlyOut::Return(self.interpret_expression(expr)?)))
      }
      Stmt::Struct {
        name,
        name_info: _,
        members,
      } => {
        self.env.borrow_mut().set(
          name,
          ExprValue::Func(InterpreterFn {
            arity: 0,
            callable: Box::new(NativeStruct::new(&self.ast, &members)),
          }),
        );
        Ok(None)
      }
    }
  }
}
