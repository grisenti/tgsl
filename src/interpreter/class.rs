use super::*;

#[derive(Clone)]
pub(super) struct NativeClass {
  methods: HashMap<String, ExprValue>,
}

impl NativeClass {
  pub(super) fn new(ast: &AST, methods: &[(StrHandle, Method)]) -> Self {
    Self {
      methods: HashMap::from_iter(methods.iter().map(|(handle, func)| {
        (
          ast.get_str(handle.clone()).to_string(),
          ExprValue::Func(InterpreterFn::native(func.clone(), Environment::global())),
        )
      })),
    }
  }
}

impl ClonableFn for NativeClass {
  fn call(&self, _: &mut Interpreter, _: Vec<ExprValue>) -> InterpreterFnResult {
    Ok(ExprValue::ClassInstance(ClassInstance::new(&self.methods)))
  }

  fn clone_box<'a>(&self) -> Box<dyn ClonableFn + 'a>
  where
    Self: 'a,
  {
    Box::new(self.clone())
  }
}

#[derive(Debug, Clone)]
pub struct ClassInstance {
  value: Rc<RefCell<HashMap<String, ExprValue>>>,
}

impl ClassInstance {
  fn new(methods: &HashMap<String, ExprValue>) -> Self {
    Self {
      value: Rc::new(RefCell::new(methods.clone())),
    }
  }

  pub fn get(&self, name: &str, name_info: &SourceInfo) -> ExprResult {
    self
      .value
      .borrow()
      .get(name)
      .ok_or_else(|| {
        SourceError::from_token_info(
          name_info,
          format!("{name} is not a valid property"),
          SourceErrorType::Runtime,
        )
      })
      .cloned()
  }

  pub fn set(&mut self, name: &str, value: ExprValue) -> ExprValue {
    self
      .value
      .borrow_mut()
      .insert(name.to_string(), value.clone());
    value
  }
}
