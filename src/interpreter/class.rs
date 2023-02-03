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
          ExprValue::Func(InterpreterFn::native(func.clone())),
        )
      })),
    }
  }
}

impl ClonableFn for NativeClass {
  fn call(&self, _: &mut Interpreter, _: Vec<ExprValue>) -> InterpreterFnResult {
    Ok(ExprValue::ClassInstance(Rc::new(RefCell::new(
      self.methods.clone(),
    ))))
  }

  fn clone_box<'a>(&self) -> Box<dyn ClonableFn + 'a>
  where
    Self: 'a,
  {
    Box::new(self.clone())
  }
}
