use std::collections::hash_map::Entry;

use super::*;

#[derive(Clone)]
pub(super) struct NativeStruct {
  methods: HashMap<String, ExprValue>,
}

impl NativeStruct {
  pub(super) fn new(ast: &AST, methods: &[StrHandle]) -> Self {
    Self {
      methods: HashMap::from_iter(
        methods
          .iter()
          .map(|handle| (ast.get_str(handle.clone()).to_string(), ExprValue::Null)),
      ),
    }
  }
}

impl ClonableFn for NativeStruct {
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

fn invalid_access(name: &str, name_info: &SourceInfo) -> SourceError {
  SourceError::from_source_info(
    name_info,
    format!("{name} is not a valid property"),
    SourceErrorType::Runtime,
  )
}

impl ClassInstance {
  fn new(methods: &HashMap<String, ExprValue>) -> Self {
    Self {
      value: Rc::new(RefCell::new(methods.clone())),
    }
  }

  pub fn get(&self, name: &str, name_info: &SourceInfo) -> Option<ExprValue> {
    self.value.borrow().get(name).cloned()
  }

  pub fn set(&mut self, name: &str, name_info: &SourceInfo, value: ExprValue) -> ExprResult {
    self
      .value
      .borrow_mut()
      .get_mut(name)
      .map(|val| *val = value.clone())
      .ok_or_else(|| invalid_access(name, name_info))?;
    Ok(value)
  }
}
