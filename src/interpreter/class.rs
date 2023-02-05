use std::collections::hash_map::Entry;

use super::*;

type StructMembers = Vec<(String, ExprValue)>;

#[derive(Clone)]
pub(super) struct NativeStruct {
  members: StructMembers,
}

impl NativeStruct {
  pub(super) fn new(ast: &AST, members: &[StrHandle]) -> Self {
    Self {
      members: Vec::from_iter(members.iter().map(|handle| {
        (
          ast.get_str(handle.clone()).to_string(),
          ExprValue::Undefined,
        )
      })),
    }
  }
}

impl ClonableFn for NativeStruct {
  fn call(&self, _: &mut Interpreter, values: Vec<ExprValue>) -> InterpreterFnResult {
    Ok(ExprValue::ClassInstance(ClassInstance::new(
      self
        .members
        .iter()
        .zip(values)
        .map(|(mem, value)| (mem.0.clone(), value))
        .collect(),
    )))
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
  value: Rc<RefCell<StructMembers>>,
}

fn invalid_access(name: &str, name_info: &SourceInfo) -> SourceError {
  SourceError::from_source_info(
    name_info,
    format!("{name} is not a valid property"),
    SourceErrorType::Runtime,
  )
}

impl ClassInstance {
  fn new(methods: StructMembers) -> Self {
    Self {
      value: Rc::new(RefCell::new(methods)),
    }
  }

  pub fn get(&self, name: &str, name_info: &SourceInfo) -> Option<ExprValue> {
    self
      .value
      .borrow()
      .iter()
      .find(|(member_name, _)| member_name == name)
      .map(|(_, value)| value)
      .cloned()
  }

  pub fn set(&mut self, name: &str, name_info: &SourceInfo, value: ExprValue) -> ExprResult {
    self
      .value
      .borrow_mut()
      .iter_mut()
      .find(|(member_name, _)| member_name == name)
      .map(|(_, val)| *val = value.clone())
      .ok_or_else(|| invalid_access(name, name_info))?;
    Ok(value)
  }
}
