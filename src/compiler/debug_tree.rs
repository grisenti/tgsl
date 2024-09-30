use std::ops::Index;

#[derive(Debug, Clone)]
pub enum DebugTree {
  ObjectNode {
    object_type: &'static str,
    names: Vec<&'static str>,
    values: Vec<DebugTree>,
  },
  ArrayNode(Vec<DebugTree>),
  Value(String),
  Null,
}

impl Index<&str> for DebugTree {
  type Output = DebugTree;

  fn index(&self, index: &str) -> &Self::Output {
    match self {
      DebugTree::ObjectNode { names, values, .. } => names
        .iter()
        .position(|&name| name == index)
        .map(|index| &values[index])
        .unwrap_or(&DebugTree::Null),
      DebugTree::ArrayNode(_) => &DebugTree::Null,
      DebugTree::Value(_) => &DebugTree::Null,
      DebugTree::Null => &DebugTree::Null,
    }
  }
}

impl Index<usize> for DebugTree {
  type Output = DebugTree;

  fn index(&self, index: usize) -> &Self::Output {
    match self {
      DebugTree::ObjectNode { .. } => &DebugTree::Null,
      DebugTree::ArrayNode(values) => &values[index],
      DebugTree::Value(_) => &DebugTree::Null,
      DebugTree::Null => &DebugTree::Null,
    }
  }
}

impl DebugTree {
  pub fn to_string(&self, space_count: usize) -> String {
    let spaces = " ".repeat(space_count);
    match self {
      DebugTree::ObjectNode {
        object_type,
        names,
        values,
      } => {
        let mut result = format!("\n{spaces}{object_type}:\n");
        for (name, value) in names.iter().zip(values.iter()) {
          result.push_str(&format!(
            "{spaces} {name}: {}\n",
            value.to_string(space_count + 2)
          ));
        }
        result.pop();
        result
      }
      DebugTree::ArrayNode(array) => {
        let mut result = String::new();
        for value in array.iter() {
          if matches!(value, DebugTree::Value(_)) {
            result.push('\n');
          }
          result.push_str(&value.to_string(space_count));
        }
        result
      }
      DebugTree::Value(value) => {
        format!("{value}")
      }
      DebugTree::Null => "null".to_string(),
    }
  }
}
