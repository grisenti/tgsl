use json::object;
use json::JsonValue;

use crate::compiler::ast::parsed_type::ParsedType;
use crate::compiler::ast::{ExprHandle, StmtHandle, TypeHandle};
use crate::compiler::lexer::Token;

use super::expression::*;
use super::statement::*;
use super::AST;

impl From<&Token<'_>> for JsonValue {
  fn from(value: &Token<'_>) -> Self {
    JsonValue::String(format!("{:?}", value))
  }
}

pub trait ToJson {
  fn to_json(&self, ast: &AST) -> JsonValue;
}

impl ToJson for ExprHandle {
  fn to_json(&self, ast: &AST) -> JsonValue {
    match self.get_expr(ast) {
      Expr::Literal(l) => l.to_json(ast),
      Expr::Id(i) => i.to_json(ast),
      Expr::Paren(p) => p.to_json(ast),
      Expr::Assignment(a) => a.to_json(ast),
      Expr::Binary(b) => b.to_json(ast),
      Expr::Unary(u) => u.to_json(ast),
      Expr::Lambda(l) => l.to_json(ast),
      Expr::FnCall(f) => f.to_json(ast),
      Expr::MemberGet(m) => m.to_json(ast),
      Expr::MemberSet(m) => m.to_json(ast),
      Expr::DotCall(d) => d.to_json(ast),
      Expr::Construct(c) => c.to_json(ast),
    }
  }
}

impl ToJson for Vec<ExprHandle> {
  fn to_json(&self, ast: &AST) -> JsonValue {
    self
      .iter()
      .map(|e| e.to_json(ast))
      .collect::<Vec<_>>()
      .into()
  }
}

impl ToJson for Token<'_> {
  fn to_json(&self, _: &AST) -> JsonValue {
    JsonValue::String(format!("{:?}", self))
  }
}

impl ToJson for expr::Literal<'_> {
  fn to_json(&self, _: &AST) -> JsonValue {
    object! {
      "Literal": {
        "value": &self.value
      }
    }
  }
}

impl ToJson for expr::Id<'_> {
  fn to_json(&self, _: &AST) -> JsonValue {
    object! {
      "Id": {
        "id": self.id
      }
    }
  }
}

impl ToJson for expr::Paren {
  fn to_json(&self, ast: &AST) -> JsonValue {
    object! {
      "Paren": {
        "expr": self.inner.to_json(ast)
      }
    }
  }
}

impl ToJson for expr::Assignment<'_> {
  fn to_json(&self, ast: &AST) -> JsonValue {
    object! {
      "Assignment": {
        "var_name": self.var_name,
        "rhs": self.rhs.to_json(ast)
      }
    }
  }
}

impl ToJson for expr::Binary<'_> {
  fn to_json(&self, ast: &AST) -> JsonValue {
    object! {
      "Binary": {
        "left": self.left.to_json(ast),
        "operator": self.operator.to_json(ast),
        "right": self.right.to_json(ast)
      }
    }
  }
}

impl ToJson for expr::Unary<'_> {
  fn to_json(&self, ast: &AST) -> JsonValue {
    object! {
      "Unary": {
        "operator": self.operator.to_json(ast),
        "right": self.right.to_json(ast)
      }
    }
  }
}

impl ToJson for expr::Lambda<'_> {
  fn to_json(&self, ast: &AST) -> JsonValue {
    object! {
      "Lambda": {
        "parameter_names": self.parameter_names.clone(),
        "parameter_types": self.parameter_types.to_json(ast),
        "return_type": self.return_type.to_json(ast),
        "body": self.body.to_json(ast),
      }
    }
  }
}

impl ToJson for expr::FnCall {
  fn to_json(&self, ast: &AST) -> JsonValue {
    object! {
      "FnCall": {
        "func": self.func.to_json(ast),
        "arguments": self.arguments.to_json(ast)
      }
    }
  }
}

impl ToJson for expr::MemberGet<'_> {
  fn to_json(&self, ast: &AST) -> JsonValue {
    object! {
      "MemberGet": {
        "lhs": self.lhs.to_json(ast),
        "member_name": self.member_name
      }
    }
  }
}

impl ToJson for expr::MemberSet<'_> {
  fn to_json(&self, ast: &AST) -> JsonValue {
    object! {
      "MemberSet": {
        "lhs": self.lhs.to_json(ast),
        "member_name": self.member_name,
        "value": self.value.to_json(ast)
      }
    }
  }
}

impl ToJson for expr::DotCall<'_> {
  fn to_json(&self, ast: &AST) -> JsonValue {
    object! {
      "DotCall": {
        "lhs": self.lhs.to_json(ast),
        "function_name": self.function_name,
        "arguments": self.arguments.to_json(ast)
      }
    }
  }
}

impl ToJson for expr::Construct<'_> {
  fn to_json(&self, ast: &AST) -> JsonValue {
    object! {
      "Construct": {
        "type_name": self.type_name,
        "arguments": self.arguments.to_json(ast),
      }
    }
  }
}

impl ToJson for StmtHandle {
  fn to_json(&self, ast: &AST) -> JsonValue {
    match self.get_stmt(ast) {
      Stmt::VarDecl(v) => v.to_json(ast),
      Stmt::StmtExpr(s) => s.to_json(ast),
      Stmt::Block(b) => b.to_json(ast),
      Stmt::IfBranch(i) => i.to_json(ast),
      Stmt::While(w) => w.to_json(ast),
      Stmt::FunctionDefinition(f) => f.to_json(ast),
      Stmt::FunctionDeclaration(f) => f.to_json(ast),
      Stmt::ForeignFunction(f) => f.to_json(ast),
      Stmt::Break => "Break".into(),
      Stmt::Return(r) => r.to_json(ast),
      Stmt::StructDeclaration(s) => s.to_json(ast),
      Stmt::StructDefinition(s) => s.to_json(ast),
      Stmt::Import(i) => i.to_json(ast),
      Stmt::ModuleDecl(m) => m.to_json(ast),
    }
  }
}

impl ToJson for Vec<StmtHandle> {
  fn to_json(&self, ast: &AST) -> JsonValue {
    self
      .iter()
      .map(|s| s.to_json(ast))
      .collect::<Vec<_>>()
      .into()
  }
}

impl ToJson for stmt::VarDecl<'_> {
  fn to_json(&self, ast: &AST) -> JsonValue {
    object! {
      "VarDecl": {
        "name": self.name,
        "specified_type": self.specified_type.to_json(ast),
        "init_expr": self.init_expr.to_json(ast)
      }
    }
  }
}

impl ToJson for stmt::StmtExpr {
  fn to_json(&self, ast: &AST) -> JsonValue {
    object! {
      "StmtExpr": {
        "expr": self.expr.to_json(ast)
      }
    }
  }
}

impl ToJson for stmt::Block {
  fn to_json(&self, ast: &AST) -> JsonValue {
    object! {
      "Block": {
        "statements": self.statements.to_json(ast)
      }
    }
  }
}

impl ToJson for stmt::IfBranch {
  fn to_json(&self, ast: &AST) -> JsonValue {
    let else_branch = if let Some(stmt) = self.else_branch {
      stmt.to_json(ast)
    } else {
      JsonValue::Null
    };
    object! {
      "IfBranch": {
        "condition": self.condition.to_json(ast),
        "true branch": self.true_branch.to_json(ast),
        "else branch": else_branch
      }
    }
  }
}

impl ToJson for stmt::While {
  fn to_json(&self, ast: &AST) -> JsonValue {
    object! {
      "While": {
        "condition": self.condition.to_json(ast),
        "body": self.loop_body.to_json(ast)
      }
    }
  }
}

impl ToJson for stmt::FunctionDefinition<'_> {
  fn to_json(&self, ast: &AST) -> JsonValue {
    object! {
      "FunctionDefinition": {
        "name": self.name,
        "parameter_names": self.parameter_names.clone(),
        "parameter_types": self.parameter_types.to_json(ast),
        "return_type": self.return_type.to_json(ast),
        "body": self.body.to_json(ast)
      }
    }
  }
}

impl ToJson for stmt::FunctionDeclaration<'_> {
  fn to_json(&self, ast: &AST) -> JsonValue {
    object! {
      "FunctionDeclaration": {
        "name": self.name,
        "parameter_names": self.parameter_names.clone(),
        "parameter_types": self.parameter_types.to_json(ast),
        "return_type": self.return_type.to_json(ast)
      }
    }
  }
}

impl ToJson for stmt::ForeignFunction<'_> {
  fn to_json(&self, ast: &AST) -> JsonValue {
    object! {
      "ForeignalFunction": {
        "name": self.name,
        "parameter_names": self.parameter_names.clone(),
        "parameter_types": self.parameter_types.to_json(ast),
        "return_type": self.return_type.to_json(ast)
      }
    }
  }
}

impl ToJson for stmt::Return {
  fn to_json(&self, ast: &AST) -> JsonValue {
    let ret_expr = if let Some(expr) = self.expr {
      expr.to_json(ast)
    } else {
      JsonValue::Null
    };
    object! {
      "Return": {
        "expr": ret_expr
      }
    }
  }
}

impl ToJson for stmt::StructDeclaration<'_> {
  fn to_json(&self, _ast: &AST) -> JsonValue {
    object! {
      "StructDeclaration": {
        "name": self.name
      }
    }
  }
}

impl ToJson for stmt::StructDefinition<'_> {
  fn to_json(&self, ast: &AST) -> JsonValue {
    object! {
      "StructDefinition": {
        "name": self.name,
        "member_names": self.member_names.clone(),
        "member_types": self.member_types.to_json(ast)
      }
    }
  }
}

impl ToJson for stmt::Import<'_> {
  fn to_json(&self, _ast: &AST) -> JsonValue {
    object! {
      "Import": {
        "module_name": self.module_name
      }
    }
  }
}

impl ToJson for stmt::ModuleDecl<'_> {
  fn to_json(&self, _ast: &AST) -> JsonValue {
    object! {
      "ModuleDecl": {
        "name": self.name
      }
    }
  }
}

impl ToJson for Vec<TypeHandle> {
  fn to_json(&self, ast: &AST) -> JsonValue {
    self
      .iter()
      .map(|t| t.to_json(ast))
      .collect::<Vec<_>>()
      .into()
  }
}

impl ToJson for TypeHandle {
  fn to_json(&self, ast: &AST) -> JsonValue {
    self.get_type(ast).to_json(ast)
  }
}

impl ToJson for ParsedType<'_> {
  fn to_json(&self, _: &AST) -> JsonValue {
    match self {
      ParsedType::Num => "num".into(),
      ParsedType::Bool => "bool".into(),
      ParsedType::Str => "str".into(),
      ParsedType::Function(_) => todo!(),
      ParsedType::Named(s) => object! {named: s.to_string()},
      ParsedType::Nothing => "nothing".into(),
      _ => todo!(),
    }
  }
}

#[allow(unused)]
pub fn to_json(ast: &AST) -> JsonValue {
  let program = ast
    .get_program()
    .iter()
    .map(|s| s.to_json(ast))
    .collect::<Vec<_>>();
  JsonValue::Array(program)
}

#[allow(unused)]
pub fn to_json_string(ast: &AST) -> String {
  json::stringify_pretty(to_json(ast), 1)
}
