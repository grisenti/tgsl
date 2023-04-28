use json::object;
use json::JsonValue;

use crate::compiler::identifier::Identifier;
use crate::compiler::types::TypeId;

use super::*;
use core::fmt::Debug;

impl From<Identifier> for JsonValue {
  fn from(value: Identifier) -> Self {
    JsonValue::String(format!("{value:?}"))
  }
}

impl From<TypeId> for JsonValue {
  fn from(value: TypeId) -> Self {
    JsonValue::String(format!("{}", value.0))
  }
}

impl StmtHandle {
  pub fn to_json(&self, ast: &AST) -> JsonValue {
    match self.get(ast) {
      Stmt::VarDecl {
        identifier,
        var_type,
        expression,
        ..
      } => {
        let init_expr = if let Some(expr) = expression {
          expr.to_json(ast)
        } else {
          JsonValue::Null
        };
        object! {
          "VarDecl": {
            "id": *identifier,
            "type": format!("{var_type:?}"),
            "init expr": init_expr
          }
        }
      }
      Stmt::Expr(expr) => {
        object! {
          "Expr": expr.to_json(ast)
        }
      }
      Stmt::Block { statements, locals } => {
        let stmts = statements
          .iter()
          .map(|s| s.to_json(ast))
          .collect::<Vec<_>>();
        object! {
          "Block": {
            "locals": format!("{locals}"),
            "statements": stmts
          }
        }
      }
      Stmt::IfBranch {
        condition,
        true_branch,
        else_branch,
        ..
      } => {
        let else_branch = if let Some(stmt) = else_branch {
          stmt.to_json(ast)
        } else {
          JsonValue::Null
        };
        object! {
          "IfBranch": {
            "condition": condition.to_json(ast),
            "true branch": true_branch.to_json(ast),
            "else branch": else_branch
          }
        }
      }
      Stmt::While {
        condition,
        loop_body,
        ..
      } => {
        object! {
          "While": {
            "condition": condition.to_json(ast),
            "body": loop_body.to_json(ast)
          }
        }
      }
      Stmt::Function {
        id,
        captures,
        fn_type,
        body,
        return_type,
        ..
      } => {
        let body = body
          .iter()
          .map(|stmt| stmt.to_json(ast))
          .collect::<Vec<_>>();
        object! {
          "Function": {
            "id": *id,
            "captures": captures.as_slice(),
            "function type": *fn_type,
            "return type": *return_type,
            "body": body
          }
        }
      }
      Stmt::ExternFunction {
        name_id: id,
        fn_type,
        extern_id,
        ..
      } => {
        object! {
          "ExternalFunction": {
            "id": *id,
            "function type": *fn_type,
            "extern id": format!("{extern_id:?}")
          }
        }
      }
      Stmt::Break(_) => JsonValue::String("Break".to_string()),
      Stmt::Return { expr, .. } => {
        let ret_expr = if let Some(expr) = expr {
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
      Stmt::Struct {
        id,
        member_names,
        member_types,
        ..
      } => {
        let member_names = member_names
          .iter()
          .map(|handle| handle.get(ast).to_string())
          .collect::<Vec<String>>();
        object! {
          "Struct": {
            "id": *id,
            "member_names": member_names,
            "member_types": member_types.as_slice()
          }
        }
      }
      Stmt::Import { module_id } => {
        object! {
          "Import": {
            "module id": format!("{}", module_id.0)
          }
        }
      }
    }
  }
}

impl ExprHandle {
  pub fn to_json(&self, ast: &AST) -> JsonValue {
    match self.get(ast) {
      Expr::Binary {
        left,
        operator,
        right,
        ..
      } => {
        object! {
          "Binary": {
            "operator": format!("{}", operator),
            "left": left.to_json(ast),
            "right": right.to_json(ast),
          }
        }
      }
      Expr::Logical {
        left,
        operator,
        right,
        ..
      } => {
        object! {
          "Logical": {
            "operator": format!("{}", operator),
            "left": left.to_json(ast),
            "right": right.to_json(ast),
          }
        }
      }
      Expr::Unary {
        operator, right, ..
      } => {
        object! {
          "Unary": {
            "operator": format!("{}", operator),
            "right": right.to_json(ast),
          }
        }
      }
      Expr::Literal { value, .. } => {
        object! {
          "Literal": {
            value: value.display(ast)
          }
        }
      }
      Expr::Variable { id, .. } => {
        object! {
          "Variable": {
            id: *id
          }
        }
      }
      Expr::Assignment { id, value, .. } => {
        object! {
          "Assignment": {
            "id": *id,
            "value": value.to_json(ast)
          }
        }
      }
      Expr::Lambda {
        parameter_types,
        captures,
        function_type_id,
        body,
        return_type,
        ..
      } => {
        let body = body
          .iter()
          .map(|stmt| stmt.to_json(ast))
          .collect::<Vec<_>>();
        object! {
          "Lambda": {
            "captures": captures.as_slice(),
            "function_type_id": *function_type_id,
            "return_type": *return_type,
            "parameter_types": parameter_types.as_slice(),
            "body": body
          }
        }
      }
      Expr::FnCall {
        func, arguments, ..
      } => {
        let arguments = arguments.iter().map(|e| e.to_json(ast)).collect::<Vec<_>>();
        object! {
          "FnCall": {
            "function": func.to_json(ast),
            "arguments": arguments
          }
        }
      }
      Expr::Dot {
        lhs,
        rhs_name,
        rhs_id,
        ..
      } => {
        object! {
          "Dot": {
            "lhs": lhs.to_json(ast),
            "rhs_name": rhs_name.get(ast),
            "rhs_id": *rhs_id
          }
        }
      }
      Expr::Set {
        object,
        member_name,
        value,
        ..
      } => {
        object! {
          "Set": {
            "object": object.to_json(ast),
            "member_name": member_name.get(ast),
            "value": value.to_json(ast)
          }
        }
      }
    }
  }
}

impl Debug for AST {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let out = self
      .program
      .iter()
      .map(|s| s.to_json(self))
      .collect::<Vec<_>>();
    write!(f, "{}", json::stringify_pretty(out, 2))
  }
}
