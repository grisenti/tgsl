use json::object;
use json::JsonValue;

use crate::compiler::identifier::Identifier;
use crate::compiler::types::TypeId;

use super::*;
use core::fmt::Debug;

impl Into<JsonValue> for Identifier {
  fn into(self) -> JsonValue {
    JsonValue::String(format!("{self:?}"))
  }
}

impl Into<JsonValue> for TypeId {
  fn into(self) -> JsonValue {
    JsonValue::String(format!("{}", self.0))
  }
}

impl AST {
  fn print_stmt(&self, stmt: StmtHandle) -> JsonValue {
    match stmt.get(self) {
      Stmt::VarDecl {
        identifier,
        var_type,
        expression,
        ..
      } => {
        let init_expr = if let Some(expr) = expression {
          self.print_expr(*expr)
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
          "Expr": self.print_expr(*expr)
        }
      }
      Stmt::Block { statements, locals } => {
        let stmts = statements
          .iter()
          .map(|s| self.print_stmt(*s))
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
          self.print_stmt(*stmt)
        } else {
          JsonValue::Null
        };
        object! {
          "IfBranch": {
            "condition": self.print_expr(*condition),
            "true branch": self.print_stmt(*true_branch),
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
            "condition": self.print_expr(*condition),
            "body": self.print_stmt(*loop_body)
          }
        }
      }
      Stmt::Function {
        id,
        captures,
        parameters,
        fn_type,
        body,
        return_type,
        ..
      } => {
        let body = body
          .iter()
          .map(|stmt| self.print_stmt(*stmt))
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
          self.print_expr(*expr)
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
        name,
        member_names,
        member_types,
        ..
      } => {
        let member_names = member_names
          .iter()
          .map(|handle| handle.get(self).to_string())
          .collect::<Vec<String>>();
        object! {
          "Struct": {
            "id": *name,
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

  fn print_expr(&self, expr: ExprHandle) -> JsonValue {
    match expr.get(self) {
      Expr::Logical {
        left,
        operator,
        right,
      }
      | Expr::Binary {
        left,
        operator,
        right,
      } => {
        object! {
          "Binary": {
            "operator": format!("{}", operator.op),
            "left": self.print_expr(*left),
            "right": self.print_expr(*right),
          }
        }
      }
      Expr::Unary { operator, right } => {
        object! {
          "Unary": {
            "operator": format!("{}", operator.op),
            "right": self.print_expr(*right),
          }
        }
      }
      Expr::Literal { literal, .. } => {
        object! {
          "Literal": literal.display(self)
        }
      }
      Expr::Variable { id, .. } => {
        object! {
          "Variable": *id
        }
      }
      Expr::Assignment { id, value, .. } => {
        object! {
          "Assignment": {
            "id": *id,
            "value": self.print_expr(*value)
          }
        }
      }
      Expr::Lambda {
        parameters,
        captures,
        fn_type,
        body,
        return_type,
        ..
      } => {
        let body = body
          .iter()
          .map(|stmt| self.print_stmt(*stmt))
          .collect::<Vec<_>>();
        object! {
          "Lambda": {
            "captures": captures.as_slice(),
            "fn type": *fn_type,
            "return type": *return_type,
            "parameters": parameters.as_slice(),
            "body": body
          }
        }
      }
      Expr::FnCall {
        func, arguments, ..
      } => {
        let arguments = arguments
          .iter()
          .map(|e| self.print_expr(*e))
          .collect::<Vec<_>>();
        object! {
          "FnCall": {
            "function": self.print_expr(*func),
            "arguments": arguments
          }
        }
      }
      Expr::Dot {
        lhs,
        name,
        identifier,
        ..
      } => {
        object! {
          "Dot": {
            "rhs name": name.get(self),
            "rhs id": *identifier,
            "lhs expr": self.print_expr(*lhs)
          }
        }
      }
      Expr::Set {
        object,
        name,
        value,
        ..
      } => {
        object! {
          "Set": {
            "object": self.print_expr(*object),
            "name": name.get(self),
            "value": self.print_expr(*value)
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
      .map(|s| self.print_stmt(*s))
      .collect::<Vec<_>>();
    write!(f, "{}", json::stringify_pretty(out, 2))
  }
}
