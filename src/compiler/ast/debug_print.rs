use super::*;
use core::fmt::Debug;

impl AST {
  fn print_stmt(&self, stmt: StmtHandle, depth: u16) -> String {
    let spaces = (0..depth).map(|_| "  ").collect::<String>();
    match stmt.get(self) {
      Stmt::VarDecl {
        identifier,
        var_type,
        expression,
        ..
      } => {
        let init_expr = if let Some(expr) = expression {
          format!("\n{spaces}-init_expr: {}", self.print_expr(expr, depth + 1))
        } else {
          "".to_string()
        };
        format!(
          "\n{spaces}VarDecl:\n{spaces}-id: {identifier:?}\n{spaces}-type: {var_type:?}{init_expr}"
        )
      }
      Stmt::Expr(expr) => {
        format!("\n{spaces}Expr:{}", self.print_expr(expr, depth + 1))
      }
      Stmt::Print(expr) => {
        format!("\n{spaces}Print:{}", self.print_expr(expr, depth + 1))
      }
      Stmt::Block(stmts) => {
        let mut result = format!("\n{spaces}Block:");
        for s in stmts {
          result += &self.print_stmt(s, depth + 1);
        }
        result
      }
      Stmt::IfBranch {
        condition,
        true_branch,
        else_branch,
        ..
      } => {
        let else_banch = if let Some(stmt) = else_branch {
          format!(
            "\n{spaces}-else branch: {}",
            self.print_stmt(stmt, depth + 1)
          )
        } else {
          "".to_string()
        };
        format!(
          "\n{spaces}IfBranch:\n{spaces}-condition:{}\n{spaces}-true branch:{}{else_banch}",
          self.print_expr(condition, depth + 1),
          self.print_stmt(true_branch, depth + 1)
        )
      }
      Stmt::While {
        condition,
        loop_body,
        ..
      } => {
        format!(
          "\n{spaces}While:\n{spaces}-condition: {}\n{spaces}-body: {}",
          self.print_expr(condition, depth + 1),
          self.print_stmt(loop_body, depth + 1)
        )
      }
      Stmt::Function {
        id,
        captures,
        parameters,
        fn_type,
        body,
        ..
      } => {
        let body = body
          .iter()
          .map(|stmt| self.print_stmt(*stmt, depth + 1))
          .collect::<String>();
        format!(
          "\n{spaces}Function:\n{spaces}-id: {id:?}\
          \n{spaces}-captures: {captures:?}\
          \n{spaces}-function type: {fn_type:?}\
          \n{spaces}-parameter ids: {parameters:?}\
          \n{spaces}-body: {body}"
        )
      }
      Stmt::ExternFunction { id, fn_type, .. } => {
        format!(
          "\n{spaces}ExternFunction:\
          \n{spaces}-id: {id:?}\
          \n{spaces}-function type: {fn_type:?}"
        )
      }
      Stmt::Break(_) => {
        format!("\n{spaces}Break")
      }
      Stmt::Return { expr, .. } => {
        format!(
          "\n{spaces}Return:\
          \n{spaces}-expression: {}",
          self.print_expr(expr, depth + 1)
        )
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
        format!(
          "\n{spaces}Struct:\
  				\n{spaces}-id: {name:?}\
  				\n{spaces}-member names: {member_names:?}\
  				\n{spaces}-member_types: {member_types:?}"
        )
      }
    }
  }

  fn print_expr(&self, expr: ExprHandle, depth: u16) -> String {
    let spaces = (0..depth).map(|_| "  ").collect::<String>();
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
        format!(
          "\n{spaces}Binary:\
          \n{spaces}-operator: {}\
          \n{spaces}-left:{}\
          \n{spaces}-right:{}",
          operator.op,
          self.print_expr(left, depth + 1),
          self.print_expr(right, depth + 1)
        )
      }
      Expr::Unary { operator, right } => {
        format!(
          "\n{spaces}Unary:\
          \n{spaces}-operator: {}\
          \n{spaces}-right:{}",
          operator.op,
          self.print_expr(right, depth + 1)
        )
      }
      Expr::Literal { literal, info } => {
        format!("\n{spaces}Literal: {literal:?}")
      }
      Expr::Variable { id, .. } => {
        format!("\n{spaces}Variable:\n{spaces}-id: {id:?}")
      }
      Expr::Assignment { id, value, .. } => {
        format!(
          "\n{spaces}Assignment:\
  				\n{spaces}id: {id:?}\
  				\n{spaces}value: {}",
          self.print_expr(value, depth + 1)
        )
      }
      Expr::Closure {
        parameters,
        captures,
        fn_type,
        body,
        ..
      } => {
        let body = body
          .iter()
          .map(|stmt| self.print_stmt(*stmt, depth + 1))
          .collect::<String>();
        format!(
          "\n{spaces}Closure:\
          \n{spaces}-captures: {captures:?}\
          \n{spaces}-function type: {fn_type:?}\
          \n{spaces}-parameter ids: {parameters:?}\
          \n{spaces}-body: {body}"
        )
      }
      Expr::FnCall {
        func, arguments, ..
      } => {
        format!(
          "\n{spaces}FnCall:\
          \n{spaces}-function: {}\
          \n{spaces}-arguments: {arguments:?}",
          self.print_expr(func, depth + 1)
        )
      }
      Expr::Dot {
        lhs,
        name,
        identifier,
        ..
      } => {
        format!(
          "\n{spaces}Dot:\
          \n{spaces}-rhs-name: '{}'\
          \n{spaces}-rhs-id: {identifier:?}\
          \n{spaces}-lhs-expr: {}",
          name.get(self),
          self.print_expr(lhs, depth + 1)
        )
      }

      Expr::Set {
        object,
        name,
        value,
        ..
      } => {
        format!(
          "\n{spaces}Set:\
          \n{spaces}-object: {}\
          \n{spaces}-name: '{}'\
          \n{spaces}-value: {}",
          self.print_expr(object, depth + 1),
          name.get(self),
          self.print_expr(value, depth + 1)
        )
      }
    }
  }
}

impl Debug for AST {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut result = String::new();
    for stmt in &self.program {
      result += &self.print_stmt(*stmt, 0);
    }
    write!(f, "{}", result)
  }
}
