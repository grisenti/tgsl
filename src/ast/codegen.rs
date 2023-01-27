use crate::lexer::TokenPair;

use super::*;

pub fn desugar_expr(expr: &Expr) -> String {
  match expr {
    Expr::BinaryExpr {
      left,
      operator,
      right,
    } => format!(
      "{} {} {}",
      desugar_expr(left),
      operator.token,
      desugar_expr(right)
    ),
    Expr::Logical {
      left,
      operator,
      right,
    } => format!(
      "{} {} {}",
      desugar_expr(left),
      operator.token,
      desugar_expr(right)
    ),
    Expr::UnaryExpr { operator, right } => format!("{} {}", operator.token, desugar_expr(right)),
    Expr::Literal(TokenPair { token, info: _ }) => format!("{}", token),
    Expr::Variable { id, id_info: _ } => format!("{}", id),
    Expr::Assignment {
      name,
      name_info: _,
      value,
    } => format!("{} = {}", name, desugar_expr(value)),
    Expr::FnCall {
      func,
      call_info: _,
      arguments,
    } => format!(
      "{}({})",
      desugar_expr(func),
      arguments
        .iter()
        .map(|arg| format!(", {}", desugar_expr(arg)))
        .collect::<String>()
    ),
  }
}

pub fn desugar_stmt(stmt: &Stmt, spaces: String) -> String {
  let mut result = String::new();
  match stmt {
    Stmt::VarDecl {
      identifier,
      id_info: _,
      expression,
    } => {
      result = format!(
        "{}var {}{};\n",
        &spaces,
        identifier,
        if let Some(exp) = expression {
          format!(" = {}", desugar_expr(exp))
        } else {
          "".to_string()
        }
      );
    }
    Stmt::Expr(exp) => result = format!("{}{};\n", spaces, desugar_expr(exp)),
    Stmt::Print { expression } => {
      result = format!("{}print {};\n", spaces, desugar_expr(expression))
    }
    Stmt::Block(stmts) => {
      result += &format!("{}{{\n", &spaces);
      for s in stmts {
        result += &format!("{}", desugar_stmt(s, format!("{}  ", &spaces)));
      }
      result += &format!("{}}}\n", &spaces);
    }
    Stmt::IfBranch {
      if_info: _,
      condition,
      true_branch,
      else_branch,
    } => {
      let else_branch_string = if let Some(stmt) = else_branch {
        format!("{}else\n{}", &spaces, desugar_stmt(stmt, spaces.clone()))
      } else {
        String::new()
      };
      result = format!(
        "{}if ({})\n{}{}",
        &spaces,
        desugar_expr(condition),
        desugar_stmt(true_branch, spaces.clone()),
        else_branch_string
      )
    }
    Stmt::While {
      info: _,
      condition,
      loop_body,
    } => {
      result = format!(
        "{}while ({})\n{}",
        &spaces,
        desugar_expr(condition),
        desugar_stmt(loop_body, spaces.clone())
      )
    }
    Stmt::Break => result = format!("{}break;\n", &spaces),
    _ => panic!(),
  }
  result
}

pub fn desugar(root: &ASTNode) -> String {
  let mut result = String::new();
  match root {
    ASTNode::Program(prog) => {
      for stmt in prog {
        result += &desugar_stmt(stmt, "".to_string());
      }
    }
    ASTNode::Stmt(stmt) => {
      result = desugar_stmt(stmt, "".to_string());
    }
    ASTNode::Expr(expr) => {
      result = desugar_expr(expr);
    }
  }
  result
}
