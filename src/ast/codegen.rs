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
  }
}

pub fn desugar_stmt(stmt: &Stmt) -> String {
  let mut result = String::new();
  match stmt {
    Stmt::VarDecl {
      identifier,
      id_info: _,
      expression,
    } => {
      result = format!(
        "var {}{};\n",
        identifier,
        if let Some(exp) = expression {
          format!(" = {}", desugar_expr(exp))
        } else {
          "".to_string()
        }
      );
    }
    Stmt::Expr(exp) => result = format!("{};\n", desugar_expr(exp)),
    Stmt::Print { expression } => result = format!("print {};\n", desugar_expr(expression)),
    Stmt::Block(stmts) => {
      result += "{\n";
      for s in stmts {
        result += &format!("{}", desugar_stmt(s));
      }
      result += "}\n";
    }
    Stmt::IfBranch {
      if_info: _,
      condition,
      true_branch,
      else_branch,
    } => {
      let else_branch_string = if let Some(stmt) = else_branch {
        format!("else\n{}", desugar_stmt(stmt))
      } else {
        String::new()
      };
      result = format!(
        "if ({})\n{}\n{}",
        desugar_expr(condition),
        desugar_stmt(true_branch),
        else_branch_string
      )
    }
    Stmt::While {
      info: _,
      condition,
      loop_body,
    } => {
      result = format!(
        "while ({})\n{}",
        desugar_expr(condition),
        desugar_stmt(loop_body)
      )
    }
    Stmt::Break => result = "break;\n".to_string(),
    _ => panic!(),
  }
  result
}

pub fn desugar(root: &ASTNode) -> String {
  let mut result = String::new();
  match root {
    ASTNode::Program(prog) => {
      for stmt in prog {
        result += &desugar_stmt(stmt);
      }
    }
    ASTNode::Stmt(stmt) => {
      result = desugar_stmt(stmt);
    }
    ASTNode::Expr(expr) => {
      result = desugar_expr(expr);
    }
  }
  result
}
