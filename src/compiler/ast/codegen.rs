use super::{Expr, ExprHandle, Stmt, StmtHandle, AST};

pub fn desugar_expr(ast: &AST, expr: ExprHandle) -> String {
  match ast.get_expression(expr) {
    Expr::BinaryExpr {
      left,
      operator,
      right,
    } => format!(
      "{} {} {}",
      desugar_expr(ast, left),
      operator.op,
      desugar_expr(ast, right)
    ),
    Expr::UnaryExpr { operator, right } => format!("{} {}", operator.op, desugar_expr(ast, right)),
    Expr::Literal { literal, info: _ } => format!("{}", literal.display(ast)),
    Expr::Variable { id, id_info: _ } => format!("{}", ast.get_str(id)),
    Expr::Assignment {
      id: name,
      id_info: _,
      value,
    } => format!("{} = {}", ast.get_str(name), desugar_expr(ast, value)),
    Expr::FnCall {
      func,
      call_info: _,
      arguments,
    } => format!(
      "{}({})",
      desugar_expr(ast, func),
      arguments
        .iter()
        .cloned()
        .map(|arg| format!(", {}", desugar_expr(ast, arg)))
        .collect::<String>()
    ),
  }
}

pub fn desugar_stmt(ast: &AST, stmt: StmtHandle, spaces: String) -> String {
  let mut result = String::new();
  match ast.get_statement(stmt) {
    Stmt::VarDecl {
      identifier,
      id_info: _,
      expression,
    } => {
      result = format!(
        "{}var {}{};\n",
        &spaces,
        ast.get_str(identifier),
        if let Some(exp) = expression {
          format!(" = {}", desugar_expr(ast, exp))
        } else {
          "".to_string()
        }
      );
    }
    Stmt::Expr(exp) => result = format!("{}{};\n", spaces, desugar_expr(ast, exp)),
    Stmt::Print(expression) => {
      result = format!("{}print {};\n", spaces, desugar_expr(ast, expression))
    }
    Stmt::Block(stmts) => {
      result += &format!("{}{{\n", &spaces);
      for s in stmts {
        result += &format!("{}", desugar_stmt(ast, s, format!("{}  ", &spaces)));
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
        format!(
          "{}else\n{}",
          &spaces,
          desugar_stmt(ast, stmt, spaces.clone())
        )
      } else {
        String::new()
      };
      result = format!(
        "{}if ({})\n{}{}",
        &spaces,
        desugar_expr(ast, condition),
        desugar_stmt(ast, true_branch, spaces.clone()),
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
        desugar_expr(ast, condition),
        desugar_stmt(ast, loop_body, spaces.clone())
      )
    }
    Stmt::Break => result = format!("{}break;\n", &spaces),
    Stmt::Return(expr) => result = format!("{}return {};\n", &spaces, desugar_expr(ast, expr)),
    Stmt::Function {
      name,
      name_info: _,
      parameters,
      body,
    } => {
      result = format!("{spaces}fun {} (", ast.get_str(name));
      for p in parameters {
        result += &format!(", {}", ast.get_str(p));
      }
      result += &format!(")\n{spaces}{{\n");
      for stmt in body {
        result += &desugar_stmt(ast, stmt, format!("  {spaces}"))
      }
      result += &format!("{spaces}}}\n");
    }
    _ => panic!(),
  }
  result
}

pub fn desugar(ast: &AST) -> String {
  let mut result = String::new();
  for stmt in ast.get_program() {
    result += &desugar_stmt(ast, stmt.clone(), "".to_string());
  }
  result
}
