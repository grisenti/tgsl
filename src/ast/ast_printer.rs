use super::*;

pub struct ASTPrinter {
  spaces: i32,
  flat: bool,
}

impl ASTPrinter {
  pub fn print(root: &ASTNode, flat: bool) {
    (&mut Self { spaces: 0, flat }).visit(root);
  }

  fn print_with_spaces(&self, rest: &str) {
    print!(
      "{}{}{}",
      (0..self.spaces).map(|_| ' ').collect::<String>(),
      rest,
      if !self.flat { '\n' } else { '\0' }
    );
  }

  fn change_spaces(&mut self, inc: i32) {
    if !self.flat {
      self.spaces += inc;
    }
  }
}

impl NodeVisitor for ASTPrinter {
  fn visit_expr(&mut self, expr: &Expr) {
    match expr {
      Expr::BinaryExpr {
        left,
        operator,
        right,
      } => {
        self.print_with_spaces("(");
        self.change_spaces(1);
        self.visit_expr(left);
        self.print_with_spaces(operator.lexeme);
        self.visit_expr(right);
        self.change_spaces(-1);
        self.print_with_spaces(")");
      }
      Expr::UnaryExpr { operator, right } => {
        self.print_with_spaces("(");
        self.change_spaces(1);
        self.print_with_spaces(operator.lexeme);
        self.visit_expr(right);
        self.change_spaces(-1);
        self.print_with_spaces(")");
      }
      Expr::Literal { literal } => {
        self.print_with_spaces(literal.lexeme);
      }
      _ => {}
    }
  }
}
