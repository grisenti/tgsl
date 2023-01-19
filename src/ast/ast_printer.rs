use super::*;

pub struct ASTPrinter {
  spaces: i32,
  flat: bool,
}

impl ASTPrinter {
  pub fn print(root: &mut dyn ASTNode, flat: bool) {
    root.accept(&mut Self { spaces: 0, flat });
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
  fn visit_binary_expr(&mut self, exp: &mut BinaryExpr) {
    self.print_with_spaces("(");
    self.change_spaces(1);
    exp.left.accept(self);
    self.print_with_spaces(exp.operator.lexeme);
    exp.right.accept(self);
    self.change_spaces(-1);
    self.print_with_spaces(")");
  }

  fn visit_unary_expr(&mut self, exp: &mut UnaryExpr) {
    self.print_with_spaces("(");
    self.change_spaces(1);
    self.print_with_spaces(exp.operator.lexeme);
    exp.right.accept(self);
    self.change_spaces(-1);
    self.print_with_spaces(")");
  }

  fn visit_literal_expr(&mut self, exp: &mut Literal) {
    self.print_with_spaces(exp.token.lexeme);
  }
}
