mod ast_printer;
mod expression;

pub use ast_printer::*;
pub use expression::*;

pub trait ASTNode {
  fn accept(&mut self, visitor: &mut dyn NodeVisitor);
}

macro_rules! accept_impl {
  (
    $derived:ty, $base:ty, $lt:lifetime, $visit_name:ident) => {
    impl<$lt> $base for $derived {
      fn accept(&mut self, visitor: &mut dyn NodeVisitor) {
        visitor.$visit_name(self);
      }
    }
  };
}

pub trait NodeVisitor {
  fn visit_binary_expr(&mut self, exp: &mut BinaryExpr);
  fn visit_unary_expr(&mut self, exp: &mut UnaryExpr);
  fn visit_literal_expr(&mut self, exp: &mut Literal);
}

accept_impl!(BinaryExpr<'src>, Expr, 'src, visit_binary_expr);
accept_impl!(UnaryExpr<'src>, Expr, 'src, visit_unary_expr);
accept_impl!(Literal<'src>, Expr, 'src, visit_literal_expr);
