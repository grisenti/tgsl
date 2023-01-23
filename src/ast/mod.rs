mod ast_printer;
mod expression;
mod statement;

pub use ast_printer::*;
pub use expression::*;
pub use statement::*;

pub enum ASTNode<'src> {
  Expr(Expr<'src>),
  Stmt(Stmt<'src>),
  Program(Vec<Stmt<'src>>),
}

pub trait NodeVisitor {
  fn visit(&mut self, root: &ASTNode) {
    match root {
      ASTNode::Expr(exp) => self.visit_expr(&exp),
      _ => {}
    }
  }

  fn visit_expr(&mut self, expr: &Expr);
}
