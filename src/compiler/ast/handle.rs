use super::{Expr, Stmt, AST};
use crate::compiler::ast::parsed_type::ParsedType;
use crate::compiler::lexer::SourceRange;

macro_rules! generate_ast_handle {
  ($name:ident) => {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct $name {
      pub(super) index: u32,
    }

    impl $name {
      pub const INVALID: Self = Self { index: u32::MAX };

      pub(super) fn new(index: u32) -> Self {
        Self { index }
      }

      pub fn is_invalid(self) -> bool {
        self == Self::INVALID
      }
    }
  };
}

generate_ast_handle!(ExprHandle);
impl ExprHandle {
  pub fn get_expr<'ast, 'src>(self, ast: &'ast AST<'src>) -> &'ast Expr<'src> {
    assert!(self.index < ast.expressions.len() as u32);
    &ast.expressions[self.index as usize]
  }

  pub fn get_source_range(self, ast: &AST) -> SourceRange {
    if self.is_invalid() {
      SourceRange::EMPTY
    } else {
      assert!(self.index < ast.expression_source_range.len() as u32);
      ast.expression_source_range[self.index as usize]
    }
  }
}

generate_ast_handle!(StmtHandle);
impl StmtHandle {
  pub fn get_stmt<'ast, 'src>(self, ast: &'ast AST<'src>) -> &'ast Stmt<'src> {
    assert!(self.index < ast.statements.len() as u32);
    &ast.statements[self.index as usize]
  }

  pub fn get_source_range(self, ast: &AST) -> SourceRange {
    if self.is_invalid() {
      SourceRange::EMPTY
    } else {
      assert!(self.index < ast.statement_source_range.len() as u32);
      ast.statement_source_range[self.index as usize]
    }
  }
}

generate_ast_handle!(TypeHandle);

impl TypeHandle {
  pub const NUM: Self = TypeHandle { index: 0 };
  pub const STR: Self = TypeHandle { index: 1 };
  pub const BOOL: Self = TypeHandle { index: 2 };
  pub const NOTHING: Self = TypeHandle { index: 3 };

  pub const ANY: Self = TypeHandle { index: 4 };

  pub fn get_type<'ast, 'src>(self, ast: &'ast AST<'src>) -> &'ast ParsedType<'src> {
    assert!(self.index < ast.types.len() as u32);
    &ast.types[self.index as usize]
  }
}
