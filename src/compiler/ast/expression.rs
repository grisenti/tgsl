pub mod expr {
  use crate::compiler::ast::{ExprHandle, StmtHandle, TypeHandle};
  use crate::compiler::lexer::Token;

  use super::Expr;

  macro_rules! expr_node {
    ($name:tt, $($member:ident : $t:ty),+) => {
      #[derive(Debug, Clone)]
      pub struct $name {
        $(pub $member: $t),+
      }

      impl<'src> From<$name> for Expr<'src> {
        fn from(value: $name) -> Self {
          Self::$name(value)
        }
      }
    };
  }

  macro_rules! src_expr_node {
    ($name:tt, $($member:ident : $t:ty),+) => {
      #[derive(Debug, Clone)]
      pub struct $name<'src> {
        $(pub $member: $t),+
      }

      impl<'src> From<$name<'src>> for Expr<'src> {
        fn from(value: $name<'src>) -> Self {
          Self::$name(value)
        }
      }
    };
  }

  src_expr_node!(Literal,
    value: Token<'src>
  );

  src_expr_node!(Id,
    id: &'src str
  );

  expr_node!(Paren,
    inner: ExprHandle
  );

  src_expr_node!(Assignment,
    var_name: &'src str,
    rhs: ExprHandle
  );

  src_expr_node!(Binary,
    left: ExprHandle,
    operator: Token<'src>,
    right: ExprHandle
  );

  src_expr_node!(Unary,
    operator: Token<'src>,
    right: ExprHandle
  );

  src_expr_node!(Lambda,
    parameter_names: Vec<&'src str>,
    parameter_types: Vec<TypeHandle>,
    return_type: TypeHandle,
    body: Vec<StmtHandle>
  );

  expr_node!(FnCall,
    func: ExprHandle,
    arguments: Vec<ExprHandle>
  );

  src_expr_node!(MemberGet,
    lhs: ExprHandle,
    member_name: &'src str
  );

  src_expr_node!(MemberSet,
    lhs: ExprHandle,
    member_name: &'src str,
    value: ExprHandle
  );

  src_expr_node!(DotCall,
    lhs: ExprHandle,
    function_name: &'src str,
    arguments: Vec<ExprHandle>
  );

  src_expr_node!(Construct,
    type_name: &'src str,
    arguments: Vec<ExprHandle>
  );
}

use expr::*;

#[derive(Debug, Clone)]
pub enum Expr<'src> {
  Literal(Literal<'src>),
  Id(Id<'src>),
  Paren(Paren),
  Assignment(Assignment<'src>),
  Binary(Binary<'src>),
  Unary(Unary<'src>),
  Lambda(Lambda<'src>),
  FnCall(FnCall),
  MemberGet(MemberGet<'src>),
  MemberSet(MemberSet<'src>),
  DotCall(DotCall<'src>),
  Construct(Construct<'src>),
}
