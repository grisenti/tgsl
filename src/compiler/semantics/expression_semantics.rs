use std::rc::Rc;

use crate::compiler::ast::expression::{expr, Expr};
use crate::compiler::ast::{ExprHandle, AST};
use crate::compiler::errors::{sema_err, ty_err, CompilerError};
use crate::compiler::functions::overload_set::{FunctionAddress, ResolvedOverload};
use crate::compiler::global_symbols::GlobalSymbol;
use crate::compiler::ir::{
  And, Assignment, BuiltinBinary, BuiltinOperation, BuiltinUnary, CallForeign, CallNative,
  CallValue, Constant, Construct, CreateClosure, ExprIr, ExpressionInfo, FunctionIr, GetMember, Ir,
  IrExprHandle, Or, SetMember, SymbolIndex, VarIndex,
};
use crate::compiler::lexer::{SourceRange, Token};
use crate::compiler::semantics::{
  check_statement_block, convert_parsed_type, convert_type_list, SemanticState,
};
use crate::compiler::structs::{MemberIndex, StructGetError};
use crate::compiler::types::{parameter_types_to_string, FunctionSignature, Type};

#[rustfmt::skip]
const BINARY_OPERATORS: &[(Token, Type, Type, Type, BuiltinOperation)] = &[
  // number
  (Token::Basic('+'), Type::Num, Type::Num, Type::Num, BuiltinOperation::AddNum),
  (Token::Basic('-'), Type::Num, Type::Num, Type::Num, BuiltinOperation::SubNum),
  (Token::Basic('*'), Type::Num, Type::Num, Type::Num, BuiltinOperation::MulNum),
  (Token::Basic('/'), Type::Num, Type::Num, Type::Num, BuiltinOperation::DivNum),
  (Token::Basic('<'), Type::Num, Type::Num, Type::Bool, BuiltinOperation::LeNum),
  (Token::Basic('>'), Type::Num, Type::Num, Type::Bool, BuiltinOperation::GeNum),
  (Token::Leq, Type::Num, Type::Num, Type::Bool, BuiltinOperation::LeqNum),
  (Token::Geq, Type::Num, Type::Num, Type::Bool, BuiltinOperation::GeqNum),
  (Token::Same, Type::Num, Type::Num, Type::Bool, BuiltinOperation::SameNum),
  (Token::Different, Type::Num, Type::Num, Type::Bool, BuiltinOperation::DiffNum),
  // string Token
  (Token::Basic('+'), Type::Str, Type::Str, Type::Str, BuiltinOperation::AddStr),
  (Token::Basic('<'), Type::Str, Type::Str, Type::Bool, BuiltinOperation::LeStr),
  (Token::Basic('>'), Type::Str, Type::Str, Type::Bool, BuiltinOperation::GeStr),
  (Token::Leq, Type::Str, Type::Str, Type::Bool, BuiltinOperation::LeqStr),
  (Token::Geq, Type::Str, Type::Str, Type::Bool, BuiltinOperation::GeqStr),
  (Token::Same, Type::Str, Type::Str, Type::Bool, BuiltinOperation::SameStr),
  (Token::Different, Type::Str, Type::Str, Type::Bool, BuiltinOperation::DiffStr),
  // bool
  (Token::Same, Type::Bool, Type::Bool, Type::Bool, BuiltinOperation::SameBool),
  (Token::Different, Type::Bool, Type::Bool, Type::Bool, BuiltinOperation::DiffBool),
];

#[rustfmt::skip]
const UNARY_OPERATORS: &[(Token, Type, Type, BuiltinOperation)] = &[
  (Token::Basic('-'), Type::Num, Type::Num, BuiltinOperation::NegNum),
  (Token::Basic('!'), Type::Bool, Type::Bool, BuiltinOperation::NotBool),
];

pub struct CheckedExpr {
  pub type_: Type,
  pub handle: IrExprHandle,
}

impl CheckedExpr {
  const ERROR: Self = Self {
    type_: Type::Error,
    handle: IrExprHandle::INVALID,
  };

  fn new<E: Into<ExprIr>>(expr: E, source_range: SourceRange, type_: Type, ir: &mut Ir) -> Self {
    let handle = ir.add_expression(
      expr.into(),
      ExpressionInfo {
        type_: type_.clone(),
        source_range,
      },
    );
    Self { type_, handle }
  }
}

pub trait CheckExprSemantics {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedExpr;
}

impl CheckExprSemantics for ExprHandle {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedExpr {
    state.push_source_range(self.get_source_range(ast));
    let checked_expr = match self.get_expr(ast) {
      Expr::Literal(literal) => literal.check(ast, ir, state),
      Expr::Id(id) => id.check(ast, ir, state),
      Expr::Paren(paren) => paren.check(ast, ir, state),
      Expr::Assignment(assignment) => assignment.check(ast, ir, state),
      Expr::Binary(binary) => binary.check(ast, ir, state),
      Expr::Unary(unary) => unary.check(ast, ir, state),
      Expr::Lambda(lambda) => lambda.check(ast, ir, state),
      Expr::FnCall(fn_call) => fn_call.check(ast, ir, state),
      Expr::MemberGet(member_get) => member_get.check(ast, ir, state),
      Expr::MemberSet(member_set) => member_set.check(ast, ir, state),
      Expr::DotCall(dot_call) => dot_call.check(ast, ir, state),
      Expr::Construct(construct) => construct.check(ast, ir, state),
    };
    state.pop_source_range();
    checked_expr
  }
}

impl CheckExprSemantics for expr::Literal<'_> {
  fn check(&self, _: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedExpr {
    let source_range = state.top_source_range();
    match &self.value {
      Token::Number(value) => CheckedExpr::new(Constant::Num(*value), source_range, Type::Num, ir),
      Token::String(value) => CheckedExpr::new(
        Constant::Str(value.to_string()),
        source_range,
        Type::Str,
        ir,
      ),
      Token::True | Token::False => CheckedExpr::new(
        Constant::Bool(self.value == Token::True),
        source_range,
        Type::Bool,
        ir,
      ),
      _ => panic!("non literal as literal value"),
    }
  }
}

impl CheckExprSemantics for expr::Id<'_> {
  fn check(&self, _: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedExpr {
    let (id, type_) = map_identifier(self.id, state);
    CheckedExpr::new(ExprIr::SymbolIndex(id), state.top_source_range(), type_, ir)
  }
}

impl CheckExprSemantics for expr::Paren {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedExpr {
    self.inner.check(ast, ir, state)
  }
}

impl CheckExprSemantics for expr::Assignment<'_> {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedExpr {
    let (lhs_symbol_index, lhs_type) = map_identifier(self.var_name, state);
    let var_index = match lhs_symbol_index {
      SymbolIndex::Local(index) => VarIndex::Local(index),
      SymbolIndex::Capture(index) => VarIndex::Capture(index),
      SymbolIndex::Global {
        index,
        module_index,
      } => VarIndex::Global {
        index,
        module_index,
      },
      _ => todo!("invalid assignment target"),
    };

    let rhs = self.rhs.check(ast, ir, state);

    // if one of these is an error type, it should have already been reported
    if lhs_type.is_error() || rhs.type_.is_error() {
      return CheckedExpr::ERROR;
    }
    if lhs_type != rhs.type_ {
      state.errors.push(ty_err::assignment_of_incompatible_types(
        state.top_source_range(),
        &lhs_type,
        &rhs.type_,
      ));
      return CheckedExpr::ERROR;
    }
    CheckedExpr::new(
      Assignment {
        target: var_index,
        value: rhs.handle,
      },
      state.top_source_range(),
      lhs_type,
      ir,
    )
  }
}

impl CheckExprSemantics for expr::Binary<'_> {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedExpr {
    let lhs = self.left.check(ast, ir, state);
    let rhs = self.right.check(ast, ir, state);

    // if one of these is an error type, it should have already been reported
    if lhs.type_.is_error() || rhs.type_.is_error() {
      return CheckedExpr::ERROR;
    }

    if lhs.type_ == Type::Bool || rhs.type_ == Type::Bool {
      if self.operator == Token::And {
        return CheckedExpr::new(
          And {
            lhs: lhs.handle,
            rhs: rhs.handle,
          },
          state.top_source_range(),
          Type::Bool,
          ir,
        );
      }
      if self.operator == Token::Or {
        return CheckedExpr::new(
          Or {
            lhs: lhs.handle,
            rhs: rhs.handle,
          },
          state.top_source_range(),
          Type::Bool,
          ir,
        );
      }
    }

    let result = BINARY_OPERATORS
      .iter()
      .filter(|bin_op| bin_op.0 == self.operator)
      .find(|bin_op| bin_op.1 == lhs.type_ && bin_op.2 == rhs.type_)
      .map(|e| (e.3.clone(), e.4));
    if let Some((expr_type, op)) = result {
      CheckedExpr::new(
        BuiltinBinary {
          operation: op,
          lhs: lhs.handle,
          rhs: rhs.handle,
        },
        state.top_source_range(),
        expr_type,
        ir,
      )
    } else {
      state.errors.push(ty_err::incorrect_binary_operator(
        state.top_source_range(),
        &self.operator,
        &lhs.type_,
        &rhs.type_,
      ));
      CheckedExpr::ERROR
    }
  }
}

impl CheckExprSemantics for expr::Unary<'_> {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedExpr {
    let rhs = self.right.check(ast, ir, state);

    // if the right hand side is an error type, it should have already been reported
    if rhs.type_.is_error() {
      return CheckedExpr::ERROR;
    }

    let result = UNARY_OPERATORS
      .iter()
      .find(|(op_token, operand_ty, ..)| *op_token == self.operator && *operand_ty == rhs.type_)
      .map(|(_, _, expression_type, operator)| (expression_type.clone(), *operator));
    if let Some((expr_type, op)) = result {
      CheckedExpr::new(
        BuiltinUnary {
          operation: op,
          operand: rhs.handle,
        },
        state.top_source_range(),
        expr_type,
        ir,
      )
    } else {
      state.errors.push(ty_err::incorrect_unary_operator(
        state.top_source_range(),
        &self.operator,
        &rhs.type_,
      ));
      CheckedExpr::ERROR
    }
  }
}

impl CheckExprSemantics for expr::Lambda<'_> {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedExpr {
    let parameter_types = convert_type_list(&self.parameter_types, ast, state);
    let return_type = convert_parsed_type(self.return_type, ast, state);
    let signature = FunctionSignature::new(parameter_types, return_type);

    let name = Rc::<str>::from(format!(
      "{}::<lambda>({}) -> {}",
      state.env.function_stack(),
      parameter_types_to_string(signature.get_parameters()),
      signature.get_return_type()
    ));

    let function_index = state.open_function(
      name.clone(),
      &self.parameter_names,
      signature.get_parameters(),
      signature.get_return_type().clone(),
    );
    let checked_body = check_statement_block(&self.body, ast, ir, state);
    let captures = state.close_function();

    ir.add_function(FunctionIr {
      name,
      index: function_index,
      signature: signature.clone(),
      source_range: state.top_source_range(),
      instructions: checked_body.block_statements,
    });
    CheckedExpr::new(
      CreateClosure {
        function_id: function_index,
        captures,
      },
      state.top_source_range(),
      signature.into(),
      ir,
    )
  }
}

impl CheckExprSemantics for expr::FnCall {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedExpr {
    if let Expr::Id(id) = self.func.get_expr(ast) {
      call_named(id.id, &self.arguments, ast, ir, state)
    } else {
      let function = self.func.check(ast, ir, state);
      call_value(function, &self.arguments, ast, ir, state)
    }
  }
}

impl CheckExprSemantics for expr::MemberGet<'_> {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedExpr {
    let lhs = self.lhs.check(ast, ir, state);

    if let Some((member_type, member_index)) = struct_member(&lhs.type_, self.member_name, state) {
      CheckedExpr::new(
        GetMember {
          lhs: lhs.handle,
          member_index,
        },
        state.top_source_range(),
        member_type,
        ir,
      )
    } else {
      CheckedExpr::ERROR
    }
  }
}

impl CheckExprSemantics for expr::MemberSet<'_> {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedExpr {
    let lhs = self.lhs.check(ast, ir, state);
    if let Some((member_type, member_index)) = struct_member(&lhs.type_, self.member_name, state) {
      let value = self.value.check(ast, ir, state);

      if value.type_ != member_type {
        todo!("invalid assignment")
      }

      CheckedExpr::new(
        SetMember {
          lhs: lhs.handle,
          member_index,
          value: value.handle,
        },
        state.top_source_range(),
        member_type,
        ir,
      )
    } else {
      CheckedExpr::ERROR
    }
  }
}

impl CheckExprSemantics for expr::DotCall<'_> {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedExpr {
    let lhs = self.lhs.check(ast, ir, state);

    if lhs.type_.is_error() {
      return CheckedExpr::ERROR;
    }

    if let Type::Struct { .. } = &lhs.type_ {
      if let Some((member_type, member_index)) =
        struct_member(&lhs.type_, self.function_name, state)
      {
        let function = CheckedExpr::new(
          GetMember {
            lhs: lhs.handle,
            member_index,
          },
          state.top_source_range(),
          member_type,
          ir,
        );
        call_value(function, &self.arguments, ast, ir, state)
      } else {
        CheckedExpr::ERROR
      }
    } else {
      // not very efficient, but its simple and short
      let mut arguments = self.arguments.clone();
      arguments.insert(0, self.lhs.clone());

      call_named(self.function_name, &self.arguments, ast, ir, state)
    }
  }
}

impl CheckExprSemantics for expr::Construct<'_> {
  fn check(&self, ast: &AST, ir: &mut Ir, state: &mut SemanticState) -> CheckedExpr {
    let (argument_types, argument_handles) = check_expression_list(&self.arguments, ast, ir, state);

    // FIXME: duplicated code from `struct_member`
    let struct_ = if let Some(GlobalSymbol::Struct(s)) = state.env.get_global(self.type_name) {
      s
    } else {
      todo!("error: struct was not defined");
    };

    if argument_types.len() != struct_.get_member_types().len() {
      todo!("error: incorrect number of arguments")
    }

    if argument_types != struct_.get_member_types() {
      todo!("error: incorrect argument types")
    }

    CheckedExpr::new(
      Construct {
        arguments: argument_handles,
      },
      state.top_source_range(),
      Type::Struct {
        name: struct_.get_name(),
        module_name: state.module_name(),
      },
      ir,
    )
  }
}

fn call_named(
  name: &str,
  arguments: &[ExprHandle],
  ast: &AST,
  ir: &mut Ir,
  state: &mut SemanticState,
) -> CheckedExpr {
  let (argument_types, argument_handles) = check_expression_list(arguments, ast, ir, state);

  if let Some(GlobalSymbol::OverloadSet(overload_set)) = state.env.get_global(name) {
    match overload_set.find(&argument_types) {
      Ok(resolved_overload) => call_overload(&resolved_overload, argument_handles, ir, state),
      Err(_) => {
        todo!("no overload error");
      }
    }
  } else {
    todo!("no overload error");
  }
}

fn call_overload(
  overload: &ResolvedOverload,
  arguments: Vec<IrExprHandle>,
  ir: &mut Ir,
  state: &mut SemanticState,
) -> CheckedExpr {
  let return_type = overload.signature.get_return_type().clone();
  match overload.address {
    FunctionAddress::Native { id, module_id } => CheckedExpr::new(
      CallNative {
        function_id: id,
        function_module_id: module_id,
        arguments,
      },
      state.top_source_range(),
      return_type,
      ir,
    ),
    FunctionAddress::Foreign { id, module_id } => CheckedExpr::new(
      CallForeign {
        function_id: id,
        function_module_id: module_id,
        arguments,
      },
      state.top_source_range(),
      return_type,
      ir,
    ),
    _ => panic!(),
  }
}

fn call_value(
  function: CheckedExpr,
  arguments: &[ExprHandle],
  ast: &AST,
  ir: &mut Ir,
  state: &mut SemanticState,
) -> CheckedExpr {
  // if the value we are trying to call is an error type, it should have already been reported
  if function.type_.is_error() {
    return CheckedExpr::ERROR;
  }

  if let Type::Function(signature) = function.type_ {
    let (argument_types, argument_handles) = check_expression_list(arguments, ast, ir, state);
    if argument_types.len() != signature.get_parameters().len() {
      todo!("error: incorrect number of arguments")
    }
    if argument_types != signature.get_parameters() {
      todo!("error: incorrect argument types")
    }

    CheckedExpr::new(
      CallValue {
        value: function.handle,
        arguments: argument_handles,
      },
      state.top_source_range(),
      signature.get_return_type().clone(),
      ir,
    )
  } else {
    todo!("error: not a function")
  }
}

fn check_expression_list(
  args: &[ExprHandle],
  ast: &AST,
  ir: &mut Ir,
  state: &mut SemanticState,
) -> (Vec<Type>, Vec<IrExprHandle>) {
  let it = args.iter().map(|arg| arg.check(ast, ir, state));
  let mut types = Vec::with_capacity(args.len());
  let mut handles = Vec::with_capacity(args.len());
  for checked in it {
    types.push(checked.type_);
    handles.push(checked.handle);
  }
  (types, handles)
}

fn struct_member(
  object_type: &Type,
  member_name: &str,
  state: &mut SemanticState,
) -> Option<(Type, MemberIndex)> {
  // if the left hand side is an error type, it should have already been reported
  if object_type.is_error() {
    return None;
  }

  let struct_name = match object_type {
    Type::Struct { name, .. } => name,
    _ => {
      todo!("not a struct");
    }
  };

  let struct_ = if let Some(GlobalSymbol::Struct(s)) = state.env.get_global(struct_name) {
    s
  } else {
    todo!("error: struct was not defined");
  };

  let member_index = match struct_.get_member_index(member_name) {
    Some(index) => index,
    None => {
      todo!("not a member")
    }
  };
  let (_, member_type) = struct_.member_info(member_index);
  Some((member_type.clone(), member_index))
}

fn check_function_call(
  errors: &mut Vec<CompilerError>,
  parameters: &[Type],
  arguments: &[Type],
  call_sr: SourceRange,
) {
  if parameters.len() != arguments.len() {
    errors.push(ty_err::incorrect_function_argument_number(
      call_sr,
      parameters.len(),
      arguments.len(),
    ));
    return;
  }

  for (index, (param, argument)) in parameters.iter().zip(arguments).enumerate() {
    if *param != *argument && *param != Type::Any {
      errors.push(ty_err::incorrect_function_argument_type(
        call_sr,
        index + 1,
        argument,
        param,
      ));
    }
  }
}
