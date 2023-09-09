use crate::compiler::ast::expression::expr::{
  Assignment, Binary, Construct, DotCall, FnCall, Id, Lambda, Literal, MemberGet, MemberSet, Paren,
  Unary,
};
use crate::compiler::ast::visitor::{ExprVisitor, ParsedTypeVisitor, StmtVisitor};
use crate::compiler::ast::{ExprHandle, AST};
use crate::compiler::codegen::bytecode::{ConstantValue, OpCode};
use crate::compiler::errors::{ty_err, CompilerError};
use crate::compiler::lexer::{SourceRange, Token};
use crate::compiler::operators::{BinaryOperator, UnaryOperator};
use crate::compiler::semantics::environment::ResolvedIdentifier;
use crate::compiler::semantics::SemanticChecker;
use crate::compiler::types::{FunctionSignature, Type};
use std::process::id;

#[rustfmt::skip]
const BINARY_OPERATORS: &[(Token, Type, Type, Type, BinaryOperator)] = &[
  // number
  (Token::Basic('+'), Type::Num, Type::Num, Type::Num, BinaryOperator::AddNum),
  (Token::Basic('-'), Type::Num, Type::Num, Type::Num, BinaryOperator::SubNum),
  (Token::Basic('*'), Type::Num, Type::Num, Type::Num, BinaryOperator::MulNum),
  (Token::Basic('/'), Type::Num, Type::Num, Type::Num, BinaryOperator::DivNum),
  (Token::Basic('<'), Type::Num, Type::Num, Type::Bool, BinaryOperator::LeNum),
  (Token::Basic('>'), Type::Num, Type::Num, Type::Bool, BinaryOperator::GeNum),
  (Token::Leq, Type::Num, Type::Num, Type::Bool, BinaryOperator::LeqNum),
  (Token::Geq, Type::Num, Type::Num, Type::Bool, BinaryOperator::GeqNum),
  (Token::Same, Type::Num, Type::Num, Type::Bool, BinaryOperator::SameNum),
  (Token::Different, Type::Num, Type::Num, Type::Bool, BinaryOperator::DiffNum),
  // string Token
  (Token::Basic('+'), Type::Str, Type::Str, Type::Str, BinaryOperator::AddStr),
  (Token::Basic('<'), Type::Str, Type::Str, Type::Bool, BinaryOperator::LeStr),
  (Token::Basic('>'), Type::Str, Type::Str, Type::Bool, BinaryOperator::GeStr),
  (Token::Leq, Type::Str, Type::Str, Type::Bool, BinaryOperator::LeqStr),
  (Token::Geq, Type::Str, Type::Str, Type::Bool, BinaryOperator::GeqStr),
  (Token::Same, Type::Str, Type::Str, Type::Bool, BinaryOperator::SameStr),
  (Token::Different, Type::Str, Type::Str, Type::Bool, BinaryOperator::DiffStr),
  // bool
  (Token::Same, Type::Bool, Type::Bool, Type::Bool, BinaryOperator::SameBool),
  (Token::Different, Type::Bool, Type::Bool, Type::Bool, BinaryOperator::DiffBool),
];

#[rustfmt::skip]
const UNARY_OPERATORS: &[(Token, Type, Type, UnaryOperator)] = &[
  (Token::Basic('-'), Type::Num, Type::Num, UnaryOperator::NegNum),
  (Token::Basic('!'), Type::Bool, Type::Bool, UnaryOperator::NotBool),
];

fn check_arguments(
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
        argument.print_pretty(),
        param.print_pretty(),
      ));
    }
  }
}

impl<'a> ExprVisitor<'a, 'a, Type> for SemanticChecker<'a> {
  fn visit_literal(&mut self, ast: &'a AST, literal: &Literal, _: ExprHandle) -> Type {
    match literal.value {
      Token::Number(value) => {
        unsafe { self.code().push_constant(ConstantValue::Number(value)) };
        Type::Num
      }
      Token::String(value) => {
        unsafe {
          self
            .code()
            .push_constant(ConstantValue::Str(value.to_string()))
        };
        Type::Str
      }
      Token::True | Token::False => {
        unsafe {
          self
            .code()
            .push_constant(ConstantValue::Bool(literal.value == Token::True))
        }
        Type::Bool
      }
      _ => panic!("non literal as literal value"),
    }
  }

  fn visit_id(&mut self, ast: &'a AST, id: &Id, expr_handle: ExprHandle) -> Type {
    let resolved_id = self.get_id(id.id, expr_handle.get_source_range(ast));
    match resolved_id {
      ResolvedIdentifier::UnresolvedOverload(overload_id) => Type::UnresolvedOverload(overload_id),
      ResolvedIdentifier::ResolvedIdentifier { id, type_ } => {
        let type_ = type_.clone();
        unsafe { self.generate_identifier_code(id) };
        type_
      }
      ResolvedIdentifier::Struct(struct_id) => Type::Struct(struct_id),
      ResolvedIdentifier::Error => Type::Error,
    }
  }

  fn visit_paren(&mut self, ast: &'a AST, paren: &Paren, _: ExprHandle) -> Type {
    self.visit_expr(ast, paren.inner)
  }

  fn visit_assignment(
    &mut self,
    ast: &'a AST,
    assignment: &Assignment,
    expr_handle: ExprHandle,
  ) -> Type {
    let expr_sr = expr_handle.get_source_range(ast);
    let rhs_type = self.visit_expr(ast, assignment.rhs);
    let (var_id, var_type) = self.get_variable(assignment.var_name, expr_sr);
    if !var_type.is_error() && !rhs_type.is_error() && *var_type != rhs_type {
      let var_type = var_type.clone(); // we already borrowed self with get_variable
      self.emit_error(ty_err::assignment_of_incompatible_types(
        expr_sr,
        rhs_type.print_pretty(),
        var_type.print_pretty(),
      ));
      Type::Error
    } else {
      unsafe { self.code().set_variable(var_id) };
      rhs_type
    }
  }

  fn visit_binary(&mut self, ast: &'a AST, binary: &Binary, expr_handle: ExprHandle) -> Type {
    let lhs = self.visit_expr(ast, binary.left);

    if binary.operator == Token::And || binary.operator == Token::Or {
      let jump = if binary.operator == Token::And {
        let jump = unsafe { self.code().push_jump(OpCode::JumpIfFalseNoPop) };
        unsafe { self.code().push_op(OpCode::Pop) };
        jump
      } else {
        let check_rhs = unsafe { self.code().push_jump(OpCode::JumpIfFalseNoPop) };
        let skip_rhs_jump = unsafe { self.code().push_jump(OpCode::Jump) };
        self.code().backpatch_current_instruction(check_rhs);
        unsafe { self.code().push_op(OpCode::Pop) };
        skip_rhs_jump
      };
      let rhs = self.visit_expr(ast, binary.right);
      if lhs == Type::Bool && lhs == rhs {
        self.code().backpatch_current_instruction(jump);
        return Type::Bool;
      } else if !rhs.is_error() || lhs.is_error() {
        self.emit_error(ty_err::incorrect_binary_operator(
          expr_handle.get_source_range(ast),
          binary.operator,
          lhs.print_pretty(),
          rhs.print_pretty(),
        ));
      }
      return Type::Error;
    }

    let rhs = self.visit_expr(ast, binary.right);
    let result = BINARY_OPERATORS
      .iter()
      .filter(|bin_op| bin_op.0 == binary.operator)
      .find(|bin_op| bin_op.1 == lhs && bin_op.2 == rhs)
      .map(|e| (e.3.clone(), e.4));
    if let Some((expr_type, bin_op)) = result {
      unsafe { self.code().push_op(bin_op.into()) };
      expr_type
    } else {
      self.emit_error(ty_err::incorrect_binary_operator(
        expr_handle.get_source_range(ast),
        binary.operator,
        lhs.print_pretty(),
        rhs.print_pretty(),
      ));
      Type::Error
    }
  }

  fn visit_unary(&mut self, ast: &'a AST, unary: &Unary, expr_handle: ExprHandle) -> Type {
    let rhs = self.visit_expr(ast, unary.right);
    let result = UNARY_OPERATORS
      .iter()
      .find(|e| e.0 == unary.operator && e.1 == rhs)
      .map(|e| (e.2.clone(), e.3));
    if let Some((expr_type, unary_op)) = result {
      unsafe { self.code().push_op(unary_op.into()) };
      expr_type
    } else {
      self.emit_error(ty_err::incorrect_unary_operator(
        expr_handle.get_source_range(ast),
        unary.operator,
        rhs.print_pretty(),
      ));
      Type::Error
    }
  }

  fn visit_lambda(
    &mut self,
    ast: &'a AST<'a>,
    lambda: &Lambda<'a>,
    expr_handle: ExprHandle,
  ) -> Type {
    let parameter_types = self.convert_parameter_types(&lambda.parameter_types);
    let return_type = self.visit_parsed_type(ast, lambda.return_type);
    let expr_sr = expr_handle.get_source_range(ast);

    self.start_lambda(return_type.clone());
    self.declare_function_parameters(&lambda.parameter_names, &parameter_types, expr_sr);
    self.visit_function_body(&lambda.body, expr_sr);
    let (function_id, captures) = self.end_lambda();
    unsafe {
      self
        .code()
        .push_constant(ConstantValue::FunctionId(function_id));
      self.code().maybe_create_closure(&captures);
    };
    FunctionSignature::new(parameter_types, return_type).into()
  }

  fn visit_fn_call(&mut self, ast: &'a AST, fn_call: &FnCall, expr_handle: ExprHandle) -> Type {
    let function_expr = self.visit_expr(ast, fn_call.func);
    let call_sr = expr_handle.get_source_range(ast);
    let expr_type = match function_expr {
      Type::Function(signature) => {
        let arguments = self.visit_expr_list(ast, &fn_call.arguments);
        check_arguments(
          &mut self.errors,
          signature.get_parameters(),
          &arguments,
          call_sr,
        );
        signature.get_return_type().clone()
      }
      Type::UnresolvedOverload(overload_id) => {
        let function_id_location = unsafe { self.code().push_stub_constant() };
        let arguments = self.visit_expr_list(ast, &fn_call.arguments);
        if let Some(resolved_overload) = self.env.resolve_overload(overload_id, &arguments) {
          let function_id = resolved_overload.function_id;
          let return_type = resolved_overload
            .function_signature
            .get_return_type()
            .clone();
          unsafe {
            self
              .code()
              .backpatch_constant(function_id_location, ConstantValue::FunctionId(function_id))
          };
          return_type
        } else {
          self.emit_error(ty_err::no_available_oveload(call_sr));
          Type::Error
        }
      }
      Type::Error => Type::Error,
      _ => {
        self.emit_error(ty_err::cannot_call_type(
          call_sr,
          function_expr.print_pretty(),
        ));
        Type::Error
      }
    };
    unsafe {
      self
        .code()
        .push_op2(OpCode::Call, fn_call.arguments.len() as u8)
    };
    expr_type
  }

  fn visit_member_get(
    &mut self,
    ast: &'a AST,
    member_get: &'a MemberGet<'a>,
    expr_handle: ExprHandle,
  ) -> Type {
    let lhs_type = self.visit_expr(ast, member_get.lhs);
    if lhs_type.is_error() {
      return Type::Error;
    }
    if let Type::Struct(struct_id) = lhs_type {
      let struct_ = self.env.get_struct(struct_id).unwrap();
      if let Some(index) = struct_.get_member_index(member_get.member_name) {
        let (_, member_type) = struct_.member_info(index);
        let member_type = member_type.clone();
        unsafe {
          self
            .code()
            .push_op2(OpCode::GetMember, index.get_index() as u8)
        };
        member_type
      } else {
        self.emit_error(ty_err::not_a_member(
          expr_handle.get_source_range(ast),
          member_get.member_name,
          struct_.get_name(),
        ));
        Type::Error
      }
    } else {
      self.emit_error(ty_err::cannot_access_member_of_non_struct_type(
        expr_handle.get_source_range(ast),
        lhs_type.print_pretty(),
      ));
      Type::Error
    }
  }

  fn visit_member_set(
    &mut self,
    ast: &'a AST,
    member_set: &'a MemberSet<'a>,
    expr_handle: ExprHandle,
  ) -> Type {
    let lhs_type = self.visit_expr(ast, member_set.lhs);
    if lhs_type.is_error() {
      return Type::Error;
    }
    if let Type::Struct(struct_id) = lhs_type {
      let struct_ = self.env.get_struct(struct_id).unwrap();
      if let Some(index) = struct_.get_member_index(member_set.member_name) {
        let (_, member_type) = struct_.member_info(index);
        let member_type = member_type.clone();
        self.visit_expr(ast, member_set.value);
        unsafe {
          self
            .code()
            .push_op2(OpCode::SetMember, index.get_index() as u8)
        };
        member_type
      } else {
        self.emit_error(ty_err::not_a_member(
          expr_handle.get_source_range(ast),
          member_set.member_name,
          struct_.get_name(),
        ));
        Type::Error
      }
    } else {
      self.emit_error(ty_err::cannot_access_member_of_non_struct_type(
        expr_handle.get_source_range(ast),
        lhs_type.print_pretty(),
      ));
      Type::Error
    }
  }

  fn visit_dot_call(&mut self, ast: &'a AST, dot_call: &DotCall, expr_handle: ExprHandle) -> Type {
    todo!()
  }

  fn visit_constructor(
    &mut self,
    ast: &'a AST,
    constructor: &Construct,
    expr_handle: ExprHandle,
  ) -> Type {
    let expr_sr = expr_handle.get_source_range(ast);
    let arguments = self.visit_expr_list(ast, &constructor.arguments);
    let struct_id = if let Ok(struct_id) = self.env.get_struct_id(constructor.type_name) {
      struct_id
    } else {
      panic!()
    };
    if let Some(struct_) = self.env.get_struct(struct_id) {
      check_arguments(
        &mut self.errors,
        struct_.get_member_types(),
        &arguments,
        expr_sr,
      );
      unsafe {
        self
          .code()
          .push_op2(OpCode::Construct, constructor.arguments.len() as u8)
      };
      Type::Struct(struct_id)
    } else {
      panic!(); // no struct, maybe incorrect name or we just have a declaration
    }
  }
}
