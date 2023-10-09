use std::rc::Rc;

use crate::compiler::ast::expression::expr::{
  Assignment, Binary, Construct, DotCall, FnCall, Id, Lambda, Literal, MemberGet, MemberSet, Paren,
  Unary,
};
use crate::compiler::ast::expression::Expr;
use crate::compiler::ast::visitor::{ExprVisitor, ParsedTypeVisitor};
use crate::compiler::ast::{ExprHandle, AST};
use crate::compiler::codegen::bytecode::{ConstantValue, OpCode};
use crate::compiler::errors::{sema_err, ty_err, CompilerError};
use crate::compiler::functions::overload_set::{FunctionAddress, ResolvedOverload};
use crate::compiler::functions::RelativeFunctionAddress;
use crate::compiler::lexer::{SourceRange, Token};
use crate::compiler::semantics::environment::Capture;
use crate::compiler::semantics::SemanticChecker;
use crate::compiler::structs::{MemberIndex, StructGetError};
use crate::compiler::types::{parameter_types_to_string, FunctionSignature, Type};

#[rustfmt::skip]
const BINARY_OPERATORS: &[(Token, Type, Type, Type, OpCode)] = &[
  // number
  (Token::Basic('+'), Type::Num, Type::Num, Type::Num, OpCode::AddNum),
  (Token::Basic('-'), Type::Num, Type::Num, Type::Num, OpCode::SubNum),
  (Token::Basic('*'), Type::Num, Type::Num, Type::Num, OpCode::MulNum),
  (Token::Basic('/'), Type::Num, Type::Num, Type::Num, OpCode::DivNum),
  (Token::Basic('<'), Type::Num, Type::Num, Type::Bool, OpCode::LeNum),
  (Token::Basic('>'), Type::Num, Type::Num, Type::Bool, OpCode::GeNum),
  (Token::Leq, Type::Num, Type::Num, Type::Bool, OpCode::LeqNum),
  (Token::Geq, Type::Num, Type::Num, Type::Bool, OpCode::GeqNum),
  (Token::Same, Type::Num, Type::Num, Type::Bool, OpCode::SameNum),
  (Token::Different, Type::Num, Type::Num, Type::Bool, OpCode::DiffNum),
  // string Token
  (Token::Basic('+'), Type::Str, Type::Str, Type::Str, OpCode::AddStr),
  (Token::Basic('<'), Type::Str, Type::Str, Type::Bool, OpCode::LeStr),
  (Token::Basic('>'), Type::Str, Type::Str, Type::Bool, OpCode::GeStr),
  (Token::Leq, Type::Str, Type::Str, Type::Bool, OpCode::LeqStr),
  (Token::Geq, Type::Str, Type::Str, Type::Bool, OpCode::GeqStr),
  (Token::Same, Type::Str, Type::Str, Type::Bool, OpCode::SameStr),
  (Token::Different, Type::Str, Type::Str, Type::Bool, OpCode::DiffStr),
  // bool
  (Token::Same, Type::Bool, Type::Bool, Type::Bool, OpCode::SameBool),
  (Token::Different, Type::Bool, Type::Bool, Type::Bool, OpCode::DiffBool),
];

#[rustfmt::skip]
const UNARY_OPERATORS: &[(Token, Type, Type, OpCode)] = &[
  (Token::Basic('-'), Type::Num, Type::Num, OpCode::NegNum),
  (Token::Basic('!'), Type::Bool, Type::Bool, OpCode::NotBool),
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
        argument,
        param,
      ));
    }
  }
}

impl<'a> ExprVisitor<'a, 'a, Type> for SemanticChecker<'a> {
  fn visit_literal(&mut self, _ast: &'a AST, literal: &Literal, _: ExprHandle) -> Type {
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
            .push_constant(ConstantValue::Bool(literal.value == Token::True));
        }
        Type::Bool
      }
      _ => panic!("non literal as literal value"),
    }
  }

  fn visit_id(&mut self, ast: &'a AST, id: &Id, expr_handle: ExprHandle) -> Type {
    self.generate_var_get(id.id, expr_handle.get_source_range(ast))
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
    let rhs_type = self.visit_expr(ast, assignment.rhs);

    let var_type = self.generate_assignment(ast, assignment.var_name, expr_handle);
    if !var_type.is_error() && !rhs_type.is_error() && var_type != rhs_type {
      let var_type = var_type.clone(); // we already borrowed self with get_variable
      self.emit_error(ty_err::assignment_of_incompatible_types(
        expr_handle.get_source_range(ast),
        &rhs_type,
        &var_type,
      ));
      Type::Error
    } else {
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
          &lhs,
          &rhs,
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
      unsafe { self.code().push_op(bin_op) };
      expr_type
    } else {
      self.emit_error(ty_err::incorrect_binary_operator(
        expr_handle.get_source_range(ast),
        binary.operator,
        &lhs,
        &rhs,
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
      unsafe { self.code().push_op(unary_op) };
      expr_type
    } else {
      self.emit_error(ty_err::incorrect_unary_operator(
        expr_handle.get_source_range(ast),
        unary.operator,
        &rhs,
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
    let parameter_types = self.convert_type_list(&lambda.parameter_types);
    let return_type = self.visit_parsed_type(ast, lambda.return_type);
    let expr_sr = expr_handle.get_source_range(ast);

    self.start_lambda(&parameter_types, return_type.clone());
    self.declare_function_parameters(&lambda.parameter_names, &parameter_types, expr_sr);
    self.visit_function_body(&lambda.body, expr_sr);
    let (function_id, captures) = self.end_lambda();
    unsafe {
      self
        .code()
        .push_constant(ConstantValue::RelativeNativeFn(function_id));
      self.create_closure(&captures);
    };
    FunctionSignature::new(parameter_types, return_type).into()
  }

  fn visit_fn_call(&mut self, ast: &'a AST, fn_call: &FnCall, expr_handle: ExprHandle) -> Type {
    let arguments = self.visit_expr_list(ast, &fn_call.arguments);
    let call_sr = expr_handle.get_source_range(ast);
    let expr_type = if let Expr::Id(id) = fn_call.func.get_expr(ast) {
      self.call_function(&arguments, id.id, call_sr)
    } else {
      let lhs_type = self.visit_expr(ast, fn_call.func);
      self.call_value(&lhs_type, &arguments, call_sr)
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

    if let Some((member_type, member_index)) = self.get_struct_member(
      member_get.member_name,
      &lhs_type,
      expr_handle.get_source_range(ast),
    ) {
      let member_type = member_type.clone();
      unsafe {
        self
          .code()
          .push_op2(OpCode::GetMember, member_index.get_index() as u8)
      };
      member_type
    } else {
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

    if let Some((member_type, member_index)) = self.get_struct_member(
      member_set.member_name,
      &lhs_type,
      expr_handle.get_source_range(ast),
    ) {
      let member_type = member_type.clone();
      self.visit_expr(ast, member_set.value);
      unsafe {
        self
          .code()
          .push_op2(OpCode::SetMember, member_index.get_index() as u8)
      };
      member_type
    } else {
      Type::Error
    }
  }

  fn visit_dot_call(&mut self, ast: &'a AST, dot_call: &DotCall, expr_handle: ExprHandle) -> Type {
    // if rhs is member, call it
    // otherwise, ensure name is a function and try to call it
    let call_sr = expr_handle.get_source_range(ast);
    let start = self.code().get_next_instruction_address();
    let lhs_type = self.visit_expr(ast, dot_call.lhs);
    let mid = self.code().get_next_instruction_address();
    let mut arguments = self.visit_expr_list(ast, &dot_call.arguments);
    let end = self.code().get_next_instruction_address();

    if let Type::Struct { name, .. } = &lhs_type {
      match self.env.global_structs().get(&name) {
        Ok(struct_) => {
          if let Some(member_index) = struct_.get_member_index(dot_call.function_name) {
            let (_, member_type) = struct_.member_info(member_index);
            if let Type::Function(signature) = member_type {
              let return_type = signature.get_return_type().clone();
              check_arguments(
                &mut self.errors,
                signature.get_parameters(),
                &arguments,
                expr_handle.get_source_range(ast),
              );
              self.code().swap(start, mid, end);
              unsafe {
                self
                  .code()
                  .push_op2(OpCode::GetMember, member_index.get_index() as u8);
                self
                  .code()
                  .push_op2(OpCode::CallValue, arguments.len() as u8);
              }
              return return_type;
            } // its not a function
          } // its not a member
        }
        Err(StructGetError::NotDefined) => {
          todo!("error: struct was not defined, we don't know if the right side is a member")
        }
        _ => {}
      }
    }
    arguments.push(lhs_type);
    self.call_function(&arguments, dot_call.function_name, call_sr)
  }

  fn visit_constructor(
    &mut self,
    ast: &'a AST,
    constructor: &Construct,
    expr_handle: ExprHandle,
  ) -> Type {
    let expr_sr = expr_handle.get_source_range(ast);
    let arguments = self.visit_expr_list(ast, &constructor.arguments);
    match self.env.global_structs().get(constructor.type_name) {
      Ok(struct_) => {
        let type_ = Type::Struct {
          name: struct_.clone_name(),
          module_name: self
            .module_name
            .clone()
            .unwrap_or(Rc::from(Type::ANONYMOUS_MODULE)),
        };
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
        type_
      }
      Err(StructGetError::NotAStruct) => {
        self.emit_error(ty_err::not_struct_name(expr_sr, constructor.type_name));
        Type::Error
      }
      Err(StructGetError::NotDefined) => {
        todo!("error: struct was not defined");
      }
      Err(StructGetError::MultipleDefinitions) | Err(StructGetError::RedefinedImport) => {
        Type::Error
      }
    }
  }
}

impl SemanticChecker<'_> {
  fn generate_var_get(&mut self, var_name: &str, var_sr: SourceRange) -> Type {
    if let Some((local_id, var_type)) = self.env.get_local_var(var_name) {
      unsafe { self.code().push_op2(OpCode::GetLocal, local_id) };
      var_type.clone()
    } else if let Some((capture_id, var_type)) = self.env.get_capture_or_capture_var(var_name) {
      unsafe { self.code().push_op2(OpCode::GetCapture, capture_id) };
      var_type.clone()
    } else if let Some((address, var_type)) = self.env.get_global_var(var_name) {
      unsafe {
        self.code().push_constant(address.into());
        self.code().push_op(OpCode::GetGlobal);
      };
      var_type.clone()
    } else if let Some(overload) = self.env.global_functions().get_overload_set(var_name) {
      match overload.auto_resolve() {
        Ok(resolved_overload) => {
          unsafe {
            self
              .code()
              .push_constant(resolved_overload.function_address.into())
          };
          resolved_overload.function_signature.clone().into()
        }
        Err(_) => {
          todo!()
        }
      }
    } else if self.env.is_type(var_name) {
      todo!("error: cannot access value of a type")
    } else {
      self.errors.push(sema_err::name_not_found(var_sr, var_name));
      Type::Error
    }
  }

  fn generate_assignment(&mut self, ast: &AST, var_name: &str, expr_handle: ExprHandle) -> Type {
    if let Some((local_id, var_type)) = self.env.get_local_var(var_name) {
      unsafe { self.code().push_op2(OpCode::SetLocal, local_id) };
      var_type
    } else if let Some((capture_id, var_type)) = self.env.get_capture_or_capture_var(var_name) {
      unsafe { self.code().push_op2(OpCode::SetCapture, capture_id) };
      var_type
    } else if let Some((address, var_type)) = self.env.get_global_var(var_name) {
      unsafe {
        self.code().push_constant(address.into());
        self.code().push_op(OpCode::SetGlobal);
      };
      var_type.clone()
    } else if self
      .env
      .global_functions()
      .get_overload_set(var_name)
      .is_some()
    {
      todo!("error: left hand side of assignment is a function");
    } else if self.env.is_type(var_name) {
      todo!("error: left hand side of assignment is a type")
    } else {
      self.errors.push(sema_err::name_not_found(
        expr_handle.get_source_range(ast),
        var_name,
      ));
      Type::Error
    }
  }

  fn start_lambda(&mut self, parameter_types: &[Type], return_type: Type) {
    let name = format!("<lambda>({})", parameter_types_to_string(parameter_types));
    self.env.push_function(name, return_type);
  }

  fn end_lambda(&mut self) -> (RelativeFunctionAddress, Vec<Capture>) {
    self.finalize_function_code();
    let function = self.env.pop_function();
    let function_id = self.env.generate_lambda_address();
    assert_eq!(function_id as usize, self.checked_functions.len());
    self.checked_functions.push(function.code);
    (function_id, function.captures)
  }

  unsafe fn call_resolved_overload(&mut self, resolved_overload: ResolvedOverload) -> Type {
    let return_type = resolved_overload
      .function_signature
      .get_return_type()
      .clone();
    let arguments = resolved_overload.function_signature.get_parameters().len() as u8;
    match resolved_overload.function_address {
      FunctionAddress::RelativeNative(address) => self
        .code()
        .push_constant(ConstantValue::RelativeNativeFn(address))
        .push_op2(OpCode::CallNative, arguments),
      FunctionAddress::RelativeExtern(address) => self
        .code()
        .push_constant(ConstantValue::RelativeExternFn(address))
        .push_op2(OpCode::CallExtern, arguments),
      FunctionAddress::AbsoluteNative(address) => self
        .code()
        .push_constant(ConstantValue::AbsoluteNativeFn(address))
        .push_op2(OpCode::CallNative, arguments),
      FunctionAddress::AbsoluteExtern(address) => self
        .code()
        .push_constant(ConstantValue::AbsoluteExternFn(address))
        .push_op2(OpCode::CallExtern, arguments),
    }
    return_type
  }

  fn call_function(
    &mut self,
    arguments: &[Type],
    function_name: &str,
    call_sr: SourceRange,
  ) -> Type {
    // TODO: improve errors for overload sets with only one element (use `check_arguments`)
    if let Some(overload_set) = self.env.global_functions().get_overload_set(function_name) {
      match overload_set.find(&arguments) {
        Ok(resolved_overload) => unsafe { self.call_resolved_overload(resolved_overload) },
        Err(_) => {
          todo!()
        }
      }
    } else {
      let var_type = self.generate_var_get(function_name, call_sr);
      self.call_value(&var_type, arguments, call_sr)
    }
  }

  fn call_value(&mut self, value_type: &Type, arguments: &[Type], call_sr: SourceRange) -> Type {
    if value_type.is_error() {
      return Type::Error;
    }

    if let Type::Function(signature) = value_type {
      check_arguments(
        &mut self.errors,
        signature.get_parameters(),
        arguments,
        call_sr,
      );
      unsafe {
        self
          .code()
          .push_op2(OpCode::CallValue, arguments.len() as u8)
      };
      signature.get_return_type().clone()
    } else {
      todo!("error: not a function")
    }
  }

  unsafe fn create_closure(&mut self, captures: &[Capture]) {
    if !captures.is_empty() {
      self
        .code()
        .push_op2(OpCode::MakeClosure, captures.len() as u8);
      for c in captures {
        match c {
          Capture::Local(local_address) => self.code().push_op2(OpCode::GetLocal, *local_address),
          Capture::Capture(capture_address) => {
            self.code().push_op2(OpCode::GetCapture, *capture_address)
          }
        }
        self.code().push_op(OpCode::Capture);
      }
    }
  }

  fn get_struct_member(
    &mut self,
    member_name: &str,
    type_: &Type,
    sr: SourceRange,
  ) -> Option<(&Type, MemberIndex)> {
    if let Type::Struct { name, .. } = type_ {
      if let Ok(struct_) = self.env.global_structs().get(name) {
        if let Some(index) = struct_.get_member_index(member_name) {
          let (_, member_type) = struct_.member_info(index);
          Some((member_type, index))
        } else {
          self
            .errors
            .push(ty_err::not_a_member(sr, member_name, struct_.get_name()));
          None
        }
      } else {
        todo!("error: struct was not defined yet")
      }
    } else {
      self.emit_error(ty_err::cannot_access_member_of_non_struct_type(sr, &type_));
      None
    }
  }
}
