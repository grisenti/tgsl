use crate::compiler::{
  ast::{Expr, ExprHandle, Literal, Operator, StmtHandle, StrHandle},
  bytecode::{ConstantValue, OpCode},
  codegen::Address,
  errors::sema_err,
  identifier::Identifier,
  lexer::SourceRange,
  types::{Type, TypeId},
};

use super::FunctionAnalizer;

const ARITHMETIC_OPERATORS: [Operator; 4] = [
  Operator::Basic('+'),
  Operator::Basic('-'),
  Operator::Basic('*'),
  Operator::Basic('/'),
];
const COMP_OPERATORS: [Operator; 6] = [
  Operator::Basic('<'),
  Operator::Basic('>'),
  Operator::Leq,
  Operator::Geq,
  Operator::Same,
  Operator::Different,
];

enum CallType {
  DotCall {
    first_argument: TypeId,
    fn_params: Vec<TypeId>,
    fn_ret: TypeId,
  },
  Call(TypeId),
  Other(TypeId),
}

enum DotKind {
  MaybeCall {
    rhs_id: Identifier,
    rhs_name: StrHandle,
    rhs_name_info: SourceRange,
    lhs_type: TypeId,
  },
  MemberAccess(TypeId),
}

impl FunctionAnalizer<'_> {
  fn get_struct_member(
    &mut self,
    struct_id: Identifier,
    name: StrHandle,
  ) -> Option<(usize, TypeId)> {
    let s = self.global_env.structs[&struct_id].clone();
    let name = name.get(self.ast);
    if let Some((_, member_type)) = s
      .member_names
      .iter()
      .zip(s.member_types.into_iter().enumerate())
      .find(|(member_name, _)| member_name.get(self.ast) == name)
    {
      Some(member_type)
    } else {
      None
    }
  }

  fn dot_call(
    &mut self,
    id: Identifier,
    lhs: TypeId,
    lhs_start_address: Address,
    name_sr: SourceRange,
    name: StrHandle,
  ) -> CallType {
    let typeid = self.get_typeid(id);
    if let Type::Function { parameters, ret } = self.type_map.get_type(typeid) {
      let function_start = self.code.get_next_instruction_address();
      unsafe { self.code.get_id(id) }
      let chunk_end = self.code.get_next_instruction_address();
      unsafe { self.code.swap(lhs_start_address, function_start, chunk_end) }
      CallType::DotCall {
        first_argument: lhs,
        fn_params: parameters.clone(),
        fn_ret: *ret,
      }
    } else {
      self.emit_error(sema_err::incorrect_dot(name_sr, name.get(self.ast)));
      CallType::Other(TypeId::ERROR)
    }
  }

  fn try_dot_call(&mut self, expr: ExprHandle) -> CallType {
    let call_start = self.code.get_next_instruction_address();
    match expr.get(self.ast) {
      &Expr::Variable { id, .. } => {
        let typeid = self.get_typeid(id);
        unsafe { self.code.get_id(id) }
        if let Type::Function { .. } = self.get_type(typeid) {
          CallType::Call(typeid)
        } else {
          CallType::Other(typeid)
        }
      }
      &Expr::Dot {
        lhs,
        rhs_name,
        rhs_id,
        rhs_sr,
      } => match self.dot_kind(lhs, rhs_name, rhs_id, rhs_sr) {
        DotKind::MaybeCall {
          rhs_id,
          rhs_name,
          rhs_name_info,
          lhs_type,
        } => self.dot_call(rhs_id, lhs_type, call_start, rhs_name_info, rhs_name),
        DotKind::MemberAccess(member_type) => CallType::Other(member_type),
      },
      _ => CallType::Other(self.analyze_expr(expr)),
    }
  }

  pub fn lambda(
    &mut self,
    info: SourceRange,
    parameters: &[TypeId],
    captures: &[Identifier],
    fn_type: TypeId,
    body: &[StmtHandle],
  ) -> TypeId {
    self.check_function(&parameters, &captures, info, body);
    unsafe {
      self.code.maybe_create_closure(&captures);
    }
    fn_type
  }

  pub fn assignment(&mut self, id: Identifier, id_info: SourceRange, value: ExprHandle) -> TypeId {
    let value_type = self.analyze_expr(value);
    self.assign(id, id_info, value_type);
    value_type
  }

  pub fn variable(&mut self, id: Identifier) -> TypeId {
    // FIXME: id type checked twice
    unsafe { self.code.get_id(id) };
    self.get_typeid(id)
  }

  pub fn literal(&mut self, literal: Literal) -> TypeId {
    let val = match literal {
      Literal::String(s) => ConstantValue::Str(s.get(self.ast).to_owned()),
      Literal::False => ConstantValue::Bool(false),
      Literal::True => ConstantValue::Bool(true),
      Literal::Number(num) => ConstantValue::Number(num),
      Literal::Null => ConstantValue::None,
    };
    unsafe {
      self.code.push_constant(val);
    }
    TypeId::from_literal(literal)
  }

  fn check_call_arguments(
    &mut self,
    parameters: &[TypeId],
    return_type: TypeId,
    args: &[TypeId],
    call_sr: SourceRange,
  ) -> TypeId {
    if args.len() != parameters.len() {
      self.emit_error(sema_err::incorrect_function_argument_number(
        call_sr,
        parameters.len(),
        args.len(),
      ));
      return TypeId::ERROR;
    }
    for (arg_num, (param_type, arg_type)) in parameters.iter().zip(args).enumerate() {
      if *param_type != TypeId::ANY && arg_type != param_type {
        self.emit_error(sema_err::incorrect_function_argument_type(
          call_sr,
          arg_num + 1, // starts at 0
          self.type_string(*arg_type),
          self.type_string(*param_type),
        ));
        return TypeId::ERROR;
      }
    }
    return_type
  }

  fn function_call(
    &mut self,
    function: ExprHandle,
    call_sr: SourceRange,
    arguments: &[ExprHandle],
  ) -> TypeId {
    let call_type = self.try_dot_call(function);
    let mut number_of_arguments = arguments.len();
    let arguments = arguments.iter().map(|arg| self.analyze_expr(*arg));
    let ret = match call_type {
      CallType::Call(type_id) => {
        let args = arguments.collect::<Vec<TypeId>>();
        // FIXME: remove clone, maybe refactor check_call_arguments
        if let Type::Function { parameters, ret } = self.get_type(type_id).clone() {
          self.check_call_arguments(&parameters, ret, &args, call_sr)
        } else {
          panic!();
        }
      }
      CallType::DotCall {
        first_argument,
        fn_params,
        fn_ret,
      } => {
        number_of_arguments += 1;
        let mut args = vec![first_argument];
        args.extend(arguments);
        self.check_call_arguments(&fn_params, fn_ret, &args, call_sr)
      }
      CallType::Other(other) => {
        self.emit_error(sema_err::cannot_call_type(call_sr, self.type_string(other)));
        TypeId::ERROR
      }
    };
    unsafe {
      self.code.push_op2(OpCode::Call, number_of_arguments as u8);
    }
    ret
  }

  pub fn binary_operation(
    &mut self,
    left_expr: ExprHandle,
    operator: Operator,
    operator_sr: SourceRange,
    right_expr: ExprHandle,
  ) -> TypeId {
    let lhs = self.analyze_expr(left_expr);
    let rhs = self.analyze_expr(right_expr);
    match (lhs, operator, rhs) {
      (TypeId::NUM, bin_op, TypeId::NUM) if ARITHMETIC_OPERATORS.contains(&bin_op) => {
        unsafe { self.code.push_op(OpCode::from_numeric_operator(bin_op)) }
        TypeId::NUM
      }
      (TypeId::STR, Operator::Basic('+'), TypeId::STR) => {
        unsafe { self.code.push_op(OpCode::AddStr) };
        TypeId::STR
      }
      (TypeId::NUM, comp_op, TypeId::NUM) if COMP_OPERATORS.contains(&comp_op) => {
        unsafe { self.code.push_op(OpCode::from_numeric_operator(comp_op)) };
        TypeId::BOOL
      }
      (TypeId::STR, comp_op, TypeId::STR) if COMP_OPERATORS.contains(&comp_op) => {
        unsafe {
          self
            .code
            .push_op(OpCode::from_string_comp_operator(comp_op))
        }
        TypeId::BOOL
      }
      (TypeId::BOOL, comp_op, TypeId::BOOL)
        if comp_op == Operator::Same || comp_op == Operator::Different =>
      {
        let op = if comp_op == Operator::Same {
          OpCode::SameBool
        } else {
          OpCode::DiffBool
        };
        unsafe {
          self.code.push_op(op);
        }
        TypeId::BOOL
      }
      (lhs, op, rhs) => {
        self.emit_error(sema_err::incorrect_binary_operator(
          operator_sr,
          op,
          self.type_string(lhs),
          self.type_string(rhs),
        ));
        TypeId::ERROR
      }
    }
  }

  pub fn logical_operation(
    &mut self,
    left_expr: ExprHandle,
    operator: Operator,
    operator_sr: SourceRange,
    right_expr: ExprHandle,
  ) -> TypeId {
    let lhs = self.analyze_expr(left_expr);
    let rhs = match operator {
      Operator::And => {
        let jump = unsafe { self.code.push_jump(OpCode::JumpIfFalseNoPop) };
        unsafe { self.code.push_op(OpCode::Pop) };
        let ret = self.analyze_expr(right_expr);
        self.code.backpatch_current_instruction(jump);
        ret
      }
      Operator::Or => {
        let check_rhs = unsafe { self.code.push_jump(OpCode::JumpIfFalseNoPop) };
        let skip_rhs_jump = unsafe { self.code.push_jump(OpCode::Jump) };
        self.code.backpatch_current_instruction(check_rhs);
        unsafe { self.code.push_op(OpCode::Pop) };
        let ret = self.analyze_expr(right_expr);
        self.code.backpatch_current_instruction(skip_rhs_jump);
        ret
      }
      _ => panic!(),
    };
    if let (TypeId::BOOL, TypeId::BOOL) = (lhs, rhs) {
      TypeId::BOOL
    } else {
      self.emit_error(sema_err::incorrect_binary_operator(
        operator_sr,
        operator,
        self.type_string(lhs),
        self.type_string(rhs),
      ));
      TypeId::ERROR
    }
  }

  pub fn unary_operation(
    &mut self,
    operator: Operator,
    operator_sr: SourceRange,
    right_expr: ExprHandle,
  ) -> TypeId {
    let rhs = self.analyze_expr(right_expr);
    match (operator, rhs) {
      (Operator::Basic('-'), TypeId::NUM) => {
        unsafe { self.code.push_op(OpCode::NegNum) }
        TypeId::NUM
      }
      (Operator::Basic('!'), TypeId::BOOL) => {
        unsafe { self.code.push_op(OpCode::NotBool) };
        TypeId::BOOL
      }
      (op, rhs) => {
        self.emit_error(sema_err::incorrect_unary_operator(
          operator_sr,
          operator,
          self.type_string(rhs),
        ));
        TypeId::ERROR
      }
    }
  }

  fn dot_kind(
    &mut self,
    left_expr: ExprHandle,
    rhs_name: StrHandle,
    rhs_id: Identifier,
    name_info: SourceRange,
  ) -> DotKind {
    let left = self.analyze_expr(left_expr);
    if let Type::Struct(id) = self.type_map.get_type(left) {
      if let Some((index, member_type)) = self.get_struct_member(*id, rhs_name) {
        unsafe { self.code.push_op2(OpCode::GetMember, index as u8) }
        DotKind::MemberAccess(member_type)
      } else {
        DotKind::MaybeCall {
          rhs_id,
          rhs_name,
          rhs_name_info: name_info,
          lhs_type: left,
        }
      }
    } else {
      DotKind::MaybeCall {
        rhs_id,
        rhs_name,
        rhs_name_info: name_info,
        lhs_type: left,
      }
    }
  }

  fn dot(
    &mut self,
    left_expr: ExprHandle,
    rhs_name: StrHandle,
    rhs_id: Identifier,
    name_sr: SourceRange,
  ) -> TypeId {
    if let DotKind::MemberAccess(typeid) = self.dot_kind(left_expr, rhs_name, rhs_id, name_sr) {
      typeid
    } else {
      self.emit_error(sema_err::incorrect_dot(name_sr, rhs_name.get(self.ast)));
      TypeId::ERROR
    }
  }

  fn set(
    &mut self,
    object: ExprHandle,
    member_name: StrHandle,
    member_name_sr: SourceRange,
    value: ExprHandle,
  ) -> TypeId {
    let object = self.analyze_expr(object);
    let value = self.analyze_expr(value);
    if let Type::Struct(id) = self.type_map.get_type(object) {
      if let Some((index, member_type)) = self.get_struct_member(*id, member_name) {
        if value == member_type {
          unsafe { self.code.push_op2(OpCode::SetMember, index as u8) }
          member_type
        } else {
          self.emit_error(sema_err::incorrect_member_assignment(
            member_name_sr,
            member_name.get(self.ast),
            self.type_string(member_type),
            self.type_string(value),
          ));
          TypeId::ERROR
        }
      } else {
        let object_type = self.type_string(object);
        self.emit_error(sema_err::not_a_member(
          member_name_sr,
          member_name.get(self.ast),
          object_type,
        ));
        TypeId::ERROR
      }
    } else {
      let lhs_type = self.type_string(object);
      self.emit_error(sema_err::incorrect_type_for_set(member_name_sr, lhs_type));
      TypeId::ERROR
    }
  }

  pub fn analyze_expr(&mut self, expr: ExprHandle) -> TypeId {
    match expr.get(self.ast) {
      Expr::Lambda {
        parameters_sr,
        parameter_types,
        captures,
        function_type_id,
        body,
        ..
      } => self.lambda(
        *parameters_sr,
        parameter_types,
        captures,
        *function_type_id,
        body,
      ),
      Expr::Assignment { id, id_sr, value } => self.assignment(*id, *id_sr, *value),
      Expr::Variable { id, .. } => self.variable(*id),
      Expr::Literal { value, .. } => self.literal(*value),
      Expr::FnCall {
        func,
        call_sr,
        arguments,
      } => self.function_call(*func, *call_sr, arguments),
      Expr::Binary {
        left,
        operator,
        operator_sr,
        right,
      } => self.binary_operation(*left, *operator, *operator_sr, *right),
      Expr::Logical {
        left,
        operator,
        operator_sr,
        right,
      } => self.logical_operation(*left, *operator, *operator_sr, *right),
      Expr::Unary {
        operator,
        operator_sr,
        right,
      } => self.unary_operation(*operator, *operator_sr, *right),
      Expr::Dot {
        lhs,
        rhs_name,
        rhs_id,
        rhs_sr,
      } => self.dot(*lhs, *rhs_name, *rhs_id, *rhs_sr),
      Expr::Set {
        object,
        member_name,
        member_name_sr,
        value,
      } => self.set(*object, *member_name, *member_name_sr, *value),
    }
  }
}
