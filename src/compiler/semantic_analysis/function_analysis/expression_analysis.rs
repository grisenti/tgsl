use crate::compiler::{
  ast::{
    Expr, ExprHandle, Literal, Operator, OperatorPair, SourceInfoHandle, StmtHandle, StrHandle,
  },
  bytecode::{OpCode, TaggedValue},
  codegen::Address,
  identifier::Identifier,
  types::Type,
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

impl FunctionAnalizer<'_> {
  fn get_struct_member(&mut self, struct_id: Identifier, name: StrHandle) -> Option<(usize, Type)> {
    let s = self.global_env.structs[&struct_id].clone();
    let name = name.get(&self.global_env.ast);
    if let Some((_, member_type)) = s
      .member_names
      .iter()
      .zip(s.member_types.into_iter().enumerate())
      .find(|(member_name, _)| member_name.get(&self.global_env.ast) == name)
    {
      Some(member_type)
    } else {
      None
    }
  }

  fn dot_call(
    &mut self,
    id: Identifier,
    lhs: Type,
    lhs_start_address: Address,
    name_info: SourceInfoHandle,
    name: StrHandle,
  ) -> Type {
    if let Type::Function(types) = self.get_type(id) {
      let function_start = self.code.get_next_instruction_address();
      unsafe { self.code.get_id(id) }
      let chunk_end = self.code.get_next_instruction_address();
      unsafe { self.code.swap(lhs_start_address, function_start, chunk_end) }
      Type::PartialCall {
        func_types: types,
        partial_arguments: vec![lhs],
      }
    } else {
      self.emit_error(
        name_info,
        format!(
          "identifier {} is neither a struct member nor a function",
          name.get(&self.global_env.ast)
        ),
      );
      Type::Error
    }
  }

  pub fn closure(
    &mut self,
    info: SourceInfoHandle,
    parameters: Vec<Identifier>,
    captures: Vec<Identifier>,
    fn_type: Vec<Type>,
    body: Vec<StmtHandle>,
  ) -> Type {
    self.check_function(&fn_type, &captures, info, body);
    unsafe {
      self.code.maybe_create_closure(&captures);
    }
    Type::Function(fn_type)
  }

  pub fn assignment(
    &mut self,
    id: Identifier,
    id_info: SourceInfoHandle,
    value: ExprHandle,
  ) -> Type {
    let value_type = self.analyze_expr(value);
    self.assign(id, id_info, &value_type);
    value_type
  }

  pub fn variable(&mut self, id: Identifier) -> Type {
    // FIXME: id type checked twice
    unsafe { self.code.get_id(id) };
    self.get_type(id)
  }

  pub fn literal(&mut self, literal: Literal) -> Type {
    unsafe {
      self
        .code
        .push_constant(TaggedValue::from_literal(&literal, &self.global_env.ast));
    }
    Type::from_literal(literal)
  }

  fn check_call_arguments(
    &mut self,
    fn_types: &[Type],
    args: &[Type],
    call_info: SourceInfoHandle,
  ) -> Type {
    if args.len() != fn_types.len() - 1 {
      self.emit_error(
        call_info,
        format!(
          "incorrect number of arguments for function call (required {}, provided {})",
          fn_types.len() - 1,
          args.len()
        ),
      );
      return Type::Error;
    }
    for (arg_num, (param_type, arg_type)) in fn_types.iter().zip(args).enumerate() {
      if arg_type != param_type {
        self.emit_error(
          call_info,
          format!(
            "mismatched types in function call. Argument {} (of type {arg_type:?}) should be of type {param_type:?}",
            arg_num + 1 // starts at 0
          ),
        );
        return Type::Error;
      }
    }
    fn_types.last().unwrap().clone()
  }

  fn function_call(
    &mut self,
    function: ExprHandle,
    call_info: SourceInfoHandle,
    arguments: &[ExprHandle],
  ) -> Type {
    let fn_type = self.analyze_expr(function);
    let mut number_of_arguments = arguments.len();
    let arguments = arguments.iter().map(|arg| self.analyze_expr(*arg));
    let ret = match fn_type {
      Type::PartialCall {
        func_types,
        mut partial_arguments,
      } => {
        number_of_arguments += partial_arguments.len();
        partial_arguments.extend(arguments);
        self.check_call_arguments(&func_types, &partial_arguments, call_info)
      }
      Type::Function(types) => {
        let args = arguments.collect::<Vec<Type>>();
        self.check_call_arguments(&types, &args, call_info)
      }
      _ => {
        self.emit_error(call_info, format!("cannot call type {fn_type:?}"));
        Type::Error
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
    operator: OperatorPair,
    right_expr: ExprHandle,
  ) -> Type {
    let lhs = self.analyze_expr(left_expr);
    let rhs = self.analyze_expr(right_expr);
    let OperatorPair { op, src_info } = operator;
    match (lhs, op, rhs) {
      (Type::Num, bin_op, Type::Num) if ARITHMETIC_OPERATORS.contains(&bin_op) => {
        unsafe { self.code.push_op(OpCode::from_numeric_operator(bin_op)) }
        Type::Num
      }
      (Type::Str, Operator::Basic('+'), Type::Str) => {
        unsafe { self.code.push_op(OpCode::AddStr) };
        Type::Str
      }
      (Type::Num, comp_op, Type::Num) if COMP_OPERATORS.contains(&comp_op) => {
        unsafe { self.code.push_op(OpCode::from_numeric_operator(comp_op)) };
        Type::Bool
      }
      (Type::Str, comp_op, Type::Str) if COMP_OPERATORS.contains(&comp_op) => {
        unsafe {
          self
            .code
            .push_op(OpCode::from_string_comp_operator(comp_op))
        }
        Type::Bool
      }
      (Type::Bool, comp_op, Type::Bool) if comp_op == Operator::Same => Type::Bool,
      (lhs, op, rhs) => {
        self.emit_error(
          src_info,
          format!("cannot apply operator {op:?} to operands {lhs:?} and {rhs:?}"),
        );
        Type::Error
      }
    }
  }

  pub fn logical_operation(
    &mut self,
    left_expr: ExprHandle,
    operator: OperatorPair,
    right_expr: ExprHandle,
  ) -> Type {
    let lhs = self.analyze_expr(left_expr);
    let OperatorPair { op, src_info } = operator;
    let rhs = match op {
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
    if let (Type::Bool, Type::Bool) = (&lhs, &rhs) {
      Type::Bool
    } else {
      self.emit_error(
        src_info,
        format!("cannot apply logical operator {op:?} to operands {lhs:?} and {rhs:?}"),
      );
      Type::Error
    }
  }

  pub fn unary_operation(&mut self, op: OperatorPair, right_expr: ExprHandle) -> Type {
    let rhs = self.analyze_expr(right_expr);
    let OperatorPair { op, src_info } = op;
    match (op, rhs) {
      (Operator::Basic('-'), Type::Num) => {
        unsafe { self.code.push_op(OpCode::NegNum) }
        Type::Num
      }
      (Operator::Basic('!'), Type::Bool) => {
        unsafe { self.code.push_op(OpCode::NotBool) };
        Type::Bool
      }
      (op, rhs) => {
        self.emit_error(
          src_info,
          format!("cannot apply operator {op:?} to operand {rhs:?}"),
        );
        Type::Error
      }
    }
  }

  pub fn dot(
    &mut self,
    left_expr: ExprHandle,
    rhs_name: StrHandle,
    rhs_id: Identifier,
    name_info: SourceInfoHandle,
  ) -> Type {
    let expr_start_address = self.code.get_next_instruction_address();
    let left = self.analyze_expr(left_expr);
    if let Type::Struct(id) = left {
      if let Some((index, member_type)) = self.get_struct_member(id, rhs_name) {
        unsafe { self.code.push_op2(OpCode::GetMember, index as u8) }
        member_type
      } else {
        self.dot_call(rhs_id, left, expr_start_address, name_info, rhs_name)
      }
    } else {
      self.dot_call(rhs_id, left, expr_start_address, name_info, rhs_name)
    }
  }

  pub fn set(
    &mut self,
    object: ExprHandle,
    member_name: StrHandle,
    member_name_info: SourceInfoHandle,
    value: ExprHandle,
  ) -> Type {
    let object = self.analyze_expr(object);
    let value = self.analyze_expr(value);
    if let Type::Struct(id) = object {
      if let Some((index, member_type)) = self.get_struct_member(id, member_name) {
        if value == member_type {
          unsafe { self.code.push_op2(OpCode::SetMember, index as u8) }
          member_type
        } else {
          self.emit_error(
            member_name_info,
            format!(
              "member '{}' is of type {member_type:?}, cannot assing value of type {value:?}",
              member_name.get(&self.global_env.ast)
            ),
          );
          Type::Error
        }
      } else {
        self.emit_error(
          member_name_info,
          format!(
            "{} is not a member of type {object:?}",
            member_name.get(&self.global_env.ast)
          ),
        );
        Type::Error
      }
    } else {
      self.emit_error(
        member_name_info,
        format!("cannot set propriety for type {object:?}"),
      );
      Type::Error
    }
  }

  pub fn analyze_expr(&mut self, expr: ExprHandle) -> Type {
    match expr.get(&self.global_env.ast) {
      Expr::Closure {
        info,
        parameters,
        captures,
        fn_type,
        body,
      } => self.closure(info, parameters, captures, fn_type, body),
      Expr::Assignment { id, id_info, value } => self.assignment(id, id_info, value),
      Expr::Variable { id, .. } => self.variable(id),
      Expr::Literal { literal, .. } => self.literal(literal),
      Expr::FnCall {
        func,
        call_info,
        arguments,
      } => self.function_call(func, call_info, &arguments),
      Expr::Binary {
        left,
        operator,
        right,
      } => self.binary_operation(left, operator, right),
      Expr::Logical {
        left,
        operator,
        right,
      } => self.logical_operation(left, operator, right),
      Expr::Unary { operator, right } => self.unary_operation(operator, right),
      Expr::Dot {
        lhs,
        name,
        identifier,
        name_info,
      } => self.dot(lhs, name, identifier, name_info),
      Expr::Set {
        object,
        name,
        name_info,
        value,
      } => self.set(object, name, name_info, value),
    }
  }
}
