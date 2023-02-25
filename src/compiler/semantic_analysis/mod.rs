use std::{collections::HashMap, ops::IndexMut, result};

use super::{
  ast::*,
  bytecode::{Chunk, OpCode, TaggedValue},
  error_from_source_info,
};
use crate::errors::{SourceError, SourceInfo};

#[derive(Clone)]
struct Struct {
  member_names: Vec<StrHandle>,
  member_types: Vec<Type>,
}

type StructMap = HashMap<Identifier, Struct>;
type FunctionMap = HashMap<Identifier, Vec<Type>>;

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

enum ReturnType {
  Conditional(Type),
  Unconditional(Type),
  Error,
}

fn to_conditional(opt_ret: Option<ReturnType>) -> Option<ReturnType> {
  if let Some(ReturnType::Unconditional(ret_type)) = opt_ret {
    Some(ReturnType::Conditional(ret_type))
  } else {
    opt_ret
  }
}

impl ReturnType {
  fn merge(self, other: ReturnType) -> Option<Self> {
    match (self, other) {
      (ReturnType::Unconditional(t1), ReturnType::Conditional(t2)) if t1 == t2 => {
        Some(ReturnType::Unconditional(t1))
      }
      (ReturnType::Conditional(t1), ReturnType::Unconditional(t2)) if t1 == t2 => {
        Some(ReturnType::Unconditional(t1))
      }
      (ReturnType::Unconditional(t1), ReturnType::Unconditional(t2)) if t1 == t2 => {
        Some(ReturnType::Unconditional(t1))
      }
      (ReturnType::Conditional(t1), ReturnType::Conditional(t2)) if t1 == t2 => {
        Some(ReturnType::Conditional(t1))
      }
      _ => None,
    }
  }
}

#[derive(Debug)]
struct LocalVariable {
  var_type: Type,
  scope_depth: u8,
}

pub struct SemanticAnalizer {
  function_stack: Vec<SourceInfoHandle>,
  global_types: Vec<Type>,
  locals: Vec<LocalVariable>,
  captures: Vec<Type>,
  scope_depth: u8,
  loop_depth: u32,
  structs: StructMap,
  functions: FunctionMap,
  errors: Vec<SourceError>,
  generated_code: Chunk,
}

impl SemanticAnalizer {
  fn emit_error(&mut self, err: SourceError) {
    self.errors.push(err);
  }

  fn equal_types(&self, value: &Type, specifier: &Type) -> bool {
    match (value, specifier) {
      (_, Type::Unknown) => true,
      (a, b) => a == b,
    }
  }

  fn push_scope(&mut self) {
    self.scope_depth += 1;
  }

  fn pop_scope(&mut self) {
    assert!(self.scope_depth >= 1);
    let names_in_local_scope = self
      .locals
      .iter()
      .rev()
      .take_while(|LocalVariable { scope_depth, .. }| self.scope_depth == *scope_depth)
      .count();
    for _ in 0..names_in_local_scope {
      unsafe { self.generated_code.push_op(OpCode::Pop) }
    }
    self
      .locals
      .resize_with(self.locals.len() - names_in_local_scope, || panic!());
    self.scope_depth -= 1;
  }

  fn get_struct_member(
    &mut self,
    struct_id: Identifier,
    name: StrHandle,
    ast: &AST,
  ) -> Option<Type> {
    let s = self.structs[&struct_id].clone();
    let name = name.get(ast);
    if let Some((_, member_type)) = s
      .member_names
      .iter()
      .zip(s.member_types)
      .find(|(member_name, _)| member_name.get(ast) == name)
    {
      Some(member_type)
    } else {
      None
    }
  }

  fn get_type_mut_ref(&mut self, id: Identifier) -> &mut Type {
    match id {
      Identifier::Global(gid) => &mut self.global_types[gid as usize],
      Identifier::Local { scope_depth, id } => &mut self.locals[id as usize].var_type,
      Identifier::Capture(id) => &mut self.captures[id as usize],
    }
  }

  pub fn set_type_or_err(&mut self, id: Identifier, rhs: Type, name_info: SourceInfo) {
    let lhs = self.get_type_mut_ref(id);
    let result = match lhs {
      Type::Unknown => {
        *lhs = rhs;
        Ok(())
      }
      Type::Error => Ok(()),
      lhs if *lhs != rhs => Err(error_from_source_info(
        &name_info,
        format!("cannot assign value of type {rhs:?} to identifier of type {lhs:?}"),
      )),
      _ => Ok(()), // equal types, no need to set
    };
    if let Err(err) = result {
      self.emit_error(err);
    }
  }

  pub fn get_type(&mut self, id: Identifier) -> Type {
    self.get_type_mut_ref(id).clone()
  }

  fn check_self_assignment(
    &mut self,
    ast: &AST,
    lhs_id: Identifier,
    expression: Option<ExprHandle>,
  ) {
    if let Some(Expr::Variable {
      id: rhs_id,
      id_info,
    }) = expression.map(|handle| handle.get(ast))
    {
      if rhs_id == lhs_id {
        self.emit_error(error_from_source_info(
          &id_info.get(ast),
          "cannot initialize identifier with itself".to_string(),
        ));
      }
    }
  }

  fn check_valid_condition_type(&mut self, info: SourceInfo, condition_type: Type) {
    if condition_type != Type::Bool {
      self.emit_error(error_from_source_info(
        &info,
        format!("cannot use value of type {condition_type:?} in a condition"),
      ))
    }
  }

  fn check_function_call(
    &mut self,
    ast: &AST,
    fn_types: Vec<Type>,
    args: Vec<Type>,
    call_info: SourceInfoHandle,
  ) -> Type {
    if args.len() != fn_types.len() - 1 {
      self.emit_error(error_from_source_info(
        &call_info.get(ast),
        format!(
          "incorrect number of arguments for function call (required {}, provided {})",
          fn_types.len() - 1,
          args.len()
        ),
      ));
      return Type::Error;
    }
    for (arg_num, (param_type, arg_type)) in fn_types.iter().zip(&args).enumerate() {
      if !self.equal_types(arg_type, param_type) {
        self.emit_error(error_from_source_info(
          &call_info.get(ast),
          format!(
            "mismatched types in function call. Argument {} should be of type {param_type:?}",
            arg_num + 1
          ),
        ));
        return Type::Error;
      }
    }
    fn_types.last().unwrap().clone()
  }

  fn check_function(&mut self, ast: &AST, fn_types: Vec<Type>, body: &[StmtHandle]) {
    let return_type = fn_types.last().unwrap();
    let mut unconditional_return = false;
    for stmt in body.iter() {
      if let Some(ret) = self.analyze_stmt(ast, *stmt) {
        match ret {
          ReturnType::Conditional(val) if val == *return_type => {}
          ReturnType::Unconditional(val) if val == *return_type => unconditional_return = true,
          ReturnType::Error => unconditional_return = true,
          _ => {
            self.emit_error(error_from_source_info(
              &self.function_stack.last().unwrap().get(ast),
              "inconsistent return types".to_string(),
            ));
            unconditional_return = true;
            break;
          }
        }
      }
    }
    if !unconditional_return && *return_type != Type::Nothing {
      self.emit_error(error_from_source_info(
        &self.function_stack.last().unwrap().get(ast),
        "function only has conditional return types".to_string(),
      ));
    }
  }

  fn check_var_decl(
    &mut self,
    ast: &AST,
    identifier: Identifier,
    id_info: SourceInfoHandle,
    expression: Option<ExprHandle>,
  ) {
    match identifier {
      Identifier::Global(gid) => {} // already set
      Identifier::Local { id, .. } => self.locals.push(LocalVariable {
        var_type: Type::Unknown,
        scope_depth: self.scope_depth,
      }),
      Identifier::Capture(_) => todo!(),
    }
    self.check_self_assignment(ast, identifier, expression);
    if let Some(expr) = expression {
      let rhs = self.analyze_expr(ast, expr);
      self.set_type_or_err(identifier, rhs, id_info.get(ast));
    } else {
      unsafe { self.generated_code.push_constant_none() }
    }
    match identifier {
      Identifier::Global(gid) => unsafe {
        self
          .generated_code
          .push_constant(TaggedValue::global_id(gid));
        self.generated_code.push_op(OpCode::SetGlobal);
        self.generated_code.push_op(OpCode::Pop);
      },
      Identifier::Local { .. } => {} // no operation needed, the value is already on the stack
      Identifier::Capture(_) => todo!(),
    }
  }

  fn check_return_types(
    &mut self,
    _ast: &AST,
    returns: Vec<Option<ReturnType>>,
  ) -> Option<ReturnType> {
    let mut ret_types = returns.into_iter().filter_map(|v| v);
    if let Some(first) = ret_types.next() {
      if let Some(ret_type) = ret_types.try_fold(first, |acc, elem| elem.merge(acc)) {
        Some(ret_type)
      } else {
        self.emit_error(error_from_source_info(
          &SourceInfo::temporary(),
          "inconsistent return types in function".to_string(),
        ));
        Some(ReturnType::Error)
      }
    } else {
      None
    }
  }

  fn analyze_stmt(&mut self, ast: &AST, stmt: StmtHandle) -> Option<ReturnType> {
    match stmt.clone().get(ast) {
      Stmt::VarDecl {
        identifier,
        id_info,
        var_type: _,
        expression,
      } => {
        self.check_var_decl(ast, identifier, id_info, expression);
        None
      }
      Stmt::While {
        info,
        condition,
        loop_body,
      } => {
        let label = self.generated_code.get_next_instruction_label();
        let condition_type = self.analyze_expr(ast, condition);
        let loop_condition = self.generated_code.push_jump(OpCode::JumpIfFalsePop);
        self.check_valid_condition_type(info.get(ast), condition_type);
        self.loop_depth += 1;
        let ret = self.analyze_stmt(ast, loop_body);
        self.loop_depth -= 1;
        self.generated_code.push_back_jump(label);
        self
          .generated_code
          .backpatch_current_instruction(loop_condition);
        to_conditional(ret)
      }
      Stmt::Struct {
        name,
        name_info: _,
        member_names,
        member_types,
      } => {
        self.structs.insert(
          name,
          Struct {
            member_names,
            member_types,
          },
        );
        None
      }
      Stmt::IfBranch {
        if_info,
        condition,
        true_branch,
        else_branch,
      } => {
        let condition_type = self.analyze_expr(ast, condition);
        self.check_valid_condition_type(if_info.get(ast), condition_type);
        let if_jump_point = self.generated_code.push_jump(OpCode::JumpIfFalsePop);
        let ret_true = to_conditional(self.analyze_stmt(ast, true_branch));
        let ret_false = if let Some(stmt) = else_branch {
          let skip_else = self.generated_code.push_jump(OpCode::Jump);
          // TODO: cleanup duplicates
          self
            .generated_code
            .backpatch_current_instruction(if_jump_point);

          let res = self.analyze_stmt(ast, stmt);
          self.generated_code.backpatch_current_instruction(skip_else);
          res
        } else {
          self
            .generated_code
            .backpatch_current_instruction(if_jump_point);
          None
        };
        self.check_return_types(ast, vec![ret_true, ret_false])
      }
      Stmt::Block(stmts) => {
        let mut return_types = Vec::new();
        self.push_scope();
        for s in stmts {
          return_types.push(self.analyze_stmt(ast, s))
        }
        self.pop_scope();
        self.check_return_types(ast, return_types)
      }
      Stmt::Function {
        id,
        name_info,
        parameters: _,
        captures,
        fn_type,
        body,
      } => {
        self.function_stack.push(name_info);
        self.check_function(ast, fn_type.clone(), &body);
        self.function_stack.pop();
        self.functions.insert(id, fn_type);
        None
      }
      Stmt::Break(info) => {
        if self.loop_depth == 0 {
          self.emit_error(error_from_source_info(
            &info.get(ast),
            "cannot have break outside of loop body".to_string(),
          ));
        }
        None
      }
      Stmt::Return { expr, src_info } => {
        if self.function_stack.len() > 0 {
          Some(ReturnType::Unconditional(self.analyze_expr(ast, expr)))
        } else {
          self.emit_error(error_from_source_info(
            &src_info.get(ast),
            "cannot have return outside of function body".to_string(),
          ));
          None
        }
      }
      Stmt::Print(expr) => {
        self.analyze_expr(ast, expr);
        unsafe { self.generated_code.push_op(OpCode::Print) };
        None
      }
      Stmt::Expr(expr) => {
        self.analyze_expr(ast, expr);
        unsafe { self.generated_code.push_op(OpCode::Pop) };
        None
      }
      Stmt::ExternFunction { .. } => None,
    }
  }

  fn dot_call(
    &mut self,
    ast: &AST,
    id: Identifier,
    lhs: Type,
    name_info: SourceInfoHandle,
    name: StrHandle,
  ) -> Type {
    if let Type::Function(types) = self.get_type(id) {
      Type::PartialCall {
        func_types: types,
        partial_arguments: vec![lhs],
      }
    } else {
      self.emit_error(error_from_source_info(
        &name_info.get(ast),
        format!(
          "identifier {} is neither a struct member nor a function",
          name.get(ast)
        ),
      ));
      Type::Error
    }
  }

  fn binary_operation(
    &mut self,
    ast: &AST,
    lhs: ExprHandle,
    op: OperatorPair,
    rhs: ExprHandle,
  ) -> Type {
    let lhs = self.analyze_expr(ast, lhs);
    let rhs = self.analyze_expr(ast, rhs);
    let OperatorPair { op, src_info } = op;
    match (lhs, op, rhs) {
      (Type::Num, bin_op, Type::Num) if ARITHMETIC_OPERATORS.contains(&bin_op) => {
        unsafe {
          self
            .generated_code
            .push_op(OpCode::from_numeric_operator(bin_op))
        }
        Type::Num
      }
      (Type::Str, Operator::Basic('+'), Type::Str) => {
        unsafe { self.generated_code.push_op(OpCode::AddStr) };
        Type::Str
      }
      (Type::Num, comp_op, Type::Num) if COMP_OPERATORS.contains(&comp_op) => {
        unsafe {
          self
            .generated_code
            .push_op(OpCode::from_numeric_operator(comp_op))
        };
        Type::Bool
      }
      (Type::Str, comp_op, Type::Str) if COMP_OPERATORS.contains(&comp_op) => {
        unsafe {
          self
            .generated_code
            .push_op(OpCode::from_string_comp_operator(comp_op))
        }
        Type::Bool
      }
      (Type::Bool, comp_op, Type::Bool) if comp_op == Operator::Same => Type::Bool,
      (lhs, op, rhs) => {
        self.emit_error(error_from_source_info(
          &src_info.get(ast),
          format!("cannot apply operator {op:?} to operands {lhs:?} and {rhs:?}"),
        ));
        Type::Error
      }
    }
  }

  fn logical_operation(
    &mut self,
    ast: &AST,
    lhs: ExprHandle,
    op: OperatorPair,
    rhs: ExprHandle,
  ) -> Type {
    let lhs = self.analyze_expr(ast, lhs);
    let OperatorPair { op, src_info } = op;
    let rhs = match op {
      Operator::And => {
        let jump = self.generated_code.push_jump(OpCode::JumpIfFalseNoPop);
        unsafe { self.generated_code.push_op(OpCode::Pop) };
        let ret = self.analyze_expr(ast, rhs);
        self.generated_code.backpatch_current_instruction(jump);
        ret
      }
      Operator::Or => {
        let check_rhs = self.generated_code.push_jump(OpCode::JumpIfFalseNoPop);
        let skip_rhs_jump = self.generated_code.push_jump(OpCode::Jump);
        self.generated_code.backpatch_current_instruction(check_rhs);
        unsafe { self.generated_code.push_op(OpCode::Pop) };
        let ret = self.analyze_expr(ast, rhs);
        self
          .generated_code
          .backpatch_current_instruction(skip_rhs_jump);
        ret
      }
      _ => panic!(),
    };
    if let (Type::Bool, Type::Bool) = (&lhs, &rhs) {
      Type::Bool
    } else {
      self.emit_error(error_from_source_info(
        &src_info.get(ast),
        format!("cannot apply logical operator {op:?} to operands {lhs:?} and {rhs:?}"),
      ));
      Type::Error
    }
  }

  fn unary_operation(&mut self, ast: &AST, op: OperatorPair, rhs: ExprHandle) -> Type {
    let rhs = self.analyze_expr(ast, rhs);
    let OperatorPair { op, src_info } = op;
    match (op, rhs) {
      (Operator::Basic('-'), Type::Num) => {
        unsafe { self.generated_code.push_op(OpCode::NegNum) }
        Type::Num
      }
      (Operator::Basic('!'), Type::Bool) => {
        unsafe { self.generated_code.push_op(OpCode::NotBool) };
        Type::Bool
      }
      (op, rhs) => {
        self.emit_error(error_from_source_info(
          &src_info.get(ast),
          format!("cannot apply operator {op:?} to operand {rhs:?}"),
        ));
        Type::Error
      }
    }
  }

  pub fn analyze_expr(&mut self, ast: &AST, expr: ExprHandle) -> Type {
    match expr.clone().get(ast) {
      Expr::Closure {
        info,
        parameters: _,
        captures,
        fn_type,
        body,
      } => {
        self.function_stack.push(info);
        self.check_function(ast, fn_type.clone(), &body);
        self.function_stack.pop();
        Type::Function(fn_type)
      }
      Expr::Assignment { id, id_info, value } => {
        let value_type = self.analyze_expr(ast, value);
        self.set_type_or_err(id, value_type.clone(), id_info.get(ast));
        match id {
          Identifier::Global(gid) => unsafe {
            self
              .generated_code
              .push_constant(TaggedValue::global_id(gid));
            self.generated_code.push_op(OpCode::SetGlobal);
          },
          Identifier::Local { id, .. } => unsafe {
            self.generated_code.push_type2_op(OpCode::SetLocal, id);
          },
          _ => todo!(),
        }
        value_type
      }
      Expr::Variable { id, .. } => {
        match id {
          Identifier::Global(gid) => unsafe {
            self
              .generated_code
              .push_constant(TaggedValue::global_id(gid));
            self.generated_code.push_op(OpCode::GetGlobal);
          },
          Identifier::Local { id, .. } => unsafe {
            self.generated_code.push_type2_op(OpCode::GetLocal, id);
          },
          _ => todo!(),
        }
        self.get_type(id)
      }
      Expr::Literal { literal, .. } => {
        unsafe {
          self
            .generated_code
            .push_constant(TaggedValue::from_literal(&literal, ast));
        }
        Type::from_literal(literal)
      }
      Expr::FnCall {
        func,
        call_info,
        arguments,
      } => {
        let func = self.analyze_expr(ast, func);
        let arguments = arguments.iter().map(|arg| self.analyze_expr(ast, *arg));
        let ret = match func {
          Type::PartialCall {
            func_types,
            mut partial_arguments,
          } => {
            partial_arguments.extend(arguments);
            self.check_function_call(ast, func_types, partial_arguments, call_info)
          }
          Type::Function(types) => {
            let args = arguments.collect::<Vec<Type>>();
            self.check_function_call(ast, types, args, call_info)
          }
          _ => {
            self.emit_error(error_from_source_info(
              &call_info.get(ast),
              format!("cannot call type {func:?}"),
            ));
            Type::Error
          }
        };
        ret
      }
      Expr::Binary {
        left,
        operator,
        right,
      } => self.binary_operation(ast, left, operator, right),
      Expr::Logical {
        left,
        operator,
        right,
      } => self.logical_operation(ast, left, operator, right),
      Expr::Unary { operator, right } => self.unary_operation(ast, operator, right),
      Expr::Dot {
        lhs,
        name,
        identifier,
        name_info,
      } => {
        let left = self.analyze_expr(ast, lhs);
        if let Type::Struct(id) = left {
          if let Some(member_type) = self.get_struct_member(id, name, ast) {
            member_type
          } else {
            self.dot_call(ast, identifier, left, name_info, name)
          }
        } else {
          self.dot_call(ast, identifier, left, name_info, name)
        }
      }
      Expr::Set {
        object,
        name,
        name_info,
        value,
      } => {
        let lhs = self.analyze_expr(ast, object);
        let rhs = self.analyze_expr(ast, value);
        if let Type::Struct(id) = lhs {
          if let Some(member_type) = self.get_struct_member(id, name, ast) {
            if self.equal_types(&rhs, &member_type) {
              member_type
            } else {
              self.emit_error(error_from_source_info(
                &name_info.get(ast),
                format!(
                  "member '{}' is of type {member_type:?}, cannot assing value of type {rhs:?}",
                  name.get(ast)
                ),
              ));
              Type::Error
            }
          } else {
            self.emit_error(error_from_source_info(
              &name_info.get(ast),
              format!("{} is not a member of type {lhs:?}", name.get(ast)),
            ));
            Type::Error
          }
        } else {
          self.emit_error(error_from_source_info(
            &name_info.get(ast),
            format!("cannot set propriety for type {lhs:?}"),
          ));
          Type::Error
        }
      }
    }
  }

  pub fn analyze(ast: &AST, types: Vec<Type>) -> Result<Chunk, SourceError> {
    let mut analizer = Self {
      function_stack: Vec::new(),
      global_types: types,
      locals: Vec::with_capacity(256),
      captures: vec![Type::Unknown; 256],
      scope_depth: 0,
      loop_depth: 0,
      structs: HashMap::new(),
      functions: HashMap::new(),
      errors: Vec::new(),
      generated_code: Chunk::empty(),
    };
    for stmt in ast.get_program() {
      analizer.analyze_stmt(ast, *stmt);
    }
    if analizer.errors.is_empty() {
      unsafe { analizer.generated_code.push_op(OpCode::Return) };
      Ok(analizer.generated_code)
    } else {
      Err(SourceError::from_err_vec(analizer.errors))
    }
  }
}
