use crate::compiler::bytecode::OpCode;
use std::fmt::Debug;

use super::{bytecode::ConstantValue, identifier::Identifier};

pub struct Label(usize);
pub struct JumpPoint(usize);
pub struct Address(usize);

#[derive(Default, Clone)]
pub struct BytecodeBuilder {
  code: Vec<u8>,
  functions: Vec<BytecodeBuilder>,
  constants: Vec<ConstantValue>,
}

impl BytecodeBuilder {
  pub unsafe fn push_constant(&mut self, val: ConstantValue) {
    let constant_offset = self.constants.len() as u8;
    let constant_type = if matches!(val, ConstantValue::Str(_)) {
      OpCode::ConstantStr
    } else {
      OpCode::Constant
    };
    self.constants.push(val);
    self.code.push(constant_type as u8);
    self.code.push(constant_offset);
  }

  pub unsafe fn push_function(&mut self, func: BytecodeBuilder) {
    let offset = self.functions.len() as u8;
    self.functions.push(func);
    self.code.push(OpCode::Function as u8);
    self.code.push(offset);
  }

  pub unsafe fn push_constant_none(&mut self) {
    self.code.push(OpCode::Constant as u8);
    self.code.push(0);
  }

  pub unsafe fn push_op(&mut self, op: OpCode) {
    self.code.push(op as u8);
  }

  /// pushes opcode with additional data
  pub unsafe fn push_op2(&mut self, op: OpCode, data: u8) {
    self.code.push(op as u8);
    self.code.push(data);
  }

  pub unsafe fn push_jump(&mut self, jump_type: OpCode) -> JumpPoint {
    debug_assert!(matches!(
      jump_type,
      OpCode::Jump | OpCode::JumpIfFalsePop | OpCode::JumpIfFalseNoPop
    ));
    unsafe { self.push_op(jump_type) };
    let index = self.code.len();
    self.code.push(0);
    self.code.push(0);
    JumpPoint(index)
  }

  pub fn push_back_jump(&mut self, Label(to): Label) {
    assert!((self.code.len() - to + 2) <= u16::MAX as usize);
    unsafe { self.push_op(OpCode::BackJump) };
    // 2 added to skip jump point
    let split = ((self.code.len() - to + 2) as u16).to_ne_bytes();
    self.code.push(split[0]);
    self.code.push(split[1]);
  }

  pub fn backpatch_current_instruction(&mut self, JumpPoint(jump_point): JumpPoint) {
    assert!((self.code.len() - jump_point - 2) <= u16::MAX as usize);
    let split = ((self.code.len() - jump_point - 2) as u16).to_ne_bytes();
    // 2 removed to skip jump point
    self.code[jump_point] = split[0];
    self.code[jump_point + 1] = split[1];
  }

  pub fn get_next_instruction_label(&self) -> Label {
    Label(self.code.len())
  }

  pub fn get_next_instruction_address(&self) -> Address {
    Address(self.code.len())
  }

  pub unsafe fn swap(
    &mut self,
    Address(start): Address,
    Address(mid): Address,
    Address(end): Address,
  ) {
    self.code[start..end].rotate_left(mid - start);
  }

  pub unsafe fn get_id(&mut self, id: Identifier) {
    match id {
      Identifier::Global(gid) => {
        self.push_constant(ConstantValue::GlobalId(gid));
        self.push_op(OpCode::GetGlobal);
      }
      Identifier::Local(id) => {
        self.push_op2(OpCode::GetLocal, id);
      }
      Identifier::Capture(id) => {
        self.push_op2(OpCode::GetCapture, id);
      }
      Identifier::Invalid => panic!("codegen with invalid ast"),
    }
  }

  pub fn create_constructor(&mut self, members: u8) {
    let mut constructor_code = Self::new();
    unsafe {
      constructor_code.push_op2(OpCode::Construct, members as u8);
      constructor_code.push_op(OpCode::Return);
      self.push_function(constructor_code);
    }
  }

  pub unsafe fn maybe_create_closure(&mut self, captures: &[Identifier]) {
    if !captures.is_empty() {
      self.push_op2(OpCode::MakeClosure, captures.len() as u8);
      for c in captures {
        self.get_id(*c);
        self.push_op(OpCode::Capture);
      }
    }
  }

  pub fn into_parts(self) -> (Vec<u8>, Vec<BytecodeBuilder>, Vec<ConstantValue>) {
    (self.code, self.functions, self.constants)
  }

  pub fn new() -> Self {
    Self {
      code: Vec::new(),
      constants: vec![ConstantValue::None],
      functions: Vec::new(),
    }
  }
}

impl Debug for BytecodeBuilder {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut result = "".to_string();
    for (i, func) in self.functions.iter().enumerate() {
      result += &format!("++ fn {i} ++\n{:?}-- fn {i} --\n", func);
    }
    let mut index = 0;
    while index < self.code.len() {
      let code = unsafe { std::mem::transmute::<u8, OpCode>(self.code[index]) };
      result += &format!("{index}: ");
      match code {
        OpCode::Constant | OpCode::ConstantStr => {
          index += 1;
          result += &format!(
            "Constant: {:?}\n",
            self.constants[self.code[index] as usize]
          );
        }
        OpCode::Function => {
          index += 1;
          result += &format!("Function: {}\n", self.code[index]);
        }
        OpCode::Call => {
          index += 1;
          result += &format!("Call: {}\n", self.code[index]);
        }
        OpCode::JumpIfFalsePop | OpCode::Jump | OpCode::JumpIfFalseNoPop => {
          index += 2;
          let jump_point = u16::from_ne_bytes([self.code[index - 1], self.code[index]]);
          result += &format!("{code:?}: {}\n", index + 1 + jump_point as usize);
        }
        OpCode::BackJump => {
          index += 2;
          let jump_point = u16::from_ne_bytes([self.code[index - 1], self.code[index]]);
          result += &format!("{code:?}: {}\n", index + 1 - jump_point as usize);
        }
        OpCode::GetLocal
        | OpCode::SetLocal
        | OpCode::GetCapture
        | OpCode::SetCapture
        | OpCode::GetMember
        | OpCode::SetMember
        | OpCode::Construct => {
          index += 1;
          result += &format!("{code:?}: {}\n", self.code[index])
        }
        OpCode::MakeClosure => {
          index += 1;
          result += &format!("MakeClosure, captures: {}\n", self.code[index])
        }
        code => result += &format!("{code:?}\n"),
      }
      index += 1;
    }
    write!(f, "{result}")
  }
}
