use std::fmt::Debug;

use crate::compiler::codegen::bytecode::{ConstantValue, OpCode};

pub struct Label(usize);
pub struct JumpPoint(usize);
pub struct Address(usize);

#[derive(Clone)]
pub struct FunctionCode {
  function_name: String,
  code: Vec<u8>,
  constants: Vec<ConstantValue>,
}

impl Default for FunctionCode {
  fn default() -> Self {
    Self {
      function_name: String::new(),
      code: vec![],
      constants: vec![ConstantValue::None],
    }
  }
}

impl FunctionCode {
  pub fn new(function_name: String) -> Self {
    Self {
      function_name,
      ..Default::default()
    }
  }

  pub fn get_name(&self) -> &str {
    &self.function_name
  }

  pub unsafe fn push_constant(&mut self, val: ConstantValue) -> &mut Self {
    debug_assert_ne!(val, ConstantValue::None, "use push_constant_none");

    let constant_offset = self.constants.len() as u8;
    let constant_type = if matches!(val, ConstantValue::Str(_)) {
      OpCode::ConstantStr
    } else {
      OpCode::Constant
    };
    self.constants.push(val);
    self.code.push(constant_type as u8);
    self.code.push(constant_offset);
    self
  }

  pub unsafe fn push_constant_none(&mut self) {
    self.code.push(OpCode::Constant as u8);
    self.code.push(0);
  }

  pub unsafe fn push_stub_constant(&mut self) -> Address {
    let address = self.get_next_instruction_address();
    self.push_constant(ConstantValue::Stub);
    address
  }

  pub unsafe fn backpatch_constant(&mut self, address: Address, value: ConstantValue) {
    assert_eq!(self.code[address.0], OpCode::Constant as u8);
    let constant_index = self.code[address.0 + 1] as usize;
    assert_eq!(
      self.constants[constant_index],
      ConstantValue::Stub,
      "trying to replace a non stub constant"
    );
    self.constants[constant_index] = value;
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

  pub fn swap(&mut self, Address(start): Address, Address(mid): Address, Address(end): Address) {
    self.code[start..end].rotate_left(mid - start);
  }

  pub fn into_parts(self) -> (Vec<u8>, Vec<ConstantValue>) {
    (self.code, self.constants)
  }
}

impl Debug for FunctionCode {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut result = "".to_string();
    let mut index = 0;
    while index < self.code.len() {
      let code = unsafe { std::mem::transmute::<u8, OpCode>(self.code[index]) };
      result += &format!("   {index}: ");
      match code {
        OpCode::Constant | OpCode::ConstantStr => {
          index += 1;
          result += &format!(
            "Constant: {:?}\n",
            self.constants[self.code[index] as usize]
          );
        }
        OpCode::CallNative | OpCode::CallExtern | OpCode::CallValue => {
          index += 1;
          result += &format!("{code:?}: {}\n", self.code[index]);
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
