use crate::compiler::bytecode::OpCode;

use super::{
  ast::Identifier,
  bytecode::{Chunk, Function, TaggedValue},
};

pub struct Label(usize);
pub struct JumpPoint(usize);

pub struct BytecodeBuilder {
  code: Vec<u8>,
  functions: Vec<Function>,
  constants: Vec<TaggedValue>,
}

impl BytecodeBuilder {
  pub unsafe fn push_constant(&mut self, val: TaggedValue) {
    let constant_offset = self.constants.len() as u8;
    self.constants.push(val);
    self.code.push(OpCode::Constant as u8);
    self.code.push(constant_offset);
  }

  pub unsafe fn push_function(&mut self, func: Function) {
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

  pub unsafe fn get_id(&mut self, id: Identifier) {
    match id {
      Identifier::Global(gid) => {
        self.push_constant(TaggedValue::global_id(gid));
        self.push_op(OpCode::GetGlobal);
      }
      Identifier::Local(id) => {
        self.push_op2(OpCode::GetLocal, id);
      }
      Identifier::Capture(id) => {
        self.push_op2(OpCode::GetCapture, id);
      }
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

  pub fn finalize(self) -> Chunk {
    Chunk {
      code: self.code,
      functions: self.functions,
      constants: self.constants,
    }
  }

  pub fn new() -> Self {
    Self {
      code: Vec::new(),
      constants: vec![TaggedValue::none()],
      functions: Vec::new(),
    }
  }
}
