use crate::compiler::debug_tree::DebugTree;
use crate::compiler::global_env::ModuleIndex;
use crate::compiler::lexer::SourceRange;
use crate::compiler::structs::MemberIndex;
use crate::compiler::types::FunctionSignature;
use crate::types::Type;
use std::rc::Rc;

pub type LocalVarIndex = u8;
pub type CapturedVarIndex = u8;
pub type GlobalVarIndex = u32;
pub type NativeFunctionIndex = u32;
pub type ForeignFunctionIndex = u32;

trait IrToDebugRepresentation {
  fn to_debug_representation(&self, ir: &Ir) -> DebugTree;
}

impl<T: IrToDebugRepresentation> IrToDebugRepresentation for Vec<T> {
  fn to_debug_representation(&self, ir: &Ir) -> DebugTree {
    DebugTree::ArrayNode(
      self
        .iter()
        .map(|item| item.to_debug_representation(&ir))
        .collect(),
    )
  }
}

impl<T: IrToDebugRepresentation> IrToDebugRepresentation for Option<T> {
  fn to_debug_representation(&self, ir: &Ir) -> DebugTree {
    match self {
      Some(value) => value.to_debug_representation(ir),
      None => DebugTree::Null,
    }
  }
}

#[derive(Debug, Clone)]
pub enum Constant {
  Str(String),
  Num(f64),
  Bool(bool),
}

impl From<Constant> for ExprIr {
  fn from(value: Constant) -> Self {
    Self::Constant(value)
  }
}

impl IrToDebugRepresentation for Constant {
  fn to_debug_representation(&self, _: &Ir) -> DebugTree {
    DebugTree::Value(format!("{:?}", self))
  }
}

impl IrToDebugRepresentation for u32 {
  fn to_debug_representation(&self, _: &Ir) -> DebugTree {
    DebugTree::Value(format!("{}", self))
  }
}

impl IrToDebugRepresentation for u8 {
  fn to_debug_representation(&self, _: &Ir) -> DebugTree {
    DebugTree::Value(format!("{}", self))
  }
}

impl IrToDebugRepresentation for Type {
  fn to_debug_representation(&self, _: &Ir) -> DebugTree {
    DebugTree::Value(format!("{}", self))
  }
}

impl IrToDebugRepresentation for MemberIndex {
  fn to_debug_representation(&self, _: &Ir) -> DebugTree {
    DebugTree::Value(format!("{:?}", self))
  }
}

impl IrToDebugRepresentation for Capture {
  fn to_debug_representation(&self, _: &Ir) -> DebugTree {
    DebugTree::Value(format!("{:?}", self))
  }
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolIndex {
  Global {
    index: GlobalVarIndex,
    module_index: ModuleIndex,
  },
  Local(LocalVarIndex),
  Capture(CapturedVarIndex),
  NativeFunction {
    index: u32,
    module_index: u32,
  },
  ForeignFunction {
    index: u32,
    module_index: u32,
  },
}

#[derive(Debug, Clone, Copy)]
pub enum VarIndex {
  Local(LocalVarIndex),
  Capture(CapturedVarIndex),
  Global {
    index: GlobalVarIndex,
    module_index: ModuleIndex,
  },
}

impl From<SymbolIndex> for ExprIr {
  fn from(value: SymbolIndex) -> Self {
    Self::SymbolIndex(value)
  }
}

impl IrToDebugRepresentation for VarIndex {
  fn to_debug_representation(&self, _: &Ir) -> DebugTree {
    DebugTree::Value(format!("{:?}", self))
  }
}

impl IrToDebugRepresentation for SymbolIndex {
  fn to_debug_representation(&self, _: &Ir) -> DebugTree {
    DebugTree::Value(format!("{:?}", self))
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinOperation {
  NegNum,
  AddNum,
  SubNum,
  MulNum,
  DivNum,
  LeNum,
  GeNum,
  LeqNum,
  GeqNum,
  SameNum,
  DiffNum,

  AddStr,
  LeStr,
  GeStr,
  LeqStr,
  GeqStr,
  SameStr,
  DiffStr,

  NotBool,
  SameBool,
  DiffBool,
}

impl IrToDebugRepresentation for BuiltinOperation {
  fn to_debug_representation(&self, _: &Ir) -> DebugTree {
    DebugTree::Value(format!("{:?}", self))
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Capture {
  Local(u8),
  Capture(u8),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IrExprHandle(u32);

impl IrExprHandle {
  pub const INVALID: Self = Self(u32::MAX);
}

macro_rules! expr_ir {
  ($name:tt, $($member:ident : $t:ty),+) => {
    #[derive(Debug, Clone)]
    pub struct $name {
      $(pub $member: $t),+
    }

    impl From<$name> for ExprIr {
      fn from(value: $name) -> Self {
        Self::$name(value)
      }
    }

    impl IrToDebugRepresentation for $name {
      fn to_debug_representation(&self, ir: &Ir) -> DebugTree {
        DebugTree::ObjectNode {
          object_type: stringify!($name),
          names: vec![$(stringify!($member)),+],
          values: vec![$(self.$member.to_debug_representation(ir)),+]
        }
      }
    }
  };
}

expr_ir!(Assignment,
  target: VarIndex,
  value: IrExprHandle
);

expr_ir!(BuiltinBinary,
  operation: BuiltinOperation,
  lhs: IrExprHandle,
  rhs: IrExprHandle
);

expr_ir!(And,
  lhs: IrExprHandle,
  rhs: IrExprHandle
);

expr_ir!(Or,
  lhs: IrExprHandle,
  rhs: IrExprHandle
);

expr_ir!(BuiltinUnary,
  operation: BuiltinOperation,
  operand: IrExprHandle
);

expr_ir!(CallBuiltin,
  operation: BuiltinOperation,
  arguments: Vec<IrExprHandle>
);

expr_ir!(CallNative,
  function_id: u32,
  function_module_id: u32,
  arguments: Vec<IrExprHandle>
);

expr_ir!(CallForeign,
  function_id: u32,
  function_module_id: u32,
  arguments: Vec<IrExprHandle>
);

expr_ir!(CallValue,
  value: IrExprHandle,
  arguments: Vec<IrExprHandle>
);

expr_ir!(GetMember,
  lhs: IrExprHandle,
  member_index: MemberIndex
);

expr_ir!(SetMember,
  lhs: IrExprHandle,
  member_index: MemberIndex,
  value: IrExprHandle
);

expr_ir!(Construct,
  arguments: Vec<IrExprHandle>
);

expr_ir!(CreateClosure,
  function_id: u32,
  captures: Vec<Capture>
);

pub enum ExprIr {
  Constant(Constant),
  SymbolIndex(SymbolIndex),
  Assignment(Assignment),
  BuiltinBinary(BuiltinBinary),
  And(And),
  Or(Or),
  BuiltinUnary(BuiltinUnary),
  CallBuiltin(CallBuiltin),
  CallNative(CallNative),
  CallForeign(CallForeign),
  CallValue(CallValue),
  GetMember(GetMember),
  SetMember(SetMember),
  Construct(Construct),
  CreateClosure(CreateClosure),
}

impl IrToDebugRepresentation for IrExprHandle {
  fn to_debug_representation(&self, ir: &Ir) -> DebugTree {
    match ir.get_expression(*self) {
      ExprIr::Constant(value) => value.to_debug_representation(ir),
      ExprIr::SymbolIndex(value) => value.to_debug_representation(ir),
      ExprIr::Assignment(value) => value.to_debug_representation(ir),
      ExprIr::BuiltinBinary(value) => value.to_debug_representation(ir),
      ExprIr::And(value) => value.to_debug_representation(ir),
      ExprIr::Or(value) => value.to_debug_representation(ir),
      ExprIr::BuiltinUnary(value) => value.to_debug_representation(ir),
      ExprIr::CallBuiltin(value) => value.to_debug_representation(ir),
      ExprIr::CallNative(value) => value.to_debug_representation(ir),
      ExprIr::CallForeign(value) => value.to_debug_representation(ir),
      ExprIr::CallValue(value) => value.to_debug_representation(ir),
      ExprIr::GetMember(value) => value.to_debug_representation(ir),
      ExprIr::SetMember(value) => value.to_debug_representation(ir),
      ExprIr::Construct(value) => value.to_debug_representation(ir),
      ExprIr::CreateClosure(value) => value.to_debug_representation(ir),
    }
  }
}

pub struct ExpressionInfo {
  pub type_: Type,
  pub source_range: SourceRange,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IrStmtHandle(u32);

impl IrStmtHandle {
  pub const INVALID: Self = Self(u32::MAX);
}

impl IrToDebugRepresentation for IrStmtHandle {
  fn to_debug_representation(&self, ir: &Ir) -> DebugTree {
    match ir.get_statement(*self) {
      StmtIr::VarDecl(value) => value.to_debug_representation(ir),
      StmtIr::Expr(value) => value.to_debug_representation(ir),
      StmtIr::Block(value) => value.to_debug_representation(ir),
      StmtIr::IfBranch(value) => value.to_debug_representation(ir),
      StmtIr::While(value) => value.to_debug_representation(ir),
      StmtIr::Return(value) => value.to_debug_representation(ir),
    }
  }
}

macro_rules! stmt_ir {
  ($name:tt, $($member:ident : $t:ty),+) => {
    #[derive(Debug, Clone)]
    pub struct $name {
      $(pub $member: $t),+
    }

    impl From<$name> for StmtIr {
      fn from(value: $name) -> Self {
        Self::$name(value)
      }
    }

    impl IrToDebugRepresentation for $name {
      fn to_debug_representation(&self, ir: &Ir) -> DebugTree {
        DebugTree::ObjectNode {
          object_type: stringify!($name),
          names: vec![$(stringify!($member)),+],
          values: vec![$(self.$member.to_debug_representation(ir)),+]
        }
      }
    }
  };
}

stmt_ir!(VarDecl,
  identifier: VarIndex,
  type_: Type,
  init_expr: IrExprHandle
);

stmt_ir!(Block,
  statements: Vec<IrStmtHandle>,
  locals_count: u8
);

stmt_ir!(IfBranch,
  condition: IrExprHandle,
  true_branch: IrStmtHandle,
  else_branch: Option<IrStmtHandle>
);

stmt_ir!(While,
  condition: IrExprHandle,
  loop_body: IrStmtHandle
);

stmt_ir!(Return,
  value: Option<IrExprHandle>
);

impl From<IrExprHandle> for StmtIr {
  fn from(value: IrExprHandle) -> Self {
    Self::Expr(value)
  }
}

pub enum StmtIr {
  VarDecl(VarDecl),
  Expr(IrExprHandle),
  Block(Block),
  IfBranch(IfBranch),
  While(While),
  Return(Return),
}

pub struct FunctionIr {
  pub name: Rc<str>,
  pub index: u32,
  pub signature: FunctionSignature,
  pub source_range: SourceRange,
  pub instructions: Vec<IrStmtHandle>,
}

#[derive(Default)]
pub struct Ir {
  expressions: Vec<ExprIr>,
  expression_info: Vec<ExpressionInfo>,
  statements: Vec<StmtIr>,
  statement_source_range: Vec<SourceRange>,
  functions: Vec<FunctionIr>,
  startup_instructions: Vec<IrStmtHandle>,
}

impl Ir {
  pub fn add_expression(&mut self, expr: ExprIr, info: ExpressionInfo) -> IrExprHandle {
    self.expressions.push(expr);
    self.expression_info.push(info);
    IrExprHandle(self.expressions.len() as u32 - 1)
  }

  pub fn get_expression(&self, handle: IrExprHandle) -> &ExprIr {
    &self.expressions[handle.0 as usize]
  }

  pub fn get_expression_info(&self, handle: IrExprHandle) -> &ExpressionInfo {
    &self.expression_info[handle.0 as usize]
  }

  pub fn add_statement(&mut self, statement: StmtIr, source_range: SourceRange) -> IrStmtHandle {
    self.statements.push(statement);
    self.statement_source_range.push(source_range);
    IrStmtHandle(self.statements.len() as u32 - 1)
  }

  pub fn get_statement(&self, handle: IrStmtHandle) -> &StmtIr {
    &self.statements[handle.0 as usize]
  }

  pub fn get_statement_source_range(&self, handle: IrStmtHandle) -> SourceRange {
    self.statement_source_range[handle.0 as usize]
  }

  pub fn add_function(&mut self, func: FunctionIr) {
    self.functions.push(func);
  }

  pub fn add_startup_instruction(&mut self, stmt: IrStmtHandle) {
    self.startup_instructions.push(stmt);
  }

  pub fn to_debug_tree(&self) -> DebugTree {
    let mut result = Vec::new();
    for stmt in &self.startup_instructions {
      result.push(stmt.to_debug_representation(self));
    }
    DebugTree::ArrayNode(result)
  }
}
