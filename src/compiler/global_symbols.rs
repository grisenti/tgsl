use crate::compiler::functions::overload_set::OverloadSet;
use crate::compiler::global_env::ModuleIndex;
use crate::compiler::structs::Struct;
use crate::types::Type;

pub type GlobalVarIndex = u32;

pub enum GlobalSymbol {
  GlobalVariable {
    index: GlobalVarIndex,
    module_index: ModuleIndex,
    type_: Type,
  },
  OverloadSet(OverloadSet),
  Struct(Struct),
  UndefinedStruct,
}
