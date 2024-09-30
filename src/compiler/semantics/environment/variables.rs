// use crate::compiler::semantics::environment::{Environment, Local};
// use crate::compiler::types::Type;
// use crate::compiler::variables::{
//   CaptureAddress, GlobalVarAddress, GlobalVarDeclarationResult, LocalAddress,
//   LocalVarDeclarationError, LocalVarDeclarationResult, RelativeGlobalVarAddress,
// };
// use std::rc::Rc;
//
// impl<'a> Environment<'a> {
//   fn is_local_in_current_scope(&self, local_name: &str) -> bool {
//     self
//       .locals
//       .iter()
//       .rev()
//       .take(self.names_in_current_scope as usize)
//       .any(|local| local_name == local.name.as_ref())
//   }
//
//   pub fn declare_local_var(
//     &mut self,
//     name: &str,
//     var_type: Type,
//   ) -> LocalVarDeclarationResult<LocalAddress> {
//     assert!(!self.in_global_scope());
//
//     if self.local_names == u8::MAX {
//       return Err(LocalVarDeclarationError::TooManyLocalNames);
//     }
//     if self.is_local_in_current_scope(name) {
//       return Err(LocalVarDeclarationError::AlreadyDefined);
//     }
//     let id = self.local_names;
//     let local = Local {
//       name: Rc::from(name),
//       id,
//       scope_local_id: self.names_in_current_scope,
//       function_depth: self.functions_declaration_stack.len() as u8,
//       type_: var_type,
//     };
//     self.local_names += 1;
//     self.names_in_current_scope += 1;
//     self.locals.push(local);
//     Ok(id)
//   }
//
//   pub fn get_local_var(&self, name: &str) -> Option<(LocalAddress, Type)> {
//     self
//       .locals
//       .iter()
//       .rev()
//       .take(self.local_names as usize)
//       .find(|local| local.name.as_ref() == name)
//       .map(|local| (local.id, local.type_.clone()))
//   }
//
//   fn find_local(&self, name: &str) -> Option<&Local> {
//     self.locals.iter().find(|local| local.name.as_ref() == name)
//   }
//
//   pub fn get_capture_or_capture_var(&mut self, name: &str) -> Option<(CaptureAddress, Type)> {
//     debug_assert!(
//       self.get_local_var(name).is_none(),
//       "variable is a local, should use that instead"
//     );
//
//     let local = self.find_local(name)?;
//     let local_id = local.id;
//     let local_type = local.type_.clone();
//
//     let current_depth = self.functions_declaration_stack.len();
//     // we know that the local is not in this current function from the above debug_assert,
//     // so we can use `local.function_depth` as the index of the first not popped function inside
//     // the local's scope.
//     let start_depth = local.function_depth as usize;
//
//     let mut capture_id =
//       self.functions_declaration_stack[start_depth].get_or_capture_above_local(local_id);
//
//     if start_depth + 1 < current_depth {
//       for func in &mut self.functions_declaration_stack[start_depth + 1..current_depth] {
//         capture_id = func.get_or_capture_above_capture(capture_id);
//       }
//     }
//     Some((capture_id, local_type))
//   }
//
//   pub fn declare_global_var(
//     &mut self,
//     name: &str,
//     type_: Type,
//   ) -> GlobalVarDeclarationResult<RelativeGlobalVarAddress> {
//     if self.global_functions.get_overload_set(name).is_some() {
//       todo!()
//     } else if self.is_type(name) {
//       todo!()
//     }
//
//     self.global_variables.declare(name, type_)
//   }
//
//   fn check_order(&self, name: &str) {
//     debug_assert!(
//       self.get_local_var(name).is_none(),
//       "variable is a local, should use that first"
//     );
//     debug_assert!(
//       self.find_local(name).is_none(),
//       "variable available for capture, should use that first"
//     );
//   }
//
//   pub fn get_global_var(&self, name: &str) -> Option<(GlobalVarAddress, Type)> {
//     self.check_order(name);
//     self.global_variables.get(name)
//   }
// }
