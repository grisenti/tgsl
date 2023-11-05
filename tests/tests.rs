use tgsl::errors::RuntimeError;
use tgsl::library::Library;
use tgsl::Tgsl;

macro_rules! test_files {
  ($module:ident, $($test:ident),+) => {
	mod $module {
			  use super::compile_and_run;
	$(
	  #[test]
	  #[allow(non_snake_case)]
	  fn $test() {
		compile_and_run(include_str!(
		  concat!("../tests/", stringify!($module), "/", stringify!($test), ".tgsl")
		));
	  }
		)+
	}
  };
}

fn assert(value: bool) -> Result<(), RuntimeError> {
  if value {
    Ok(())
  } else {
    Err(RuntimeError::Panic("failed assertion".into()))
  }
}

struct TestUtils();

impl Library for TestUtils {
  type Context = ();

  fn load(&mut self, tgls: &mut Tgsl) {
    tgls
      .load_module(include_str!("../tests/test_utils.tgsl"))
      .bind_function("assert", assert)
      .execute()
      .expect("errors loading utils file")
  }

  fn context(self) -> Option<Self::Context> {
    None
  }
}

fn compile_and_run(test_file: &str) {
  let mut tgsl = Tgsl::default();
  tgsl.load_library(TestUtils());
  if let Err(msg) = tgsl.load_module(test_file).execute() {
    panic!("{:?}", msg);
  }
}

#[rustfmt::skip]
test_files!(closures,
  capture_copy,
  capture_single_multi_level,
  capture_single_one_level,
  empty_return_type_does_not_need_to_return
);

#[rustfmt::skip]
test_files!(functions,
  conditional_return_types,
  function_call_before_definition,
  function_call,
  mutually_recursive_functions,
  self_recursive_function,
  function_definition
);

#[rustfmt::skip]
test_files!(misc,
  passing_closures,
  primitive_operations,
  if_condition
);

#[rustfmt::skip]
test_files!(structs,
  construction,
  definition,
  member_access,
  member_set,
  nesting,
  reference_semantics,
  mutually_recursive
);

#[rustfmt::skip]
test_files!(ufc,
  primitive_types,
  aggregates,
  repeated,
  in_local_scope,
  call_struct_member
);

#[rustfmt::skip]
test_files!(overloading,
  resolve_dot_call,
  resolve_simple,
  single_overload_can_be_assigned_and_called
);

#[rustfmt::skip]
test_files!(variables,
  assign_to_variable,
  scope_resolution,
  variable_declaration
);

mod modules {
  use tgsl::Tgsl;

  use crate::TestUtils;

  fn compile_and_run_multiple(test_files: &[&str]) {
    let mut tgsl = Tgsl::default();
    tgsl.load_library(TestUtils());
    for source in test_files {
      if let Err(msg) = tgsl.load_module(source).execute() {
        panic!("{:?}", msg);
      }
    }
  }

  macro_rules! modules_test {
  ($test_name:ident, $($test_file:ident),+) => {
    #[test]
    #[allow(non_snake_case)]
    fn $test_name() {
      compile_and_run_multiple(
        &[$(include_str!(concat!("../tests/modules/", stringify!($test_name), "/", stringify!($test_file), ".tgsl"))),+]
      );
    }

  };
}

  modules_test!(access_global, decl, access);

  modules_test!(multiple_imports, mod1, mod2, main);

  modules_test!(access_struct, struct_decl, access);
}
