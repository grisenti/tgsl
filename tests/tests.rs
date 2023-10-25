use tgsl::module::Module;
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

fn assert(value: bool) {
  assert!(value);
}

fn compile_and_run(test_file: &str) {
  let mut tgsl = Tgsl::default();
  let mut assert_module = Module::new(include_str!("../tests/test_utils.tgsl"));
  assert_module.add_function("assert", assert);
  tgsl
    .load_module(assert_module)
    .expect("error in utils file");
  if let Err(msg) = tgsl.load_module(Module::new(test_file)) {
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
  use tgsl::module::Module;
  use tgsl::Tgsl;

  use crate::assert;

  fn compile_and_run_multiple(test_files: &[&str]) {
    let mut tgsl = Tgsl::default();
    let mut assert_module = Module::new(include_str!("../tests/test_utils.tgsl"));
    assert_module.add_function("assert", assert);
    tgsl
      .load_module(assert_module)
      .expect("error in utils file");
    for source in test_files {
      if let Err(msg) = tgsl.load_module(Module::new(source)) {
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
