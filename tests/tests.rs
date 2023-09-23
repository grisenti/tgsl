use language::vm::extern_function::ExternFunction;
use language::vm::{VM};

macro_rules! test_files {
  ($module:ident, $($test:ident),+) => {
	mod $module {
			  use super::compile_and_run;
	$(
	  #[test]
	  #[allow(non_snake_case)]
	  fn $test() {
		compile_and_run(include_str!(
		  concat!("../tests/", stringify!($module), "/", stringify!($test), ".wds")
		));
	  }
		)+
	}
  };
}

macro_rules! modules_test {
  ($test_name:ident, $($test_file:ident),+) => {
    #[test]
    #[allow(non_snake_case)]
    fn $test_name() {
      compile_and_run_multiple(
        &[$(include_str!(concat!("../tests/modules/", stringify!($test_name), "/", stringify!($test_file), ".wds"))),+]
      );
    }

  };
}

fn assert(value: bool) {
  assert!(value);
}

fn compile_and_run(test_file: &str) {
  let mut vm = VM::new();
  vm.load_module(
    include_str!("../tests/test_utils.wds"),
    vec![ExternFunction::create("assert", assert)],
  )
  .expect("error in utils file");
  if let Err(msg) = vm.load_module(test_file, vec![]) {
    panic!("{}", msg);
  }
}

fn compile_and_run_multiple(test_files: &[&str]) {
  let mut vm = VM::new();
  vm.load_module(
    include_str!("../tests/test_utils.wds"),
    vec![ExternFunction::create("assert", assert)],
  )
  .expect("error in utils file");

  for test_file in test_files {
    if let Err(msg) = vm.load_module(test_file, vec![]) {
      panic!("{}", msg);
    }
  }
}

#[rustfmt::skip]
test_files!(closures,
  capture_copy,
  capture_single_multi_level,
  capture_single_one_level
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

modules_test!(access_global, decl, access);

modules_test!(multiple_imports, mod1, mod2, main);

modules_test!(access_struct, struct_decl, access);
