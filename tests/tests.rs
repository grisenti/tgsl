use language::vm::{value::TaggedValue, VM};

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

fn assert(args: Vec<TaggedValue>) -> TaggedValue {
  assert!(unsafe { args[0].value.boolean });
  TaggedValue::none()
}

fn compile_and_run(test_file: &str) {
  let mut vm = VM::new();
  vm.load_module(
    include_str!("../tests/test_utils.wds"),
    vec![("assert", Box::new(assert))],
  )
  .expect("error in utils file");
  if let Err(msg) = vm.load_module(test_file, vec![]) {
    panic!("{}", msg);
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
  self_recursive_function
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
  reference_semantics
);

#[rustfmt::skip]
test_files!(ufc,
  primitive_types,
  aggregates,
  repeated,
  in_local_scope
);

#[rustfmt::skip]
test_files!(overloading,
  resolve_dot_call,
  resolve_simple
);

#[rustfmt::skip]
test_files!(variables,
  assign_to_variable,
  scope_resolution,
  variable_declaration
);
