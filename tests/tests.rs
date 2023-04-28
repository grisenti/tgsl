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
  if let Err(msg) = vm.load_module(
    "test".to_string(),
    test_file,
    vec![("assert", Box::new(assert))],
  ) {
    panic!("{}", msg);
  }
}

test_files!(closures, capture_single_one_level, capture_copy);
test_files!(ufc, primitive_types, aggregates, repeated, in_local_scope);
test_files!(structs, construction, member_access, nesting);
test_files!(
  misc,
  function_declaration,
  passing_closures,
  primitive_operations,
  recursive_function,
  struct_declaration,
  struct_construction,
  mutually_recursive_functions,
  conditional_return_types,
  if_condition
);
