#[cfg(test)]
mod function_tests {

  use crate::{
    ast::{Item, VisItemKind},
    parser_utils::ParserContext,
    tests::support::parse_item,
  };

  fn parse_fn(input: &str) -> Result<(), ()> {
    let item = parse_item(input, "function_test_temp", ParserContext::Default)?;

    match item {
      Item::Vis(vis) => match vis.kind {
        VisItemKind::Function(_) => Ok(()),
        _ => Err(()),
      },
      _ => Err(()),
    }
  }

  fn assert_ok(input: &str) {
    assert!(parse_fn(input).is_ok(), "expected ok for {input:?}");
  }

  fn assert_err(input: &str) {
    assert!(parse_fn(input).is_err(), "expected error for {input:?}");
  }

  // Basic functions

  #[test]
  fn function_no_params_no_return() {
    assert_ok("fn foo() {}");
  }

  #[test]
  fn function_with_return_type() {
    assert_ok("fn foo() -> i32 { 1 }");
  }

  #[test]
  fn function_with_params() {
    assert_ok("fn foo(x: i32, y: i32) {}");
  }

  #[test]
  fn function_with_trailing_comma() {
    assert_ok("fn foo(x: i32, y: i32,) {}");
  }

  // Qualifiers

  #[test]
  fn function_const() {
    assert_ok("const fn foo() {}");
  }

  #[test]
  fn function_async() {
    assert_ok("async fn foo() {}");
  }

  #[test]
  fn function_unsafe() {
    assert_ok("unsafe fn foo() {}");
  }

  #[test]
  fn function_extern() {
    assert_ok("extern fn foo() {}");
  }

  #[test]
  fn function_extern_with_abi() {
    assert_ok(r#"extern "C" fn foo() {}"#);
  }

  #[test]
  fn function_all_qualifiers() {
    assert_ok(r#"const async unsafe extern "C" fn foo() {}"#);
  }

  // Generic params and where clause

  #[test]
  fn function_with_generics() {
    assert_ok("fn foo<T>() {}");
  }

  #[test]
  fn function_with_multiple_generics() {
    assert_ok("fn foo<T, U>() {}");
  }

  #[test]
  fn function_with_where_clause() {
    assert_ok("fn foo<T>() where T: Copy {}");
  }

  #[test]
  fn function_with_generics_and_where() {
    assert_ok("fn foo<T, U>() where T: Copy, U: Clone {}");
  }

  // Self parameters

  #[test]
  fn function_shorthand_self() {
    assert_ok("fn foo(self) {}");
  }

  #[test]
  fn function_mut_self() {
    assert_ok("fn foo(mut self) {}");
  }

  #[test]
  fn function_ref_self() {
    assert_ok("fn foo(&self) {}");
  }

  #[test]
  fn function_ref_mut_self() {
    assert_ok("fn foo(&mut self) {}");
  }

  #[test]
  fn function_typed_self() {
    assert_ok("fn foo(self: Box<Self>) {}");
  }

  // Variadic and patterns

  #[test]
  fn function_variadic_param() {
    assert_err("fn foo(x: i32, ...) {}");
  }

  #[test]
  fn function_pattern_param() {
    assert_ok("fn foo((x, y): (i32, i32)) {}");
  }

  // Foreign and associated functions

  #[test]
  fn foreign_function_item() {
    assert_err("extern fn foo();");
  }

  #[test]
  fn associated_function_with_body() {
    assert_ok("fn foo() {}");
  }

  #[test]
  fn associated_function_without_body() {
    assert_err("fn foo();");
  }

  // Error cases

  #[test]
  fn function_missing_name_errors() {
    assert_err("fn () {}");
  }

  #[test]
  fn function_missing_params_errors() {
    assert_err("fn foo {}");
  }

  #[test]
  fn function_missing_body_errors() {
    assert_err("fn foo()");
  }

  #[test]
  fn function_return_type_without_type_errors() {
    assert_err("fn foo() -> {}");
  }

  #[test]
  fn function_double_arrow_errors() {
    assert_err("fn foo() -> -> i32 {}");
  }

  #[test]
  fn function_invalid_param_syntax_errors() {
    assert_err("fn foo(x:) {}");
  }

  #[test]
  fn function_stray_tokens_errors() {
    assert_err("fn foo() {} 1");
  }
}
