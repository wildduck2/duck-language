#[cfg(test)]
mod static_tests {
  use crate::{parser_utils::ParserContext, tests::support::parse_item};

  fn parse_static_item(input: &str) -> Result<(), ()> {
    parse_item(input, "static_decl_test_temp", ParserContext::Default).map(|_| ())
  }

  #[test]
  fn static_without_init_errors_for_now() {
    assert!(parse_static_item("static X i32").is_err());
  }

  #[test]
  fn static_with_init_errors_for_now() {
    assert!(parse_static_item("static mut X i32 = 1").is_err());
  }
}
