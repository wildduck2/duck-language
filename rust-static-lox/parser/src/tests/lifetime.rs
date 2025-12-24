#[cfg(test)]
mod lifetime_tests {
  use crate::{parser_utils::ParserContext, tests::support::run_parser};

  fn parse_lifetime(input: &str) -> Result<String, ()> {
    run_parser(input, "lifetime_parse_test_temp", |parser| parser.parse_lifetime())
  }

  fn parse_lifetimes(input: &str) -> Result<Vec<String>, ()> {
    run_parser(input, "lifetimes_parse_test_temp", |parser| parser.parse_lifetimes())
  }

  fn parse_for_lifetimes(input: &str) -> Result<Option<Vec<String>>, ()> {
    run_parser(input, "for_lifetimes_test_temp", |parser| parser.parse_for_lifetimes())
  }

  fn parse_lifetime_bounds(input: &str) -> Result<Vec<String>, ()> {
    run_parser(input, "lifetime_bounds_test_temp", |parser| parser.parse_lifetime_bounds())
  }

  #[test]
  fn parses_single_lifetime() {
    assert_eq!(parse_lifetime("'a").unwrap(), "'a");
  }

  #[test]
  fn parses_static_lifetime() {
    assert_eq!(parse_lifetime("'static").unwrap(), "'static");
  }

  #[test]
  fn errors_on_missing_lifetime_token() {
    assert!(parse_lifetime("a").is_err(), "expected error for missing lifetime");
  }

  #[test]
  fn parses_multiple_lifetimes() {
    assert_eq!(
      parse_lifetimes("'a, 'b, 'c").unwrap(),
      vec!["'a".to_string(), "'b".to_string(), "'c".to_string()]
    );
  }

  #[test]
  fn trailing_comma_is_allowed_in_lifetimes() {
    assert_eq!(
      parse_lifetimes("'a, 'b,").unwrap(),
      vec!["'a".to_string(), "'b".to_string()]
    );
  }

  #[test]
  fn missing_comma_between_lifetimes_errors() {
    assert!(parse_lifetimes("'a 'b").is_err(), "expected error for missing comma");
  }

  #[test]
  fn for_lifetimes_absent_returns_none() {
    assert_eq!(parse_for_lifetimes("").unwrap(), None);
  }

  #[test]
  fn parses_for_lifetimes() {
    assert_eq!(
      parse_for_lifetimes("for<'a, 'b>").unwrap(),
      Some(vec!["'a".to_string(), "'b".to_string()])
    );
  }

  #[test]
  fn for_lifetimes_trailing_comma_is_allowed() {
    assert_eq!(
      parse_for_lifetimes("for<'a,>").unwrap(),
      Some(vec!["'a".to_string()])
    );
  }

  #[test]
  fn for_lifetimes_empty_errors() {
    assert!(parse_for_lifetimes("for<>").is_err(), "expected error for empty for<>");
  }

  #[test]
  fn for_lifetimes_unclosed_errors() {
    assert!(parse_for_lifetimes("for<'a").is_err(), "expected error for unclosed for<...>");
  }

  #[test]
  fn parses_lifetime_bounds() {
    assert_eq!(
      parse_lifetime_bounds("'a + 'b + 'c").unwrap(),
      vec!["'a".to_string(), "'b".to_string(), "'c".to_string()]
    );
  }

  #[test]
  fn empty_lifetime_bounds_is_ok() {
    assert_eq!(parse_lifetime_bounds("").unwrap(), Vec::<String>::new());
  }

  #[test]
  fn lifetime_bounds_trailing_plus_errors() {
    assert!(parse_lifetime_bounds("'a +").is_err(), "expected error for trailing plus");
  }

  #[test]
  fn lifetime_bounds_invalid_token_after_plus_errors() {
    assert!(parse_lifetime_bounds("'a + T").is_err(), "expected error for invalid bound");
  }

  #[test]
  fn lifetime_in_reference_type() {
    let result = run_parser("&'a i32", "lifetime_ref_type_temp", |parser| {
      parser.parse_type(ParserContext::Type)
    });
    assert!(result.is_ok(), "expected reference type with lifetime to parse");
  }
}
