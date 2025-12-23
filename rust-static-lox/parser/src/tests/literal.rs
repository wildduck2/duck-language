#[cfg(test)]
mod literal_tests {

  use crate::{
    ast::expr::{ExprKind, Lit},
    parser_utils::ParserContext,
    tests::support::parse_primary_expr,
  };

  fn parse_single(input: &str) -> Result<ExprKind, ()> {
    parse_primary_expr(input, "literal_test_temp", ParserContext::Default)
  }

  fn assert_err(input: &str) {
    assert!(
      parse_single(input).is_err(),
      "expected error for: {:?}",
      input
    );
  }

  fn assert_lit(input: &str, expected: ExprKind) {
    let expr = parse_single(input).unwrap();
    assert_eq!(expr, expected);
  }

  // Character literals
  #[test]
  fn test_char_simple() {
    assert_lit("'a'", ExprKind::Literal(Lit::Char('a')));
  }

  #[test]
  fn test_char_newline() {
    assert_lit("'\\n'", ExprKind::Literal(Lit::Char('\n')));
  }

  #[test]
  fn test_char_tab() {
    assert_lit("'\\t'", ExprKind::Literal(Lit::Char('\t')));
  }

  #[test]
  fn test_char_carriage_return() {
    assert_lit("'\\r'", ExprKind::Literal(Lit::Char('\r')));
  }

  #[test]
  fn test_char_backslash() {
    assert_lit("'\\\\'", ExprKind::Literal(Lit::Char('\\')));
  }

  #[test]
  fn test_char_null() {
    assert_lit("'\\0'", ExprKind::Literal(Lit::Char('\0')));
  }

  #[test]
  fn test_char_hex() {
    assert_lit("'\\x41'", ExprKind::Literal(Lit::Char('A')));
  }

  #[test]
  fn test_char_long_hex_escape_should_error() {
    assert_err("'\\x4142'");
  }

  // String literals
  #[test]
  fn test_string_simple() {
    assert_lit(
      "\"hello\"",
      ExprKind::Literal(Lit::String {
        value: "hello".to_string(),
        raw_hashes: None,
      }),
    );
  }

  #[test]
  fn test_string_escaped_quote() {
    assert_lit(
      "\"escaped \\\" quote\"",
      ExprKind::Literal(Lit::String {
        value: "escaped \" quote".to_string(),
        raw_hashes: None,
      }),
    );
  }

  #[test]
  fn test_string_multi_line() {
    assert_lit(
      "\"multi\\nline\\nstring\"",
      ExprKind::Literal(Lit::String {
        value: "multi\nline\nstring".to_string(),
        raw_hashes: None,
      }),
    );
  }

  #[test]
  fn test_string_with_tab() {
    assert_lit(
      "\"with\\ttab\"",
      ExprKind::Literal(Lit::String {
        value: "with\ttab".to_string(),
        raw_hashes: None,
      }),
    );
  }

  #[test]
  fn test_string_with_backslash() {
    assert_lit(
      "\"with\\\\backslash\"",
      ExprKind::Literal(Lit::String {
        value: "with\\backslash".to_string(),
        raw_hashes: None,
      }),
    );
  }

  // Raw strings
  #[test]
  fn test_raw_string() {
    assert_lit(
      "r\"raw string \\n not escaped\"",
      ExprKind::Literal(Lit::String {
        value: r"raw string \n not escaped".to_string(),
        raw_hashes: Some(0),
      }),
    );
  }

  #[test]
  fn test_raw_string_with_quotes() {
    assert_lit(
      "r#\"raw string with \"quotes\"\"#",
      ExprKind::Literal(Lit::String {
        value: r#"raw string with "quotes""#.to_string(),
        raw_hashes: Some(1),
      }),
    );
  }

  #[test]
  fn test_raw_string_nested_hashes() {
    assert_lit(
      "r##\"raw string with #\"nested\"# quotes\"##",
      ExprKind::Literal(Lit::String {
        value: r##"raw string with #"nested"# quotes"##.to_string(),
        raw_hashes: Some(2),
      }),
    );
  }

  // Byte literals
  #[test]
  fn test_byte_simple() {
    assert_lit("b'a'", ExprKind::Literal(Lit::Byte(97)));
  }

  #[test]
  fn test_byte_newline() {
    assert_lit("b'\\n'", ExprKind::Literal(Lit::Byte(10)));
  }

  #[test]
  fn test_byte_tab() {
    assert_lit("b'\\t'", ExprKind::Literal(Lit::Byte(9)));
  }

  #[test]
  fn test_byte_backslash() {
    assert_lit("b'\\\\'", ExprKind::Literal(Lit::Byte(92)));
  }

  #[test]
  fn test_byte_quote() {
    assert_lit("b'\\''", ExprKind::Literal(Lit::Byte(39)));
  }

  #[test]
  fn test_byte_null() {
    assert_lit("b'\\0'", ExprKind::Literal(Lit::Byte(0)));
  }

  #[test]
  fn test_byte_hex() {
    assert_lit("b'\\x41'", ExprKind::Literal(Lit::Byte(65)));
  }

  // Byte string literals
  #[test]
  fn test_byte_string_simple() {
    assert_lit(
      "b\"byte string\"",
      ExprKind::Literal(Lit::ByteString {
        value: b"byte string".to_vec(),
        raw_hashes: None,
      }),
    );
  }

  #[test]
  fn test_byte_string_escaped_quote() {
    assert_lit(
      "b\"escaped \\\" quote\"",
      ExprKind::Literal(Lit::ByteString {
        value: b"escaped \" quote".to_vec(),
        raw_hashes: None,
      }),
    );
  }

  #[test]
  fn test_byte_string_multi_line() {
    assert_lit(
      "b\"multi\\nline\"",
      ExprKind::Literal(Lit::ByteString {
        value: b"multi\nline".to_vec(),
        raw_hashes: None,
      }),
    );
  }

  #[test]
  fn test_byte_string_quote() {
    assert_lit(
      "b\"\\\"\"",
      ExprKind::Literal(Lit::ByteString {
        value: vec![34],
        raw_hashes: None,
      }),
    );
  }

  // Raw byte strings
  #[test]
  fn test_raw_byte_string() {
    assert_lit(
      "br\"raw byte string \\n not escaped\"",
      ExprKind::Literal(Lit::ByteString {
        value: br"raw byte string \n not escaped".to_vec(),
        raw_hashes: Some(0),
      }),
    );
  }

  #[test]
  fn test_raw_byte_string_with_quotes() {
    assert_lit(
      "br#\"raw byte string with \"quotes\"\"#",
      ExprKind::Literal(Lit::ByteString {
        value: br#"raw byte string with "quotes""#.to_vec(),
        raw_hashes: Some(1),
      }),
    );
  }

  // C strings
  #[test]
  fn test_c_string_simple() {
    assert_lit(
      "c\"c string\"",
      ExprKind::Literal(Lit::String {
        value: "c string".to_string(),
        raw_hashes: None,
      }),
    );
  }

  #[test]
  fn test_c_string_escaped_quote() {
    assert_lit(
      "c\"escaped \\\" quote\"",
      ExprKind::Literal(Lit::String {
        value: "escaped \" quote".to_string(),
        raw_hashes: None,
      }),
    );
  }

  #[test]
  fn test_raw_c_string() {
    assert_lit(
      "cr\"raw c string \\n not escaped\"",
      ExprKind::Literal(Lit::String {
        value: r"raw c string \n not escaped".to_string(),
        raw_hashes: Some(0),
      }),
    );
  }

  #[test]
  fn test_raw_c_string_with_quotes() {
    assert_lit(
      "cr#\"raw c string with \"quotes\"\"#",
      ExprKind::Literal(Lit::String {
        value: r#"raw c string with "quotes""#.to_string(),
        raw_hashes: Some(1),
      }),
    );
  }

  // Integers
  #[test]
  fn test_integer_zero() {
    assert_lit(
      "0",
      ExprKind::Literal(Lit::Integer {
        value: 0,
        suffix: None,
      }),
    );
  }

  #[test]
  fn test_integer_decimal() {
    assert_lit(
      "42",
      ExprKind::Literal(Lit::Integer {
        value: 42,
        suffix: None,
      }),
    );
  }

  #[test]
  fn test_integer_with_underscores() {
    assert_lit(
      "1_000_000",
      ExprKind::Literal(Lit::Integer {
        value: 1_000_000,
        suffix: None,
      }),
    );
  }

  #[test]
  fn test_integer_hex() {
    assert_lit(
      "0xFF",
      ExprKind::Literal(Lit::Integer {
        value: 255,
        suffix: None,
      }),
    );
  }

  #[test]
  fn test_integer_binary() {
    assert_lit(
      "0b101010",
      ExprKind::Literal(Lit::Integer {
        value: 42,
        suffix: None,
      }),
    );
  }

  #[test]
  fn test_integer_octal() {
    assert_lit(
      "0o755",
      ExprKind::Literal(Lit::Integer {
        value: 493,
        suffix: None,
      }),
    );
  }

  // Floats
  #[test]
  #[allow(clippy::approx_constant)]
  fn test_float_simple() {
    assert_lit(
      "3.14",
      ExprKind::Literal(Lit::Float {
        value: 3.14,
        suffix: None,
      }),
    );
  }

  #[test]
  fn test_float_scientific() {
    assert_lit(
      "1e10",
      ExprKind::Literal(Lit::Float {
        value: 1e10,
        suffix: None,
      }),
    );
  }

  #[test]
  fn test_float_with_exponent() {
    assert_lit(
      "2.5e-3",
      ExprKind::Literal(Lit::Float {
        value: 2.5e-3,
        suffix: None,
      }),
    );
  }

  #[test]
  fn test_float_with_underscores() {
    assert_lit(
      "1_000.5",
      ExprKind::Literal(Lit::Float {
        value: 1000.5,
        suffix: None,
      }),
    );
  }

  #[test]
  fn test_hex_float_simple() {
    assert_lit(
      "0x1p+4",
      ExprKind::Literal(Lit::Float {
        value: 16.0,
        suffix: None,
      }),
    );
  }

  #[test]
  fn test_hex_float_with_fraction_and_suffix() {
    assert_lit(
      "0x1.fp1f32",
      ExprKind::Literal(Lit::Float {
        value: 3.875,
        suffix: Some("f32".to_string()),
      }),
    );
  }

  // Booleans
  #[test]
  fn test_bool_true() {
    assert_lit("true", ExprKind::Literal(Lit::Bool(true)));
  }

  #[test]
  fn test_bool_false() {
    assert_lit("false", ExprKind::Literal(Lit::Bool(false)));
  }

  // Invalid numeric forms
  #[test]
  fn test_integer_incomplete_hex_should_error() {
    assert_err("0x");
  }

  #[test]
  fn test_integer_invalid_hex_digit_should_error() {
    assert_err("0xG");
  }

  #[test]
  fn test_integer_incomplete_binary_should_error() {
    assert_err("0b");
  }

  #[test]
  fn test_integer_invalid_binary_digit_should_error() {
    assert_err("0b2");
  }

  #[test]
  fn test_integer_invalid_octal_digit_should_error() {
    assert_err("0o9");
  }

  #[test]
  fn test_float_incomplete_exponent_should_error() {
    assert_err("1e");
  }

  #[test]
  fn test_float_incomplete_exponent_plus_should_error() {
    assert_err("1e+");
  }

  #[test]
  fn test_float_incomplete_exponent_minus_should_error() {
    assert_err("1e-");
  }
}
