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

  #[test]
  fn test_char_unicode_escape() {
    assert_lit("'\\u{41}'", ExprKind::Literal(Lit::Char('A')));
  }

  #[test]
  fn test_char_unicode_escape_emoji() {
    assert_lit("'\\u{1F600}'", ExprKind::Literal(Lit::Char('😀')));
  }

  #[test]
  fn test_char_unicode_escape_zero() {
    assert_lit("'\\u{0}'", ExprKind::Literal(Lit::Char('\0')));
  }

  #[test]
  fn test_char_unicode_escape_max() {
    assert_lit("'\\u{10FFFF}'", ExprKind::Literal(Lit::Char('\u{10FFFF}')));
  }

  #[test]
  fn test_char_unicode_escape_invalid_missing_brace() {
    assert_err("'\\u41'");
  }

  #[test]
  fn test_char_unicode_escape_invalid_no_brace() {
    assert_err("'\\u'");
  }

  #[test]
  fn test_char_unicode_escape_invalid_empty() {
    assert_err("'\\u{}'");
  }

  #[test]
  fn test_char_unicode_escape_invalid_too_many_digits() {
    assert_err("'\\u{1234567}'");
  }

  #[test]
  fn test_char_unicode_escape_invalid_surrogate() {
    assert_err("'\\u{D800}'");
  }

  #[test]
  fn test_char_unicode_escape_invalid_surrogate_end() {
    assert_err("'\\u{DFFF}'");
  }

  #[test]
  fn test_char_unicode_escape_7_digits_should_error() {
    assert_err("'\\u{1000000}'");
  }

  #[test]
  fn test_char_unicode_escape_non_hex_digit_middle() {
    assert_err("'\\u{4G1}'");
  }

  #[test]
  fn test_char_unicode_escape_non_hex_digit_at_end() {
    assert_err("'\\u{41G}'");
  }

  #[test]
  fn test_char_unicode_escape_non_hex_digit_at_start() {
    assert_err("'\\u{G41}'");
  }

  #[test]
  fn test_string_unicode_escape_non_hex_digit() {
    assert_err("\"\\u{41G}\"");
  }

  #[test]
  fn test_string_unicode_escape_non_hex_digit_middle() {
    assert_err("\"\\u{4G1}\"");
  }

  #[test]
  fn test_char_unicode_escape_exactly_zero_digits() {
    assert_err("'\\u{}'");
  }

  #[test]
  fn test_char_unicode_escape_value_one_over_max() {
    assert_err("'\\u{110000}'");
  }

  #[test]
  fn test_string_unicode_escape_non_hex_before_close() {
    assert_err("\"\\u{41G}\"");
  }

  #[test]
  fn test_char_unicode_escape_missing_opening_brace() {
    // Tests decode_unicode_escape error path when '{' is missing
    assert_err("'\\u41'");
  }

  #[test]
  fn test_string_unicode_escape_missing_opening_brace() {
    assert_err("\"\\u41\"");
  }

  #[test]
  fn test_char_unicode_escape_missing_closing_brace() {
    // Tests decode_unicode_escape error path when '}' is missing
    assert_err("'\\u{41'");
  }

  #[test]
  fn test_string_unicode_escape_missing_closing_brace() {
    assert_err("\"\\u{41\"");
  }


  #[test]
  fn test_char_unicode_escape_surrogate_middle() {
    assert_err("'\\u{DC00}'");
  }

  #[test]
  fn test_char_unicode_escape_missing_opening_brace_other_char() {
    // Tests decode_unicode_escape when next char is not '{'
    assert_err("'\\uA'");
  }

  #[test]
  fn test_char_unicode_escape_missing_opening_brace_eof() {
    // Tests decode_unicode_escape when next char is None
    assert_err("'\\u");
  }

  #[test]
  fn test_char_unicode_escape_missing_closing_brace_other_char() {
    // Tests decode_unicode_escape when closing brace is wrong char
    assert_err("'\\u{41A'");
  }

  #[test]
  fn test_char_unicode_escape_missing_closing_brace_eof() {
    // Tests decode_unicode_escape when closing brace is missing (EOF)
    assert_err("'\\u{41");
  }

  #[test]
  fn test_char_unicode_escape_invalid_no_closing_brace() {
    assert_err("'\\u{41'");
  }

  #[test]
  fn test_char_invalid_escape() {
    assert_err("'\\z'");
  }

  #[test]
  fn test_char_invalid_escape_unterminated() {
    assert_err("'\\'");
  }

  #[test]
  fn test_char_hex_escape_invalid_one_digit() {
    assert_err("'\\x4'");
  }

  #[test]
  fn test_char_hex_escape_invalid_no_digits() {
    assert_err("'\\x'");
  }

  #[test]
  fn test_char_hex_escape_invalid_non_hex() {
    assert_err("'\\xGG'");
  }

  #[test]
  fn test_char_hex_escape_first_none() {
    // Tests decode_hex_escape when first char is None (EOF after \x)
    assert_err("'\\x");
  }

  #[test]
  fn test_char_hex_escape_second_none() {
    // Tests decode_hex_escape when second char is None (only one char after \x)
    assert_err("'\\xA");
  }

  #[test]
  fn test_char_hex_escape_first_not_hex() {
    assert_err("'\\xG1'");
  }

  #[test]
  fn test_char_hex_escape_second_not_hex() {
    assert_err("'\\x1G'");
  }

  #[test]
  fn test_char_hex_escape_both_not_hex() {
    assert_err("'\\xGG'");
  }

  #[test]
  fn test_char_hex_escape_no_second_digit() {
    assert_err("'\\xA'");
  }

  #[test]
  fn test_char_hex_escape_no_digits_at_all() {
    assert_err("'\\x'");
  }

  #[test]
  fn test_byte_hex_escape_no_digits() {
    assert_err("b'\\x'");
  }

  #[test]
  fn test_byte_hex_escape_one_digit() {
    assert_err("b'\\xA'");
  }

  #[test]
  fn test_byte_string_hex_escape_no_digits() {
    assert_err("b\"\\x\"");
  }

  #[test]
  fn test_byte_string_hex_escape_one_digit() {
    assert_err("b\"\\xA\"");
  }

  #[test]
  fn test_byte_string_hex_escape_first_none() {
    assert_err("b\"\\x\"");
  }

  #[test]
  fn test_byte_string_hex_escape_second_none() {
    assert_err("b\"\\xA");
  }

  #[test]
  fn test_byte_string_hex_escape_first_not_hex() {
    assert_err("b\"\\xG1\"");
  }

  #[test]
  fn test_byte_string_hex_escape_second_not_hex() {
    assert_err("b\"\\x1G\"");
  }

  #[test]
  fn test_byte_hex_escape_first_none() {
    assert_err("b'\\x'");
  }

  #[test]
  fn test_byte_hex_escape_second_none() {
    assert_err("b'\\xA");
  }

  #[test]
  fn test_byte_hex_escape_first_not_hex() {
    assert_err("b'\\xG1'");
  }

  #[test]
  fn test_byte_hex_escape_second_not_hex() {
    assert_err("b'\\x1G'");
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

  #[test]
  fn test_string_unicode_escape() {
    assert_lit(
      "\"\\u{41}\"",
      ExprKind::Literal(Lit::String {
        value: "A".to_string(),
        raw_hashes: None,
      }),
    );
  }

  #[test]
  fn test_string_unicode_escape_emoji() {
    assert_lit(
      "\"\\u{1F600}\"",
      ExprKind::Literal(Lit::String {
        value: "😀".to_string(),
        raw_hashes: None,
      }),
    );
  }

  #[test]
  fn test_string_unicode_escape_multiple() {
    assert_lit(
      "\"\\u{48}\\u{65}\\u{6C}\\u{6C}\\u{6F}\"",
      ExprKind::Literal(Lit::String {
        value: "Hello".to_string(),
        raw_hashes: None,
      }),
    );
  }

  #[test]
  fn test_string_unicode_escape_invalid() {
    assert_err("\"\\u{}\"");
  }

  #[test]
  fn test_string_unicode_escape_invalid_missing_brace() {
    assert_err("\"\\u41\"");
  }

  #[test]
  fn test_string_invalid_escape() {
    assert_err("\"\\z\"");
  }

  #[test]
  fn test_string_hex_escape_invalid() {
    assert_err("\"\\x4\"");
  }

  #[test]
  fn test_string_hex_escape_no_digits() {
    assert_err("\"\\x\"");
  }

  #[test]
  fn test_string_hex_escape_one_digit_only() {
    assert_err("\"\\xA\"");
  }

  #[test]
  fn test_string_hex_escape_invalid_hex() {
    assert_err("\"\\xGG\"");
  }

  #[test]
  fn test_string_hex_escape_first_invalid() {
    assert_err("\"\\xG1\"");
  }

  #[test]
  fn test_string_hex_escape_second_invalid() {
    assert_err("\"\\x1G\"");
  }

  #[test]
  fn test_char_hex_escape_missing_first_digit() {
    assert_err("'\\x'");
  }

  #[test]
  fn test_char_hex_escape_missing_second_digit() {
    assert_err("'\\xA'");
  }

  #[test]
  fn test_char_hex_escape_both_missing() {
    assert_err("'\\x'");
  }

  #[test]
  fn test_byte_hex_escape_missing_first() {
    assert_err("b'\\x'");
  }

  #[test]
  fn test_byte_hex_escape_missing_second() {
    assert_err("b'\\xA'");
  }

  #[test]
  fn test_byte_string_hex_escape_missing_first() {
    assert_err("b\"\\x\"");
  }

  #[test]
  fn test_byte_string_hex_escape_missing_second() {
    assert_err("b\"\\xA\"");
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

  #[test]
  fn test_byte_unicode_escape_invalid() {
    assert_err("b'\\u{41}'");
  }

  #[test]
  fn test_byte_invalid_escape() {
    assert_err("b'\\z'");
  }

  #[test]
  fn test_byte_hex_escape_invalid() {
    assert_err("b'\\x4'");
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
  fn test_byte_string_with_quote_at_start() {
    // Tests decode_byte_string removing quote at start
    assert_lit(
      "b\"\\\"test\"",
      ExprKind::Literal(Lit::ByteString {
        value: b"\"test".to_vec(),
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

  #[test]
  fn test_byte_string_unicode_escape_invalid() {
    assert_err("b\"\\u{41}\"");
  }

  #[test]
  fn test_byte_string_invalid_escape() {
    assert_err("b\"\\z\"");
  }

  #[test]
  fn test_byte_string_hex_escape_invalid() {
    assert_err("b\"\\x4\"");
  }

  #[test]
  fn test_byte_string_non_byte_value() {
    assert_err("b\"\\u{100}\"");
  }

  #[test]
  fn test_byte_string_unicode_escape_disallowed() {
    // Tests decode_escape with allow_unicode=false
    assert_err("b\"\\u{41}\"");
  }

  #[test]
  fn test_byte_non_byte_value() {
    assert_err("b'\\u{100}'");
  }

  #[test]
  fn test_byte_string_invalid_escape_unterminated() {
    assert_err("b\"\\\"");
  }

  #[test]
  fn test_string_invalid_escape_unterminated() {
    assert_err("\"\\\"");
  }

  #[test]
  fn test_char_invalid_escape_unterminated_at_end() {
    assert_err("'\\");
  }

  #[test]
  fn test_string_invalid_escape_unterminated_at_end() {
    // Tests decode_escape None case for strings
    assert_err("\"\\");
  }

  #[test]
  fn test_byte_string_invalid_escape_unterminated_at_end() {
    // Tests decode_escape None case for byte strings
    assert_err("b\"\\");
  }

  #[test]
  fn test_byte_invalid_escape_unterminated_at_end() {
    // Tests decode_escape None case for bytes
    assert_err("b'\\");
  }

  #[test]
  fn test_string_invalid_escape_various_chars() {
    // Tests decode_escape Some(other) case for various invalid escapes
    assert_err("\"\\a\"");
    assert_err("\"\\b\"");
    assert_err("\"\\c\"");
    assert_err("\"\\d\"");
    assert_err("\"\\e\"");
    assert_err("\"\\f\"");
    assert_err("\"\\g\"");
    assert_err("\"\\h\"");
    assert_err("\"\\i\"");
    assert_err("\"\\j\"");
    assert_err("\"\\k\"");
    assert_err("\"\\l\"");
    assert_err("\"\\m\"");
    assert_err("\"\\o\"");
    assert_err("\"\\p\"");
    assert_err("\"\\q\"");
    assert_err("\"\\s\"");
    assert_err("\"\\v\"");
    assert_err("\"\\w\"");
    assert_err("\"\\y\"");
    assert_err("\"\\z\"");
  }

  #[test]
  fn test_char_invalid_escape_various_chars() {
    assert_err("'\\a'");
    assert_err("'\\b'");
    assert_err("'\\c'");
    assert_err("'\\d'");
    assert_err("'\\e'");
    assert_err("'\\f'");
    assert_err("'\\g'");
    assert_err("'\\h'");
    assert_err("'\\i'");
    assert_err("'\\j'");
    assert_err("'\\k'");
    assert_err("'\\l'");
    assert_err("'\\m'");
    assert_err("'\\o'");
    assert_err("'\\p'");
    assert_err("'\\q'");
    assert_err("'\\s'");
    assert_err("'\\v'");
    assert_err("'\\w'");
    assert_err("'\\y'");
    assert_err("'\\z'");
  }

  #[test]
  fn test_byte_string_invalid_escape_various_chars() {
    assert_err("b\"\\a\"");
    assert_err("b\"\\b\"");
    assert_err("b\"\\c\"");
    assert_err("b\"\\d\"");
    assert_err("b\"\\e\"");
    assert_err("b\"\\f\"");
    assert_err("b\"\\g\"");
    assert_err("b\"\\h\"");
    assert_err("b\"\\i\"");
    assert_err("b\"\\j\"");
    assert_err("b\"\\k\"");
    assert_err("b\"\\l\"");
    assert_err("b\"\\m\"");
    assert_err("b\"\\o\"");
    assert_err("b\"\\p\"");
    assert_err("b\"\\q\"");
    assert_err("b\"\\s\"");
    assert_err("b\"\\v\"");
    assert_err("b\"\\w\"");
    assert_err("b\"\\y\"");
    assert_err("b\"\\z\"");
  }

  #[test]
  fn test_byte_invalid_escape_various_chars() {
    assert_err("b'\\a'");
    assert_err("b'\\b'");
    assert_err("b'\\c'");
    assert_err("b'\\d'");
    assert_err("b'\\e'");
    assert_err("b'\\f'");
    assert_err("b'\\g'");
    assert_err("b'\\h'");
    assert_err("b'\\i'");
    assert_err("b'\\j'");
    assert_err("b'\\k'");
    assert_err("b'\\l'");
    assert_err("b'\\m'");
    assert_err("b'\\o'");
    assert_err("b'\\p'");
    assert_err("b'\\q'");
    assert_err("b'\\s'");
    assert_err("b'\\v'");
    assert_err("b'\\w'");
    assert_err("b'\\y'");
    assert_err("b'\\z'");
  }

  #[test]
  fn test_char_empty_should_error() {
    assert_err("''");
  }

  #[test]
  fn test_char_multiple_chars_should_error() {
    assert_err("'ab'");
  }

  #[test]
  fn test_char_three_chars_should_error() {
    assert_err("'abc'");
  }

  #[test]
  fn test_char_zero_chars_should_error() {
    // Tests decode_char when out.len() == 0 after processing
    assert_err("''");
  }

  #[test]
  fn test_char_four_chars_should_error() {
    // Tests decode_char when out.len() > 1 (specifically 4)
    assert_err("'abcd'");
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
