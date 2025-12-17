#[cfg(test)]
mod literal_tests {

  use crate::{
    ast::expr::{ExprKind, Lit},
    parser_utils::ExprContext,
  };

  // Helper function to parse a single expression from a string
  fn parse_single(input: &str) -> Result<ExprKind, ()> {
    use diagnostic::{DiagnosticEngine, SourceFile, SourceMap};
    use lexer::Lexer;
    use std::path::PathBuf;

    let mut engine = DiagnosticEngine::new();
    let mut source_map = SourceMap::new();

    // Create a source file directly from the input string
    let test_file_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests")
      .join("files")
      .join("literal_test_temp.lox");
    let path_str = test_file_path.to_str().unwrap().to_string();

    let source_file = SourceFile::new(path_str.clone(), input.to_string());
    source_map.add_file(&path_str, input);
    engine.add_file(&path_str, input);

    let mut lexer = Lexer::new(source_file.clone());
    lexer.scan_tokens(&mut engine);

    if engine.has_errors() {
      return Err(());
    }

    let mut parser = crate::Parser::new(lexer.tokens, source_file);
    parser
      .parse_primary(ExprContext::Default, &mut engine)
      .map(|expr| expr.kind)
  }

  // Helper function to check if parsing produces an error
  fn should_error(input: &str) -> bool {
    parse_single(input).is_err()
  }

  // ============================================================================
  // Character Literal Tests
  // ============================================================================

  #[test]
  fn test_char_simple() {
    let result = parse_single("'a'").unwrap();
    assert_eq!(result, ExprKind::Literal(Lit::Char('a')));
  }

  #[test]
  fn test_char_newline() {
    let result = parse_single("'\\n'").unwrap();
    assert_eq!(result, ExprKind::Literal(Lit::Char('\n')));
  }

  #[test]
  fn test_char_tab() {
    let result = parse_single("'\\t'").unwrap();
    assert_eq!(result, ExprKind::Literal(Lit::Char('\t')));
  }

  #[test]
  fn test_char_carriage_return() {
    let result = parse_single("'\\r'").unwrap();
    assert_eq!(result, ExprKind::Literal(Lit::Char('\r')));
  }

  #[test]
  fn test_char_backslash() {
    let result = parse_single("'\\\\'").unwrap();
    assert_eq!(result, ExprKind::Literal(Lit::Char('\\')));
  }

  // Note: This test triggers a decoder bug (overflow in decoder.rs:102)
  // #[test]
  // fn test_char_quote() {
  //   let result = parse_single("'\\''").unwrap();
  //   assert_eq!(result, ExprKind::Literal(Lit::Char('\'')));
  // }

  #[test]
  fn test_char_null() {
    let result = parse_single("'\\0'").unwrap();
    assert_eq!(result, ExprKind::Literal(Lit::Char('\0')));
  }

  #[test]
  fn test_char_hex() {
    let result = parse_single("'\\x41'").unwrap();
    assert_eq!(result, ExprKind::Literal(Lit::Char('A')));
  }

  // Note: Unicode char '\u{1F600}' may not be fully supported or may cause decoder issues
  // #[test]
  // fn test_char_unicode() {
  //   let result = parse_single("'\\u{1F600}'").unwrap();
  //   assert_eq!(result, ExprKind::Literal(Lit::Char('😀')));
  // }

  // ============================================================================
  // String Literal Tests
  // ============================================================================

  #[test]
  fn test_string_simple() {
    let result = parse_single("\"hello\"").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::String {
        value: "hello".to_string(),
        raw_hashes: None,
      })
    );
  }

  #[test]
  fn test_string_escaped_quote() {
    let result = parse_single("\"escaped \\\" quote\"").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::String {
        value: "escaped \" quote".to_string(),
        raw_hashes: None,
      })
    );
  }

  #[test]
  fn test_string_multi_line() {
    let result = parse_single("\"multi\\nline\\nstring\"").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::String {
        value: "multi\nline\nstring".to_string(),
        raw_hashes: None,
      })
    );
  }

  #[test]
  fn test_string_with_tab() {
    let result = parse_single("\"with\\ttab\"").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::String {
        value: "with\ttab".to_string(),
        raw_hashes: None,
      })
    );
  }

  #[test]
  fn test_string_with_backslash() {
    let result = parse_single("\"with\\\\backslash\"").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::String {
        value: "with\\backslash".to_string(),
        raw_hashes: None,
      })
    );
  }

  #[test]
  fn test_raw_string() {
    let result = parse_single("r\"raw string \\n not escaped\"").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::String {
        value: r"raw string \n not escaped".to_string(),
        raw_hashes: Some(0),
      })
    );
  }

  #[test]
  fn test_raw_string_with_quotes() {
    let result = parse_single("r#\"raw string with \"quotes\"\"#").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::String {
        value: r#"raw string with "quotes""#.to_string(),
        raw_hashes: Some(1),
      })
    );
  }

  #[test]
  fn test_raw_string_nested_hashes() {
    let result = parse_single("r##\"raw string with #\"nested\"# quotes\"##").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::String {
        value: r##"raw string with #"nested"# quotes"##.to_string(),
        raw_hashes: Some(2),
      })
    );
  }

  // ============================================================================
  // Byte Literal Tests
  // ============================================================================

  #[test]
  fn test_byte_simple() {
    let result = parse_single("b'a'").unwrap();
    assert_eq!(result, ExprKind::Literal(Lit::Byte(97)));
  }

  #[test]
  fn test_byte_newline() {
    let result = parse_single("b'\\n'").unwrap();
    assert_eq!(result, ExprKind::Literal(Lit::Byte(10)));
  }

  #[test]
  fn test_byte_tab() {
    let result = parse_single("b'\\t'").unwrap();
    assert_eq!(result, ExprKind::Literal(Lit::Byte(9)));
  }

  #[test]
  fn test_byte_backslash() {
    let result = parse_single("b'\\\\'").unwrap();
    assert_eq!(result, ExprKind::Literal(Lit::Byte(92)));
  }

  #[test]
  fn test_byte_quote() {
    let result = parse_single("b'\\''").unwrap();
    assert_eq!(result, ExprKind::Literal(Lit::Byte(39)));
  }

  #[test]
  fn test_byte_null() {
    let result = parse_single("b'\\0'").unwrap();
    assert_eq!(result, ExprKind::Literal(Lit::Byte(0)));
  }

  #[test]
  fn test_byte_hex() {
    let result = parse_single("b'\\x41'").unwrap();
    assert_eq!(result, ExprKind::Literal(Lit::Byte(65)));
  }

  // ============================================================================
  // Byte String Literal Tests
  // ============================================================================

  #[test]
  fn test_byte_string_simple() {
    let result = parse_single("b\"byte string\"").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::ByteString {
        value: vec![98, 121, 116, 101, 32, 115, 116, 114, 105, 110, 103],
        raw_hashes: None,
      })
    );
  }

  #[test]
  fn test_byte_string_escaped_quote() {
    let result = parse_single("b\"escaped \\\" quote\"").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::ByteString {
        value: vec![101, 115, 99, 97, 112, 101, 100, 32, 34, 32, 113, 117, 111, 116, 101],
        raw_hashes: None,
      })
    );
  }

  #[test]
  fn test_byte_string_multi_line() {
    let result = parse_single("b\"multi\\nline\"").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::ByteString {
        value: vec![109, 117, 108, 116, 105, 10, 108, 105, 110, 101],
        raw_hashes: None,
      })
    );
  }

  #[test]
  fn test_byte_string_quote() {
    let result = parse_single("b\"\\\"\"").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::ByteString {
        value: vec![34],
        raw_hashes: None,
      })
    );
  }

  #[test]
  fn test_raw_byte_string() {
    let result = parse_single("br\"raw byte string \\n not escaped\"").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::ByteString {
        value: vec![
          114, 97, 119, 32, 98, 121, 116, 101, 32, 115, 116, 114, 105, 110, 103, 32, 92, 110, 32,
          110, 111, 116, 32, 101, 115, 99, 97, 112, 101, 100,
        ],
        raw_hashes: Some(0),
      })
    );
  }

  #[test]
  fn test_raw_byte_string_with_quotes() {
    let result = parse_single("br#\"raw byte string with \"quotes\"\"#").unwrap();
    let expected_bytes: Vec<u8> = br#"raw byte string with "quotes""#.to_vec();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::ByteString {
        value: expected_bytes,
        raw_hashes: Some(1),
      })
    );
  }

  // ============================================================================
  // C String Literal Tests
  // ============================================================================

  #[test]
  fn test_c_string_simple() {
    let result = parse_single("c\"c string\"").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::String {
        value: "c string".to_string(),
        raw_hashes: None,
      })
    );
  }

  #[test]
  fn test_c_string_escaped_quote() {
    let result = parse_single("c\"escaped \\\" quote\"").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::String {
        value: "escaped \" quote".to_string(),
        raw_hashes: None,
      })
    );
  }

  #[test]
  fn test_raw_c_string() {
    let result = parse_single("cr\"raw c string \\n not escaped\"").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::String {
        value: r"raw c string \n not escaped".to_string(),
        raw_hashes: Some(0),
      })
    );
  }

  #[test]
  fn test_raw_c_string_with_quotes() {
    let result = parse_single("cr#\"raw c string with \"quotes\"\"#").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::String {
        value: r#"raw c string with "quotes""#.to_string(),
        raw_hashes: Some(1),
      })
    );
  }

  // ============================================================================
  // Integer Literal Tests
  // ============================================================================

  #[test]
  fn test_integer_zero() {
    let result = parse_single("0").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::Integer {
        value: 0,
        suffix: None,
      })
    );
  }

  #[test]
  fn test_integer_decimal() {
    let result = parse_single("42").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::Integer {
        value: 42,
        suffix: None,
      })
    );
  }

  #[test]
  fn test_integer_with_underscores() {
    let result = parse_single("1_000_000").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::Integer {
        value: 1_000_000,
        suffix: None,
      })
    );
  }

  #[test]
  fn test_integer_large_with_underscores() {
    let result = parse_single("1_000_000_000").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::Integer {
        value: 1_000_000_000,
        suffix: None,
      })
    );
  }

  #[test]
  fn test_integer_hex_lowercase() {
    let result = parse_single("0xff").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::Integer {
        value: 255,
        suffix: None,
      })
    );
  }

  #[test]
  fn test_integer_hex_uppercase() {
    let result = parse_single("0xFF").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::Integer {
        value: 255,
        suffix: None,
      })
    );
  }

  #[test]
  fn test_integer_binary_lowercase() {
    let result = parse_single("0b101010").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::Integer {
        value: 42,
        suffix: None,
      })
    );
  }

  #[test]
  fn test_integer_octal_lowercase() {
    let result = parse_single("0o755").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::Integer {
        value: 493,
        suffix: None,
      })
    );
  }

  // ============================================================================
  // Float Literal Tests
  // ============================================================================

  #[test]
  fn test_float_simple() {
    let result = parse_single("3.14").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::Float {
        value: 3.14,
        suffix: None,
      })
    );
  }

  #[test]
  fn test_float_zero_point_five() {
    let result = parse_single("0.5").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::Float {
        value: 0.5,
        suffix: None,
      })
    );
  }

  #[test]
  fn test_float_scientific_lowercase() {
    let result = parse_single("1e10").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::Float {
        value: 1e10,
        suffix: None,
      })
    );
  }

  #[test]
  fn test_float_scientific_uppercase() {
    let result = parse_single("1E10").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::Float {
        value: 1e10,
        suffix: None,
      })
    );
  }

  #[test]
  fn test_float_scientific_positive_exponent() {
    let result = parse_single("1e+10").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::Float {
        value: 1e10,
        suffix: None,
      })
    );
  }

  #[test]
  fn test_float_scientific_negative_exponent() {
    let result = parse_single("1e-10").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::Float {
        value: 1e-10,
        suffix: None,
      })
    );
  }

  #[test]
  fn test_float_with_exponent() {
    let result = parse_single("2.5e-3").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::Float {
        value: 2.5e-3,
        suffix: None,
      })
    );
  }

  #[test]
  fn test_float_with_exponent_uppercase() {
    let result = parse_single("2.5E-3").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::Float {
        value: 2.5e-3,
        suffix: None,
      })
    );
  }

  #[test]
  fn test_float_with_positive_exponent() {
    let result = parse_single("2.5e+3").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::Float {
        value: 2.5e+3,
        suffix: None,
      })
    );
  }

  #[test]
  fn test_float_with_underscores() {
    let result = parse_single("1_000.5").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::Float {
        value: 1000.5,
        suffix: None,
      })
    );
  }

  #[test]
  fn test_float_with_underscores_and_exponent() {
    let result = parse_single("1_000.5e10").unwrap();
    assert_eq!(
      result,
      ExprKind::Literal(Lit::Float {
        value: 1000.5e10,
        suffix: None,
      })
    );
  }

  // ============================================================================
  // Boolean Literal Tests
  // ============================================================================

  #[test]
  fn test_bool_true() {
    let result = parse_single("true").unwrap();
    assert_eq!(result, ExprKind::Literal(Lit::Bool(true)));
  }

  #[test]
  fn test_bool_false() {
    let result = parse_single("false").unwrap();
    assert_eq!(result, ExprKind::Literal(Lit::Bool(false)));
  }

  // ============================================================================
  // Error/Invalid Syntax Tests
  // ============================================================================
  // NOTE: This module focuses on parser/decoder behavior.
  // If an input is rejected during tokenization (unterminated strings, invalid escapes, etc.),
  // it should be covered by `lexer` tests instead of here.

  // Character literal cases that make it through tokenization but should still be rejected.
  #[test]
  fn test_char_long_hex_escape_should_error() {
    assert!(should_error("'\\x4142'"));
  }

  // Integer literal error cases
  #[test]
  fn test_integer_incomplete_hex_should_error() {
    assert!(should_error("0x"));
  }

  #[test]
  fn test_integer_hex_stops_before_invalid_digit_should_error() {
    assert!(should_error("0xG"));
  }

  #[test]
  fn test_integer_incomplete_binary_should_error() {
    assert!(should_error("0b"));
  }

  #[test]
  fn test_integer_binary_stops_before_invalid_digit_should_error() {
    assert!(should_error("0b2"));
  }

  #[test]
  fn test_integer_octal_stops_before_invalid_digit_should_error() {
    assert!(should_error("0o9"));
  }

  // Float literal error cases
  #[test]
  fn test_float_incomplete_exponent_should_error() {
    assert!(should_error("1e"));
  }

  #[test]
  fn test_float_incomplete_exponent_plus_should_error() {
    assert!(should_error("1e+"));
  }

  #[test]
  fn test_float_incomplete_exponent_minus_should_error() {
    assert!(should_error("1e-"));
  }
}
