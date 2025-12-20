#[cfg(test)]
mod string_tests {
  use crate::{
    token::{LiteralKind, TokenKind},
    Lexer,
  };

  fn lex_kinds(input: &str) -> Result<Vec<TokenKind>, ()> {
    use diagnostic::{DiagnosticEngine, SourceFile, SourceMap};
    use std::{cell::RefCell, path::PathBuf, rc::Rc};

    let engine = Rc::new(RefCell::new(DiagnosticEngine::new()));
    let mut _source_map = SourceMap::new();

    let test_file_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests")
      .join("files")
      .join("literal_test_temp.lox");
    let path_str = test_file_path.to_str().unwrap().to_string();

    let source_file = SourceFile::new(path_str.clone(), input.to_string());
    _source_map.add_file(&path_str, input);
    engine.borrow_mut().add_file(&path_str, input);

    let mut lexer = Lexer::new(source_file, engine.clone());
    let _ = lexer.scan_tokens();

    if engine.borrow().has_errors() {
      return Err(());
    }

    Ok(lexer.tokens.iter().map(|t| t.kind).collect())
  }

  fn lex_single_literal(input: &str) -> Result<TokenKind, ()> {
    let kinds = lex_kinds(input)?;

    // If you have TokenKind::Eof, strip it so we can assert "exactly one literal".
    let non_eof: Vec<TokenKind> = kinds
      .into_iter()
      .filter(|k| !matches!(k, TokenKind::Eof))
      .collect();

    if non_eof.len() != 1 {
      return Err(());
    }
    match non_eof[0] {
      TokenKind::Literal { .. } => Ok(non_eof[0]),
      _ => Err(()),
    }
  }

  fn lex_literal_with_optional_suffix(input: &str) -> Result<TokenKind, ()> {
    let kinds = lex_kinds(input)?;
    let mut iter = kinds.into_iter().filter(|k| !matches!(k, TokenKind::Eof));
    match iter.next() {
      Some(lit @ TokenKind::Literal { .. }) => Ok(lit),
      _ => Err(()),
    }
  }

  fn assert_lit(input: &str, expected: LiteralKind) {
    let tok = lex_single_literal(input).unwrap();
    assert_eq!(tok, TokenKind::Literal { kind: expected });
  }

  fn assert_err(input: &str) {
    assert!(
      lex_single_literal(input).is_err(),
      "expected error for: {:?}",
      input
    );
  }

  // 1) Normal string literals: "..."
  #[test]
  fn normal_string_ok() {
    let cases = [
      "\"\"",
      "\"hello\"",
      "\"utf8: é α β\"",
      "\"a\\n\\r\\t\\\\\\\"\\'\\0\"",
      "\"\\x00\\x41\\x7F\"",
      "\"\\u{0}\\u{41}\\u{1F980}\"",
      "\"line1\nline2\"",
      "\"hello\\\n   world\"",
      "\"hello\\\nworld\"",
    ];

    for src in cases {
      assert_lit(src, LiteralKind::Str);
    }
  }

  #[test]
  fn normal_string_errors() {
    // Unterminated
    assert_err("\"hello");

    // Invalid escapes
    assert_err("\"\\q\"");
    assert_err("\"\\x\"");
    assert_err("\"\\x0\"");
    assert_err("\"\\xGG\"");
    assert_err("\"\\u\"");
    assert_err("\"\\u{}\"");
    assert_err("\"\\u{_1}\"");
    assert_err("\"\\u{1234567}\"");

    // \xNN must be <= 0x7F in normal strings (7-bit)
    assert_err("\"\\x80\"");
    assert_err("\"\\xFF\"");

    // Invalid Unicode scalar values
    assert_err("\"\\u{D800}\"");
    assert_err("\"\\u{DFFF}\"");
    assert_err("\"\\u{110000}\"");

    // Bare CR inside a normal string is forbidden
    let cr = '\r';
    assert_err(&format!("\"a{}b\"", cr));

    // Backslash at end (incomplete escape)
    assert_err("\"abc\\\"");
  }

  // 2) Raw string literals: r"...", r#"..."#, ...
  #[test]
  fn raw_string_ok() {
    assert_lit("r\"hello\"", LiteralKind::RawStr { n_hashes: 0 });
    assert_lit("r#\"hello\"#", LiteralKind::RawStr { n_hashes: 1 });
    assert_lit("r##\"hello\"##", LiteralKind::RawStr { n_hashes: 2 });

    // Backslashes are literal in raw strings
    assert_lit("r\"C:\\foo\\bar\"", LiteralKind::RawStr { n_hashes: 0 });

    // Quotes can be included by using hashes
    assert_lit("r#\"\"foo\"\"#", LiteralKind::RawStr { n_hashes: 1 });

    // Newlines allowed
    assert_lit("r\"hello\nworld\"", LiteralKind::RawStr { n_hashes: 0 });
  }

  #[test]
  fn raw_string_errors() {
    // Unterminated (missing closing delimiter)
    assert_err("r\"hello");
    assert_err("r#\"hello\""); // missing trailing #
                               // assert_err("r##\"hello\"#"); // missing one #

    // Must have opening quote immediately after hashes
    assert_err("r#hello\"#");

    // Bare CR forbidden even in raw strings (Rust Reference)
    let cr = '\r';
    assert_err(&format!("r\"a{}b\"", cr));
  }

  #[test]
  fn raw_string_hash_limit() {
    // Rust requires fewer than 256 # (so 255 ok, 256 error).
    let h255 = "#".repeat(255);
    let h256 = "#".repeat(256);

    let ok = format!("r{h}\"hi\"{h}", h = h255);
    let bad = format!("r{h}\"hi\"{h}", h = h256);

    assert_lit(&ok, LiteralKind::RawStr { n_hashes: 255 });
    assert_err(&bad);
  }

  // 3) Byte strings: b"..."
  #[test]
  fn byte_string_ok() {
    let cases = [
      "b\"\"",
      "b\"abc\"",
      "b\"\\n\\r\\t\\\\\\\"\\0\"",
      "b\"\\x00\\x41\\x7F\\xFF\"",
      "b\"line1\nline2\"",
      "b\"hello\\\n   world\"",
    ];
    for src in cases {
      assert_lit(src, LiteralKind::ByteStr);
    }
  }

  #[test]
  fn byte_string_errors() {
    // Unterminated
    assert_err("b\"hello");

    // Non-ASCII directly in byte strings is forbidden
    assert_err("b\"é\"");
    assert_err("b\"α\"");

    // Unicode escapes are forbidden in byte strings
    assert_err("b\"\\u{41}\"");

    // Invalid escape
    assert_err("b\"\\q\"");

    // Bare CR forbidden
    let cr = '\r';
    assert_err(&format!("b\"a{}b\"", cr));
  }

  // 4) Raw byte strings: br"...", br#"..."#, ...
  #[test]
  fn raw_byte_string_ok() {
    // NOTE: rename LiteralKind::RawByteStr if your enum uses a different name.
    assert_lit("br\"abc\"", LiteralKind::RawByteStr { n_hashes: 0 });
    assert_lit("br#\"abc\"#", LiteralKind::RawByteStr { n_hashes: 1 });

    // Escapes are not processed (backslash stays literal)
    assert_lit(
      "br\"\\x41\\u{41}\\n\"",
      LiteralKind::RawByteStr { n_hashes: 0 },
    );

    // Newlines allowed
    assert_lit(
      "br\"hello\nworld\"",
      LiteralKind::RawByteStr { n_hashes: 0 },
    );
  }

  #[test]
  fn raw_byte_string_errors() {
    // Unterminated / mismatched hashes
    assert_err("br\"abc");
    assert_err("br#\"abc\"");
    assert_err("br##\"abc\"#");

    // Non-ASCII forbidden in raw byte strings too
    assert_err("br\"é\"");

    //Bare CR forbidden
    let cr = '\r';
    assert_err(&format!("br\"a{}b\"", cr));
  }

  #[test]
  fn raw_byte_string_hash_limit() {
    let h255 = "#".repeat(255);
    let h256 = "#".repeat(256);

    let ok = format!("br{h}\"hi\"{h}", h = h255);
    let bad = format!("br{h}\"hi\"{h}", h = h256);

    assert_lit(&ok, LiteralKind::RawByteStr { n_hashes: 255 });
    assert_err(&bad);
  }

  // 5) C strings: c"..."
  #[test]
  fn c_string_ok() {
    // Normal C string
    assert_lit("c\"hello\"", LiteralKind::CStr);

    // Byte escapes in C strings can be any 00..FF
    assert_lit("c\"\\xFF\"", LiteralKind::CStr);

    // Unicode escapes allowed
    assert_lit("c\"\\u{00E6}\\u{1F980}\"", LiteralKind::CStr);

    // Newlines allowed
    assert_lit("c\"hello\nworld\"", LiteralKind::CStr);

    // Continuation allowed
    assert_lit("c\"hello\\\n   world\"", LiteralKind::CStr);
  }

  #[test]
  fn c_string_errors() {
    // Unterminated
    assert_err("c\"hello");

    // Interior NUL forbidden, including escapes that produce 0
    assert_err("c\"\\0\"");
    assert_err("c\"\\x00\"");
    assert_err("c\"\\u{0}\"");
    assert_err("c\"\\u{000000}\"");

    // Direct NUL character also forbidden
    let nul = '\0';
    assert_err(&format!("c\"a{}b\"", nul));

    // Bare CR forbidden
    let cr = '\r';
    assert_err(&format!("c\"a{}b\"", cr));
  }

  // 6) Raw C strings: cr"...", cr#"..."#, ...
  #[test]
  fn raw_c_string_ok() {
    // NOTE: rename LiteralKind::RawCStr if your enum uses a different name.
    assert_lit("cr\"hello\"", LiteralKind::RawCStr { n_hashes: 0 });
    assert_lit("cr#\"hello\"#", LiteralKind::RawCStr { n_hashes: 1 });

    // No escapes processed
    assert_lit("cr\"C:\\foo\\bar\"", LiteralKind::RawCStr { n_hashes: 0 });

    // Quotes via hashes
    assert_lit("cr#\"\"foo\"\"#", LiteralKind::RawCStr { n_hashes: 1 });

    // Newlines allowed
    assert_lit("cr\"hello\nworld\"", LiteralKind::RawCStr { n_hashes: 0 });
  }

  #[test]
  fn raw_c_string_errors() {
    // Unterminated / mismatched hashes
    assert_err("cr\"hello");
    assert_err("cr#\"hello\"");
    assert_err("cr##\"hello\"#");

    // NUL forbidden even in raw form
    let nul = '\0';
    assert_err(&format!("cr\"a{}b\"", nul));

    // Bare CR forbidden
    let cr = '\r';
    assert_err(&format!("cr\"a{}b\"", cr));
  }

  #[test]
  fn raw_c_string_hash_limit() {
    let h255 = "#".repeat(255);
    let h256 = "#".repeat(256);

    let ok = format!("cr{h}\"hi\"{h}", h = h255);
    let bad = format!("cr{h}\"hi\"{h}", h = h256);

    assert_lit(&ok, LiteralKind::RawCStr { n_hashes: 255 });
    assert_err(&bad);
  }

  // 7) Char literals: 'x', escapes, unicode
  #[test]
  fn char_ok() {
    let cases = [
      "'a'",
      "'é'",
      "'\\n'",
      "'\\r'",
      "'\\t'",
      "'\\\\'",
      "'\\0'",
      "'\\''",
      "'\\x00'",
      "'\\x41'",
      "'\\x7F'",
      "'\\u{41}'",
      "'\\u{1F980}'",
    ];
    for src in cases {
      assert_lit(src, LiteralKind::Char);
    }
  }

  #[test]
  fn char_errors() {
    // Unterminated
    assert_err("'a");

    // Empty
    assert_err("''");

    // Too long
    assert_err("'ab'");

    // Newline inside char literal
    assert_err("'\n'");

    // Invalid escape
    assert_err("'\\q'");

    // \xNN must be <= 0x7F for char
    assert_err("'\\x80'");
    assert_err("'\\xFF'");

    // Invalid unicode scalar
    assert_err("'\\u{D800}'");
    assert_err("'\\u{110000}'");
  }

  // 8) Byte char literals: b'X', b'\xNN', etc
  #[test]
  fn byte_char_ok() {
    // NOTE: rename LiteralKind::ByteChar if your enum uses a different name.
    let cases = [
      "b'a'", "b'Z'", "b'\\n'", "b'\\r'", "b'\\t'", "b'\\\\'", "b'\\0'", "b'\\''", "b'\\x00'",
      "b'\\x7F'", "b'\\xFF'",
    ];
    for src in cases {
      assert_lit(src, LiteralKind::Byte);
    }
  }

  #[test]
  fn byte_char_errors() {
    // Unterminated
    assert_err("b'a");

    // Empty / too long
    assert_err("b''");
    assert_err("b'ab'");

    // Non-ASCII directly
    assert_err("b'é'");

    // Unicode escapes forbidden
    assert_err("b'\\u{41}'");

    // Invalid escape
    assert_err("b'\\q'");
  }

  // 9) Literal suffixes (lexer-level)
  #[test]
  fn literal_suffix_tokenization() {
    // Rust's lexer allows suffixes on ANY literal token. Parsing may reject them later.
    // Keep these tests only if your language keeps Rust's lexer behavior.
    assert!(lex_literal_with_optional_suffix("\"foo\"bar").is_ok());
    assert!(lex_literal_with_optional_suffix("b\"foo\"tag").is_ok());
    assert!(lex_literal_with_optional_suffix("r#\"foo\"#suffix").is_ok());
  }

  // 10) Prefix recognition / reserved prefixes (language policy)
  #[test]
  fn unknown_or_reserved_prefix_policy() {
    // In upstream Rust, these are reserved/edition-sensitive cases.
    // Keep these tests only if your lexer chooses to DIAGNOSTIC on them (as your doc says).
    let cases = [
      "f\"x\"", "cf\"x\"", "rf\"x\"", "z\"x\"", "bb\"x\"", "rc\"x\"",
    ];
    for src in cases {
      assert_err(src);
    }
  }
}
