#[cfg(test)]
mod number_tests {
  use crate::{
    token::{Base, LiteralKind, TokenKind},
    Lexer,
  };

  fn lex_kinds(input: &str) -> Result<Vec<TokenKind>, ()> {
    use diagnostic::{DiagnosticEngine, SourceFile, SourceMap};
    use std::path::PathBuf;

    let mut engine = DiagnosticEngine::new();
    let mut _source_map = SourceMap::new();

    let test_file_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests")
      .join("files")
      .join("literal_test_temp.lox");
    let path_str = test_file_path.to_str().unwrap().to_string();

    let source_file = SourceFile::new(path_str.clone(), input.to_string());
    _source_map.add_file(&path_str, input);
    engine.add_file(&path_str, input);

    let mut lexer = Lexer::new(source_file);
    let _ = lexer.scan_tokens(&mut engine);

    if engine.has_errors() {
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

  // Decimal integers
  #[test]
  fn decimal_integer_ok() {
    let cases = [
      "0", "1", "42", "999999", "1_000", "12_34_56", "123usize", "10u8", "255u16", "100i32",
      "0isize",
    ];

    for src in cases {
      let suffix_start = src
        .find(|c: char| !c.is_ascii_digit() && c != '_')
        .unwrap_or_else(|| src.len());
      assert_lit(
        src,
        LiteralKind::Integer {
          base: Base::Decimal,
          empty_int: false,
          suffix_start,
        },
      );
    }
  }

  #[test]
  fn decimal_integer_errors() {
    let cases = ["_1", "1_", "__1", "1__2"];

    for src in cases {
      assert_err(src);
    }
  }

  // Binary integers
  #[test]
  fn binary_integer_ok() {
    let cases = ["0b0", "0b1", "0b1010", "0b1010_0101", "0b1111u8", "0b0i32"];

    for src in cases {
      assert!(lex_single_literal(src).is_ok());
    }
  }

  #[test]
  fn binary_integer_errors() {
    let cases = ["0b", "0b_", "0b__", "0b102", "0b2", "0b1__0", "_0b101"];

    for src in cases {
      assert_err(src);
    }
  }

  // Octal integers
  #[test]
  fn octal_integer_ok() {
    let cases = ["0o0", "0o7", "0o755", "0o7_5_5", "0o10u16"];

    for src in cases {
      assert!(lex_single_literal(src).is_ok());
    }
  }

  #[test]
  fn octal_integer_errors() {
    let cases = ["0o", "0o8", "0o9", "0o7__5", "0o_755"];

    for src in cases {
      assert_err(src);
    }
  }

  // Hexadecimal integers
  #[test]
  fn hexadecimal_integer_ok() {
    let cases = [
      "0x0",
      "0xF",
      "0xff",
      "0xFF",
      "0xdeadBEEF",
      "0xDEAD_BEEF",
      "0x10u32",
      "0xFFi64",
    ];

    for src in cases {
      assert!(lex_single_literal(src).is_ok());
    }
  }

  #[test]
  fn hexadecimal_integer_errors() {
    let cases = ["0x", "0x_", "0xG", "0xZ", "0x1__F", "_0xFF"];

    for src in cases {
      assert_err(src);
    }
  }

  // Decimal floats
  #[test]
  fn decimal_float_ok() {
    let cases = [
      "0.0", "1.0", "3.14", "1_000.5", "1._5", "1.0_0", "1e10", "1E10", "1e+10", "1e-10", "2.5e-3",
      "2.5E+3", "1e", "1e+", "1e-", "1.0f32", "2.5f64",
    ];

    for src in cases {
      assert!(lex_single_literal(src).is_ok());
    }
  }

  #[test]
  fn decimal_float_errors() {
    let cases = ["_1.0", "1._", "1.__0", "1e__10"];

    for src in cases {
      assert_err(src);
    }
  }

  // Hexadecimal floats
  #[test]
  fn hexadecimal_float_ok() {
    let cases = [
      "0x1.0p0",
      "0x1p1",
      "0x1p+1",
      "0x1p-1",
      "0xA.BCp10",
      "0xA.BC_p10",
      "0x1.2p3f64",
    ];

    for src in cases {
      assert!(lex_single_literal(src).is_ok());
    }
  }

  #[test]
  fn hexadecimal_float_errors() {
    let cases = ["0x1.p", "0x1p", "0x1p+", "0x.p1", "0x1__p2"];

    for src in cases {
      assert_err(src);
    }
  }

  // Suffix handling
  #[test]
  fn numeric_suffix_ok() {
    let cases = [
      "1u8", "1u16", "1u32", "1u64", "1u128", "1usize", "1i8", "1i16", "1i32", "1i64", "1i128",
      "1isize", "1.0f32", "1.0f64",
    ];

    for src in cases {
      assert!(lex_literal_with_optional_suffix(src).is_ok());
    }
  }

  #[test]
  fn numeric_suffix_errors() {
    let cases = ["1u", "1i", "1u3", "1i3", "1f", "1f128", "1.0u8", "1.0i32"];

    for src in cases {
      assert_err(src);
    }
  }
}
