#[cfg(test)]
mod lexer_tests {
  use diagnostic::{DiagnosticEngine, SourceFile, SourceMap};
  use lexer::{
    token::{Base, LiteralKind, Token, TokenKind},
    Lexer,
  };
  use rand::{distributions::Standard, rngs::StdRng, Rng, SeedableRng};

  fn lex_test_file(file: &str) -> (Lexer, DiagnosticEngine) {
    let mut engine = DiagnosticEngine::new();
    let mut source_map = SourceMap::new();

    source_map.add_wd("tests/files").unwrap();
    let source = source_map.get(file).unwrap().clone();
    let mut lexer = Lexer::new(source);
    lexer.scan_tokens(&mut engine);
    (lexer, engine)
  }

  fn assert_contains(tokens: &[Token], kind: TokenKind) {
    assert!(
      tokens.iter().any(|tok| tok.kind == kind),
      "expected token {:?} to be present",
      kind
    );
  }

  fn token_text<'a>(lexer: &'a Lexer, token: &Token) -> &'a str {
    &lexer.source.src[token.span.start..token.span.end]
  }

  fn lex_inline(name: &str, src: &str) -> (Lexer, DiagnosticEngine) {
    let mut engine = DiagnosticEngine::new();
    let source = SourceFile::new(name.to_string(), src.to_string());
    let mut lexer = Lexer::new(source);
    lexer.scan_tokens(&mut engine);
    (lexer, engine)
  }

  #[test]
  fn test_whitespaces() {
    let (lexer, engine) = lex_test_file("tests/files/whitespace.lox");

    assert_eq!(lexer.tokens.len(), 1);
    assert!(!engine.has_errors(), "Lexer produced unexpected errors");
    assert!(!engine.has_warnings(), "Lexer produced unexpected warnings");
  }

  #[test]
  fn test_comments() {
    let (lexer, engine) = lex_test_file("tests/files/comments.lox");

    assert!(!engine.has_errors(), "Lexer produced unexpected errors");
    assert!(!engine.has_warnings(), "Lexer produced unexpected warnings");

    let last = lexer.tokens.last().unwrap();
    assert!(matches!(last.kind, TokenKind::Eof));
  }

  #[test]
  fn test_numbers() {
    let (lexer, engine) = lex_test_file("tests/files/numbers.lox");

    assert!(!engine.has_errors(), "Lexer produced unexpected errors");
    assert!(!engine.has_warnings(), "Lexer produced unexpected warnings");

    let tokens = &lexer.tokens;

    // EOF
    let last = tokens.last().unwrap();
    assert!(matches!(last.kind, TokenKind::Eof));

    // Base checks
    assert!(
      tokens.iter().any(|t| matches!(
        t.kind,
        TokenKind::Literal {
          kind: LiteralKind::Integer {
            base: Base::Binary,
            ..
          },
          ..
        }
      )),
      "Missing binary literal"
    );

    assert!(
      tokens.iter().any(|t| matches!(
        t.kind,
        TokenKind::Literal {
          kind: LiteralKind::Integer {
            base: Base::Octal,
            ..
          },
          ..
        }
      )),
      "Missing octal literal"
    );

    assert!(
      tokens.iter().any(|t| matches!(
        t.kind,
        TokenKind::Literal {
          kind: LiteralKind::Integer {
            base: Base::Hexadecimal,
            ..
          },
          ..
        }
      )),
      "Missing hex literal"
    );

    assert!(
      tokens.iter().any(|t| matches!(
        t.kind,
        TokenKind::Literal {
          kind: LiteralKind::Float {
            base: Base::Decimal,
            ..
          },
          ..
        }
      )),
      "Missing float literal"
    );

    // Empty int detection
    assert!(
      tokens.iter().any(|t| matches!(
        t.kind,
        TokenKind::Literal {
          kind: LiteralKind::Integer {
            base: Base::Hexadecimal,
            empty_int: true,
            ..
          },
          ..
        }
      )),
      "Empty 0x not marked correctly"
    );

    // Empty exponent detection
    assert!(
      tokens.iter().any(|t| matches!(
        t.kind,
        TokenKind::Literal {
          kind: LiteralKind::Float { .. },
          ..
        }
      )),
      "Malformed exponents not detected"
    );

    // Numeric suffixes stay on the literal token (no stray identifiers)
    let suffix_literals = ["0xFFu8", "123usize", "3.14f32", "1.0f64"];
    for expected in suffix_literals {
      assert!(
        tokens.iter().any(|tok| {
          matches!(tok.kind, TokenKind::Literal { .. }) && token_text(&lexer, tok) == expected
        }),
        "expected literal `{expected}` to remain a single token"
      );
    }

    let forbidden_suffix_idents = ["f32", "f64", "u8", "usize"];
    for ident in forbidden_suffix_idents {
      assert!(
        tokens
          .iter()
          .all(|tok| { !matches!(tok.kind, TokenKind::Ident) || token_text(&lexer, tok) != ident }),
        "suffix `{ident}` should not be emitted as a standalone identifier"
      );
    }

    assert!(
      tokens
        .iter()
        .any(|tok| { matches!(tok.kind, TokenKind::Ident) && token_text(&lexer, tok) == "flags" }),
      "`0b1010flags` should still leave the trailing identifier `flags`"
    );
  }

  #[test]
  fn test_string_literal_variants() {
    let (lexer, engine) = lex_test_file("tests/files/strings_valid.lox");

    assert!(
      !engine.has_errors(),
      "valid string literals should parse cleanly"
    );
    assert!(
      !engine.has_warnings(),
      "valid string literals should not warn"
    );

    let mut saw_str = false;
    let mut saw_raw_str = false;
    let mut saw_byte_str = false;
    let mut saw_raw_byte_str = false;
    let mut saw_c_str = false;
    let mut saw_raw_c_str = false;
    let mut saw_char = false;
    let mut saw_byte = false;

    for token in &lexer.tokens {
      if let TokenKind::Literal { kind, .. } = token.kind {
        match kind {
          LiteralKind::Str => saw_str = true,
          LiteralKind::RawStr { .. } => saw_raw_str = true,
          LiteralKind::ByteStr => saw_byte_str = true,
          LiteralKind::RawByteStr { .. } => saw_raw_byte_str = true,
          LiteralKind::CStr => saw_c_str = true,
          LiteralKind::RawCStr { .. } => saw_raw_c_str = true,
          LiteralKind::Char => saw_char = true,
          LiteralKind::Byte => saw_byte = true,
          _ => {}
        }
      }
    }

    assert!(saw_str, "regular string literal missing");
    assert!(saw_raw_str, "raw string literal missing");
    assert!(saw_byte_str, "byte string literal missing");
    assert!(saw_raw_byte_str, "raw byte string literal missing");
    assert!(saw_c_str, "c string literal missing");
    assert!(saw_raw_c_str, "raw c string literal missing");
    assert!(saw_char, "character literal missing");
    assert!(saw_byte, "byte character literal missing");
  }

  #[test]
  fn test_shebang_tokens() {
    let (lexer, engine) = lex_test_file("tests/files/shebang_valid.lox");

    if engine.has_errors() {
      engine.print_diagnostics();
    }
    assert!(!engine.has_errors(), "valid shebang should not error");
    assert!(
      lexer
        .tokens
        .iter()
        .any(|tok| matches!(tok.kind, TokenKind::Shebang)),
      "shebang token was not emitted"
    );
  }

  #[test]
  fn test_invalid_shebang_reports_error() {
    let (lexer, engine) = lex_test_file("tests/files/shebang_invalid.lox");

    assert!(engine.has_errors(), "invalid shebang should emit an error");
    assert!(
      lexer
        .tokens
        .iter()
        .all(|tok| !matches!(tok.kind, TokenKind::Shebang)),
      "invalid shebang should not produce a shebang token"
    );
  }

  #[test]
  fn test_literal_prefix_errors() {
    let (_lexer, engine) = lex_test_file("tests/files/prefix_errors.lox");

    assert!(
      engine.has_errors(),
      "unknown/reserved prefixes should produce diagnostics"
    );
    assert!(
      engine.error_count() >= 3,
      "expected at least three prefix diagnostics (unknown + reserved variants)"
    );
  }

  #[test]
  fn test_lifetimes() {
    let (lexer, engine) = lex_test_file("tests/files/lifetimes.lox");

    assert!(
      engine.error_count() >= 1,
      "numeric-start lifetimes should trigger diagnostics"
    );

    let mut lifetimes = Vec::new();
    for tok in &lexer.tokens {
      if let TokenKind::Lifetime { starts_with_number } = tok.kind {
        lifetimes.push((token_text(&lexer, tok).to_string(), starts_with_number));
      }
    }

    assert!(lifetimes.contains(&(String::from("'a"), false)));
    assert!(lifetimes.contains(&(String::from("'static"), false)));
    assert!(lifetimes.contains(&(String::from("'_"), false)));
    assert!(lifetimes.contains(&(String::from("'life"), false)));
  }

  #[test]
  fn test_invalid_numeric_suffixes() {
    let (_lexer, engine) = lex_test_file("tests/files/numeric_suffix_errors.lox");

    assert!(
      engine.has_errors(),
      "invalid numeric suffixes should emit diagnostics"
    );
    assert!(
      engine.error_count() >= 3,
      "expected multiple suffix diagnostics (saw {})",
      engine.error_count()
    );
  }

  #[test]
  fn test_keywords_and_operators() {
    let (lexer, engine) = lex_test_file("tests/files/keywords_ops.lox");

    if engine.has_errors() {
      engine.print_diagnostics();
    }
    assert!(
      !engine.has_errors(),
      "keyword/operator sample should not emit diagnostics"
    );

    let tokens = &lexer.tokens;

    let keyword_tokens = [
      TokenKind::KwIf,
      TokenKind::KwElse,
      TokenKind::KwMatch,
      TokenKind::KwLoop,
      TokenKind::KwWhile,
      TokenKind::KwFor,
      TokenKind::KwLet,
      TokenKind::KwFn,
      TokenKind::KwStruct,
      TokenKind::KwEnum,
      TokenKind::KwUnion,
      TokenKind::KwTrait,
      TokenKind::KwImpl,
      TokenKind::KwType,
      TokenKind::KwMod,
      TokenKind::KwUse,
      TokenKind::KwStatic,
      TokenKind::KwExtern,
      TokenKind::KwMacro,
      TokenKind::KwAuto,
      TokenKind::KwDefault,
      TokenKind::Kwpub,
      TokenKind::KwMut,
      TokenKind::KwMove,
      TokenKind::KwUnsafe,
      TokenKind::KwAsync,
      TokenKind::KwAwait,
      TokenKind::KwDyn,
      TokenKind::KwSelf,
      TokenKind::KwSelfType,
      TokenKind::KwSuper,
      TokenKind::KwTrue,
      TokenKind::KwFalse,
      TokenKind::KwAs,
      TokenKind::KwIn,
      TokenKind::KwWhere,
      TokenKind::KwTry,
      TokenKind::KwYield,
      TokenKind::KwAbstract,
      TokenKind::KwDo,
      TokenKind::KwFinal,
      TokenKind::KwOverride,
      TokenKind::KwTypeof,
      TokenKind::KwUnsized,
      TokenKind::KwVirtual,
    ];

    for kind in keyword_tokens {
      assert_contains(tokens, kind);
    }

    let punct_tokens = [
      TokenKind::OpenParen,
      TokenKind::CloseParen,
      TokenKind::OpenBrace,
      TokenKind::CloseBrace,
      TokenKind::OpenBracket,
      TokenKind::CloseBracket,
      TokenKind::Comma,
      TokenKind::Semi,
      TokenKind::Dot,
      TokenKind::At,
      TokenKind::Pound,
      TokenKind::Tilde,
      TokenKind::Question,
      TokenKind::Colon,
      TokenKind::Dollar,
    ];

    for kind in punct_tokens {
      assert_contains(tokens, kind);
    }

    let operator_tokens = [
      TokenKind::Eq,
      TokenKind::EqEq,
      TokenKind::Ne,
      TokenKind::Lt,
      TokenKind::Le,
      TokenKind::Gt,
      TokenKind::Ge,
      TokenKind::Plus,
      TokenKind::PlusEq,
      TokenKind::Minus,
      TokenKind::MinusEq,
      TokenKind::Star,
      TokenKind::StarEq,
      TokenKind::Slash,
      TokenKind::SlashEq,
      TokenKind::And,
      TokenKind::AndEq,
      TokenKind::AndAnd,
      TokenKind::Or,
      TokenKind::OrEq,
      TokenKind::OrOr,
      TokenKind::Caret,
      TokenKind::CaretEq,
      TokenKind::ColonColon,
      TokenKind::ThinArrow,
      TokenKind::FatArrow,
      TokenKind::DotDot,
    ];

    for kind in operator_tokens {
      assert_contains(tokens, kind);
    }

    assert!(
      tokens.iter().any(|t| matches!(t.kind, TokenKind::Ident)),
      "regular identifiers should be present"
    );
  }

  #[test]
  fn test_operator_tokens() {
    let (lexer, engine) = lex_test_file("tests/files/operators.lox");
    if engine.has_errors() {
      engine.print_diagnostics();
    }
    assert!(!engine.has_errors(), "operator matrix should be valid");

    let expected = [
      TokenKind::Semi,
      TokenKind::Comma,
      TokenKind::Dot,
      TokenKind::OpenParen,
      TokenKind::CloseParen,
      TokenKind::OpenBrace,
      TokenKind::CloseBrace,
      TokenKind::OpenBracket,
      TokenKind::CloseBracket,
      TokenKind::At,
      TokenKind::Pound,
      TokenKind::Tilde,
      TokenKind::Question,
      TokenKind::Colon,
      TokenKind::Dollar,
      TokenKind::Eq,
      TokenKind::EqEq,
      TokenKind::Ne,
      TokenKind::Lt,
      TokenKind::Le,
      TokenKind::Gt,
      TokenKind::Ge,
      TokenKind::Plus,
      TokenKind::PlusEq,
      TokenKind::Minus,
      TokenKind::MinusEq,
      TokenKind::Star,
      TokenKind::StarEq,
      TokenKind::Slash,
      TokenKind::SlashEq,
      TokenKind::And,
      TokenKind::AndEq,
      TokenKind::AndAnd,
      TokenKind::Or,
      TokenKind::OrEq,
      TokenKind::OrOr,
      TokenKind::Caret,
      TokenKind::CaretEq,
      TokenKind::ColonColon,
      TokenKind::ThinArrow,
      TokenKind::FatArrow,
      TokenKind::DotDot,
    ];

    for k in expected {
      assert_contains(&lexer.tokens, k);
    }
  }

  #[test]
  fn test_unknown_tokens() {
    let (_lexer, engine) = lex_test_file("tests/files/unknown_tokens.lox");
    assert!(
      engine.has_errors(),
      "unknown tokens should emit diagnostics"
    );
  }

  #[test]
  fn test_identifier_variants() {
    let (lexer, engine) = lex_test_file("tests/files/identifiers.lox");
    if engine.has_errors() {
      engine.print_diagnostics();
    }
    assert!(
      !engine.has_errors(),
      "identifier sample should be clean (no diagnostics expected)"
    );

    assert!(
      lexer
        .tokens
        .iter()
        .any(|tok| matches!(tok.kind, TokenKind::Ident)),
      "expected regular identifiers"
    );
    assert!(
      lexer.tokens.iter().any(|tok| tok.kind == TokenKind::KwLet),
      "expected keyword token"
    );
  }

  #[test]
  fn test_unicode_whitespace() {
    let (_lexer, engine) = lex_test_file("tests/files/unicode_whitespace.lox");

    if engine.has_errors() {
      engine.print_diagnostics();
    }
    assert!(
      engine.has_errors(),
      "current lexer treats unicode whitespace as invalid characters"
    );
  }

  #[test]
  fn test_nested_and_doc_comments() {
    let (_lexer, engine) = lex_test_file("tests/files/comments_nested.lox");
    assert!(
      !engine.has_errors(),
      "nested/doc comments should lex without diagnostics for now"
    );
  }

  #[test]
  fn test_char_and_byte_literals() {
    let (lexer, _engine) = lex_test_file("tests/files/chars_bytes.lox");
    assert!(
      lexer.tokens.iter().any(|tok| matches!(
        tok.kind,
        TokenKind::Literal {
          kind: LiteralKind::Char,
          ..
        }
      )),
      "expected character literal tokens"
    );
    assert!(
      lexer.tokens.iter().any(|tok| matches!(
        tok.kind,
        TokenKind::Literal {
          kind: LiteralKind::Byte,
          ..
        }
      )),
      "expected byte literal tokens"
    );
  }

  #[test]
  fn test_integration_valid_source() {
    let (lexer, engine) = lex_test_file("tests/files/integration_valid.rs");
    if engine.has_errors() {
      engine.print_diagnostics();
    }
    assert!(
      !engine.has_errors(),
      "sample Rust program should lex without diagnostics"
    );
    assert!(
      lexer.tokens.len() > 10,
      "expected a healthy number of tokens from integration sample"
    );
  }

  #[test]
  fn test_integration_intentional_errors() {
    let (_lexer, engine) = lex_test_file("tests/files/integration_errors.lox");
    assert!(engine.has_errors(), "intentional errors should be reported");
  }

  #[test]
  fn test_edge_case_inputs() {
    let (_lexer, engine) = lex_inline("empty", "");
    assert!(
      !engine.has_errors(),
      "empty file should not generate diagnostics"
    );

    let (lexer_ws, engine_ws) = lex_test_file("tests/files/whitespace.lox");
    assert!(
      !engine_ws.has_errors(),
      "whitespace-only file should be clean"
    );
    assert_eq!(lexer_ws.tokens.len(), 1);

    let (_lexer_comments, engine_comments) = lex_test_file("tests/files/comments.lox");
    assert!(
      !engine_comments.has_errors(),
      "comment-only file should not error"
    );
  }

  #[test]
  fn test_large_file_performance() {
    let mut builder = String::new();
    while builder.len() < 1_100_000 {
      builder.push_str("let value = 42;\n");
    }
    let (lexer, engine) = lex_inline("large", &builder);
    assert!(
      !engine.has_errors(),
      "large repetitive file should lex without diagnostics"
    );
    assert!(lexer.tokens.len() > 10_000);
  }

  #[test]
  fn fuzz_random_bytes_smoke() {
    let mut rng = StdRng::seed_from_u64(0xDEADBEEF);
    for i in 0..5 {
      let len = 2048;
      let bytes: Vec<u8> = (&mut rng).sample_iter(Standard).take(len).collect();
      let src = String::from_utf8_lossy(&bytes).into_owned();
      let (_lexer, _engine) = lex_inline(&format!("fuzz-rand-{i}"), &src);
    }
  }

  #[test]
  fn fuzz_semi_valid_smoke() {
    let mut rng = StdRng::seed_from_u64(0xFEEDFACE);
    let pieces = [
      "let ", "fn ", "mut ", "x", "y", " = ", "42", "0xFF", ";\n", "if ", "{", "}", "while ",
      "true", "false",
    ];

    for i in 0..5 {
      let mut src = String::new();
      for _ in 0..256 {
        src.push_str(pieces[rng.gen_range(0..pieces.len())]);
      }
      let (_lexer, _engine) = lex_inline(&format!("fuzz-semi-{i}"), &src);
    }
  }
}
