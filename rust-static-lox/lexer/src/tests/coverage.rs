#[cfg(test)]
mod coverage_tests {
  use crate::{
    token::{Base, DocStyle, LiteralKind, TokenKind},
    Lexer,
  };
  use diagnostic::{DiagnosticEngine, SourceFile, Span};
  use std::{cell::RefCell, path::PathBuf, rc::Rc};

  fn make_lexer(input: &str) -> (Lexer, Rc<RefCell<DiagnosticEngine>>) {
    let engine = Rc::new(RefCell::new(DiagnosticEngine::new()));
    let test_file_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests")
      .join("files")
      .join("coverage_test_temp.lox");
    let path_str = test_file_path.to_str().unwrap().to_string();

    let source_file = SourceFile::new(path_str.clone(), input.to_string());
    engine.borrow_mut().add_file(&path_str, input);

    (Lexer::new(source_file, engine.clone()), engine)
  }

  fn scan_tokens_with_engine(
    input: &str,
  ) -> (Vec<TokenKind>, Rc<RefCell<DiagnosticEngine>>) {
    let (mut lexer, engine) = make_lexer(input);
    let _ = lexer.scan_tokens();
    (lexer.tokens.iter().map(|t| t.kind).collect(), engine)
  }

  fn non_eof(tokens: Vec<TokenKind>) -> Vec<TokenKind> {
    tokens
      .into_iter()
      .filter(|k| !matches!(k, TokenKind::Eof))
      .collect()
  }

  #[test]
  fn token_kind_helpers_cover_all_branches() {
    use TokenKind::*;

    let lit_int = TokenKind::Literal {
      kind: LiteralKind::Integer {
        base: Base::Decimal,
        empty_int: false,
        suffix_start: 0,
      },
    };
    let lit_float = TokenKind::Literal {
      kind: LiteralKind::Float {
        base: Base::Decimal,
        suffix_start: 0,
      },
    };
    let lifetime = TokenKind::Lifetime {
      starts_with_number: false,
    };

    let path_starters = [
      Ident, KwSelf, KwSuper, KwCrate, Dollar, ColonColon, KwSelfType,
    ];
    for kind in path_starters {
      assert!(kind.can_start_path());
    }
    assert!(!At.can_start_path());

    let expr_starters = [
      Ident,
      lit_int,
      LParen,
      LBrace,
      LBracket,
      Minus,
      Bang,
      Star,
      Amp,
      KwIf,
      KwMatch,
      KwWhile,
      KwFor,
      KwLoop,
    ];
    for kind in expr_starters {
      assert!(kind.token_starts_expression());
    }
    assert!(!Semi.token_starts_expression());

    let full_expr_starters = [
      Ident,
      RawIdent,
      lit_float,
      LParen,
      LBracket,
      LBrace,
      Or,
      KwMove,
      Minus,
      Star,
      Bang,
      Amp,
      KwIf,
      KwMatch,
      KwWhile,
      KwLoop,
      KwFor,
      KwReturn,
      KwBreak,
      KwContinue,
      lifetime,
      ColonColon,
      KwSelf,
      KwSelfType,
      KwSuper,
      KwCrate,
      KwTry,
      KwConst,
      KwAsync,
      KwUnsafe,
      KwExtern,
      Lt,
    ];
    for kind in full_expr_starters {
      assert!(kind.can_start_expression());
    }
    assert!(!Semi.can_start_expression());

    let continue_expr = [
      Dot, LParen, LBracket, Question, KwAs, Plus, Minus, Star, Slash, Percent, Caret, Or, Amp,
      EqEq, Ne, Lt, Le, Gt, Ge, PlusEq, MinusEq, StarEq, SlashEq, PercentEq, AndEq, OrEq, CaretEq,
      ShiftLeftEq, ShiftRightEq, DotDot, DotDotEq, ThinArrow, FatArrow,
    ];
    for kind in continue_expr {
      assert!(kind.can_continue_expression());
    }
    assert!(!Semi.can_continue_expression());

    assert!(Plus.can_continue_expression_and_not(Star));
    assert!(!Plus.can_continue_expression_and_not(Plus));
    assert!(Semi.can_continue_expression_or(Semi));
    assert!(Ident.can_start_expression_and_not(Semi));
    assert!(!Ident.can_start_expression_and_not(Ident));
    assert!(Semi.can_start_expression_or(Semi));

    for kind in [EqEq, Ne, Lt, Le, Gt, Ge] {
      assert!(kind.is_binary_operator());
    }
    assert!(!Plus.is_binary_operator());

    assert!(Whitespace.is_trivia());
    assert!(LineComment { doc_style: None }.is_trivia());
    assert!(
      BlockComment {
        doc_style: Some(DocStyle::Outer),
        terminated: true
      }
      .is_trivia()
    );
    assert!(!Ident.is_trivia());

    assert!(lit_int.is_literal());
    assert!(!Ident.is_literal());

    let keywords = [
      KwAs,
      KwBreak,
      KwConst,
      KwContinue,
      KwCrate,
      KwElse,
      KwEnum,
      KwExtern,
      KwFalse,
      KwFn,
      KwFor,
      KwIf,
      KwImpl,
      KwIn,
      KwLet,
      KwLoop,
      KwMatch,
      KwMod,
      KwMove,
      KwMut,
      Kwpub,
      KwRef,
      KwReturn,
      KwSelf,
      KwSelfType,
      KwStatic,
      KwStruct,
      KwSuper,
      KwTrait,
      KwTrue,
      KwType,
      KwUnsafe,
      KwUse,
      KwWhere,
      KwWhile,
      KwAsync,
      KwAwait,
      KwDyn,
      KwAbstract,
      KwBecome,
      KwFinal,
      KwMacro,
      KwOverride,
      KwTry,
      KwTypeof,
      KwUnion,
      KwUnsized,
      KwYield,
      KwBox,
      KwDo,
      KwVirtual,
    ];
    for kind in keywords {
      assert!(kind.is_keyword());
    }
    assert!(!Ident.is_keyword());

    let string_like = [
      LiteralKind::Str,
      LiteralKind::ByteStr,
      LiteralKind::CStr,
      LiteralKind::RawStr { n_hashes: 0 },
      LiteralKind::RawByteStr { n_hashes: 1 },
      LiteralKind::RawCStr { n_hashes: 2 },
    ];
    for kind in string_like {
      assert!(kind.is_string_like());
    }
    assert!(!LiteralKind::Char.is_string_like());

    assert!(
      LiteralKind::Integer {
        base: Base::Decimal,
        empty_int: false,
        suffix_start: 0,
      }
      .is_numeric()
    );
    assert!(
      LiteralKind::Float {
        base: Base::Decimal,
        suffix_start: 0,
      }
      .is_numeric()
    );
    assert!(!LiteralKind::Str.is_numeric());

    assert!(LiteralKind::Char.is_char_like());
    assert!(LiteralKind::Byte.is_char_like());
    assert!(!LiteralKind::Str.is_char_like());
  }

  #[test]
  fn lexer_helpers_at_eof() {
    let (mut lexer, _) = make_lexer("");
    lexer.current = 0;
    assert_eq!(lexer.peek_next(0), None);
    assert_eq!(lexer.advance(), '\0');
  }

  #[test]
  fn scanner_utils_whitespace_newline_and_invalid_char() {
    let (mut lexer, _) = make_lexer("  a");
    lexer.start = 0;
    lexer.current = 1;
    lexer.column = 1;
    assert!(matches!(lexer.lex_tokens(' '), Ok(TokenKind::Whitespace)));

    let (mut lexer, _) = make_lexer("\n");
    lexer.start = 0;
    lexer.current = 1;
    lexer.column = 1;
    assert!(matches!(lexer.lex_tokens('\n'), Ok(TokenKind::Whitespace)));
    assert_eq!(lexer.line, 1);
    assert_eq!(lexer.column, 0);

    let (mut lexer, engine) = make_lexer("\u{7F}");
    lexer.start = 0;
    lexer.current = 1;
    lexer.column = 1;
    assert!(lexer.lex_tokens('\u{7F}').is_err());
    assert!(engine.borrow().has_errors());
  }

  #[test]
  fn lexer_diagnostics_cover_all_builders() {
    let (mut lexer, _) = make_lexer("x");
    let span = Span::new(0, 1);

    let _ = lexer.err_invalid_shebang(span, "bad");
    let _ = lexer.err_invalid_character(span, 'x');
    let _ = lexer.err_unterminated_string(span, "string");
    let _ = lexer.err_too_many_raw_str_hashes(span, 3, 2);
    let _ = lexer.err_invalid_string_start(span, "bad");
    let _ = lexer.err_invalid_escape(span, "q", Some("string"));
    let _ = lexer.err_invalid_escape(span, "q", None);
    let _ = lexer.err_unknown_prefix(span, "z");
    let _ = lexer.err_reserved_prefix(span, "f");
    let _ = lexer.err_invalid_lifetime(span, "'1", "bad");
    let _ = lexer.err_lifetime_starts_with_digit(span, "'1");
    let _ = lexer.err_lifetime_missing_name(span);
    let _ = lexer.err_invalid_integer(span, "bad int");
    let _ = lexer.err_integer_starts_with_underscore(span, "_1");
    let _ = lexer.err_binary_literal_no_digits(span);
    let _ = lexer.err_octal_literal_no_digits(span);
    let _ = lexer.err_hex_literal_no_digits(span);
    let _ = lexer.err_invalid_digit_in_base(span, '9', "binary");
    let _ = lexer.err_consecutive_underscores(span, "decimal");
    let _ = lexer.err_underscore_after_prefix(span, "0x");
    let _ = lexer.err_invalid_identifier(span, "9abc");
    let _ = lexer.err_invalid_literal(span, "bad literal");
    let _ = lexer.err_empty_char(span);

    lexer.emit_diagnostic(lexer.err_invalid_character(span, 'x'));
    assert!(lexer.engine.borrow().has_errors());
  }

  #[test]
  fn lex_keywords_invalid_identifier_path() {
    let (mut lexer, engine) = make_lexer("1abc");
    lexer.start = 0;
    lexer.current = 1;
    lexer.column = 1;
    assert!(lexer.lex_keywords().is_err());
    assert!(engine.borrow().has_errors());
  }

  #[test]
  fn lex_lifetime_starts_with_digit_path() {
    let (mut lexer, _) = make_lexer("'abc");
    lexer.start = 0;
    lexer.current = 1;
    lexer.column = 1;
    assert!(lexer.lex_lifetime().is_ok());

    let (mut lexer, engine) = make_lexer("'1foo");
    lexer.start = 0;
    lexer.current = 1;
    lexer.column = 1;
    assert!(lexer.lex_lifetime().is_err());
    assert!(engine.borrow().has_errors());
  }

  #[test]
  fn comment_and_shebang_edges() {
    let (mut lexer, _) = make_lexer("// line\nnext");
    lexer.advance();
    let _ = lexer.lex_slash().unwrap();

    let (mut lexer, _) = make_lexer("/* line\nmore */");
    lexer.advance();
    let _ = lexer.lex_slash().unwrap();
    assert_eq!(lexer.line, 1);

    let (mut lexer, _) = make_lexer("#!/bin\n");
    lexer.start = 0;
    lexer.current = 2;
    lexer.column = 2;
    assert_eq!(lexer.lex_shebang().unwrap(), TokenKind::Shebang);

    let (mut lexer, _) = make_lexer("#!/bin");
    lexer.start = 1;
    lexer.current = 2;
    lexer.column = 2;
    assert!(lexer.lex_shebang().is_err());
  }

  #[test]
  fn number_edge_cases_for_suffixes_and_dot() {
    let (tokens, engine) = scan_tokens_with_engine("1.foo");
    assert!(!engine.borrow().has_errors());
    let tokens = non_eof(tokens);
    assert!(matches!(
      tokens.as_slice(),
      [
        TokenKind::Literal {
          kind: LiteralKind::Integer { base: Base::Decimal, empty_int: false, .. }
        },
        TokenKind::Dot,
        TokenKind::Ident
      ]
    ));

    let (tokens, engine) = scan_tokens_with_engine("1e1_2");
    assert!(!engine.borrow().has_errors());
    let tokens = non_eof(tokens);
    assert!(matches!(
      tokens.as_slice(),
      [TokenKind::Literal { kind: LiteralKind::Float { base: Base::Decimal, .. } }]
    ));

    for src in ["1.0f32", "1.0f64"] {
      let (tokens, engine) = scan_tokens_with_engine(src);
      assert!(!engine.borrow().has_errors());
      let tokens = non_eof(tokens);
      assert_eq!(tokens.len(), 1);
      assert!(matches!(
        tokens[0],
        TokenKind::Literal { kind: LiteralKind::Float { base: Base::Decimal, .. } }
      ));
    }

    let (tokens, engine) = scan_tokens_with_engine("1.0f128");
    assert!(!engine.borrow().has_errors());
    assert!(!tokens.is_empty());

    for src in ["1u128", "1u64", "1usize"] {
      let (tokens, engine) = scan_tokens_with_engine(src);
      assert!(!engine.borrow().has_errors());
      let tokens = non_eof(tokens);
      assert_eq!(tokens.len(), 1);
      assert!(matches!(
        tokens[0],
        TokenKind::Literal { kind: LiteralKind::Integer { base: Base::Decimal, .. } }
      ));
    }

    for src in ["1u1", "1u6", "1usiz"] {
      let (_, engine) = scan_tokens_with_engine(src);
      assert!(engine.borrow().has_errors());
    }

    let (tokens, engine) = scan_tokens_with_engine("0x1p1_2");
    assert!(!engine.borrow().has_errors());
    let tokens = non_eof(tokens);
    assert!(matches!(
      tokens.as_slice(),
      [TokenKind::Literal { kind: LiteralKind::Float { base: Base::Hexadecimal, .. } }]
    ));
  }

  #[test]
  fn number_suffix_direct_paths() {
    for src in ["1.0f32", "1.0f64"] {
      let (mut lexer, engine) = make_lexer(src);
      lexer.start = 0;
      lexer.current = 1;
      lexer.column = 1;
      let tok = lexer.lex_number().expect("lex number");
      assert!(!engine.borrow().has_errors());
      assert!(matches!(
        tok,
        TokenKind::Literal { kind: LiteralKind::Float { base: Base::Decimal, .. } }
      ));
    }

    for src in ["1u128", "1usize"] {
      let (mut lexer, engine) = make_lexer(src);
      lexer.start = 0;
      lexer.current = 1;
      lexer.column = 1;
      let tok = lexer.lex_number().expect("lex number");
      assert!(!engine.borrow().has_errors());
      assert!(matches!(
        tok,
        TokenKind::Literal { kind: LiteralKind::Integer { base: Base::Decimal, .. } }
      ));
    }
  }

  #[test]
  fn string_prefix_and_error_edges() {
    let (mut lexer, engine) = make_lexer("f\"x\"");
    lexer.start = 0;
    lexer.current = 1;
    lexer.column = 1;
    assert!(lexer.lex_string().is_err());
    assert!(engine.borrow().has_errors());

    let (mut lexer, engine) = make_lexer("z\"x\"");
    lexer.start = 0;
    lexer.current = 1;
    lexer.column = 1;
    assert!(lexer.lex_string().is_err());
    assert!(engine.borrow().has_errors());

    let (mut lexer, engine) = make_lexer("cr\"x\"");
    lexer.start = 0;
    lexer.current = 1;
    lexer.column = 1;
    assert!(lexer.lex_string().is_ok());
    assert!(!engine.borrow().has_errors());

    let (mut lexer, _) = make_lexer("b");
    lexer.start = 0;
    lexer.current = 1;
    lexer.column = 1;
    assert!(lexer.lex_string().is_err());

    let (mut lexer, _) = make_lexer("z");
    assert!(!lexer.is_string_prefix('z'));
  }

  #[test]
  fn string_escape_edges() {
    let (_, engine) = scan_tokens_with_engine("br#abc");
    assert!(engine.borrow().has_errors());

    let (_, engine) = scan_tokens_with_engine("cr#noquote");
    assert!(engine.borrow().has_errors());

    let (_, engine) = scan_tokens_with_engine("b\"\\x1\"");
    assert!(engine.borrow().has_errors());

    let (_, engine) = scan_tokens_with_engine("b'\\x'");
    assert!(engine.borrow().has_errors());

    let (_, engine) = scan_tokens_with_engine("b'\\\n'");
    assert!(engine.borrow().has_errors());

    let (_, engine) = scan_tokens_with_engine("b'\n");
    assert!(engine.borrow().has_errors());

    let (_, engine) = scan_tokens_with_engine("'\\x'");
    assert!(engine.borrow().has_errors());

    let (_, engine) = scan_tokens_with_engine("'\\u{G}'");
    assert!(engine.borrow().has_errors());

    let (_, engine) = scan_tokens_with_engine("'\\u{123");
    assert!(engine.borrow().has_errors());

    let (tokens, engine) = scan_tokens_with_engine("r#\"line1\n   line2\"#");
    assert!(!engine.borrow().has_errors());
    let tokens = non_eof(tokens);
    assert!(matches!(
      tokens.as_slice(),
      [TokenKind::Literal { kind: LiteralKind::RawStr { n_hashes: 1 } }]
    ));

    let (_, engine) = scan_tokens_with_engine("r#\"line1\nr#\"next\"#");
    assert!(engine.borrow().has_errors());
  }

  #[test]
  fn char_length_error_branch() {
    let (_, engine) = scan_tokens_with_engine("'ab'");
    assert!(engine.borrow().has_errors());
  }
}

