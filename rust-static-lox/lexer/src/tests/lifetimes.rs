#[cfg(test)]
mod lifetime_tests {
  use crate::{token::TokenKind, Lexer};

  fn lex_tokens(input: &str) -> Result<Vec<TokenKind>, ()> {
    use diagnostic::{DiagnosticEngine, SourceFile, SourceMap};
    use std::{cell::RefCell, path::PathBuf, rc::Rc};

    let engine = Rc::new(RefCell::new(DiagnosticEngine::new()));
    let mut _source_map = SourceMap::new();

    let test_file_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests")
      .join("files")
      .join("lifetime_test_temp.lox");
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

  fn lex_single_lifetime(input: &str) -> Result<TokenKind, ()> {
    let tokens = lex_tokens(input)?;
    let mut iter = tokens.into_iter().filter(|k| !matches!(k, TokenKind::Eof));
    match iter.next() {
      Some(tok @ TokenKind::Lifetime { .. }) => Ok(tok),
      _ => Err(()),
    }
  }

  fn assert_err(input: &str) {
    assert!(
      lex_tokens(input).is_err(),
      "expected lexer error for lifetime input {input:?}"
    );
  }

  #[test]
  fn lifetime_basic_cases() {
    let cases = ["'a", "'abc", "'static", "'_", "'LongName"];

    for src in cases {
      let tok = lex_single_lifetime(src).expect(src);
      assert_eq!(
        tok,
        TokenKind::Lifetime {
          starts_with_number: false
        }
      );
    }
  }

  #[test]
  fn lifetime_trims_trailing_punctuation() {
    let tokens = lex_tokens("'abc,").expect("should lex");
    let tokens: Vec<_> = tokens
      .into_iter()
      .filter(|k| !matches!(k, TokenKind::Eof))
      .collect();
    assert_eq!(
      tokens,
      vec![
        TokenKind::Lifetime {
          starts_with_number: false
        },
        TokenKind::Comma
      ]
    );
  }

  #[test]
  fn lifetime_rejects_numeric_prefix() {
    let cases = ["'1foo", "'0", "'9_name"];

    for src in cases {
      assert_err(src);
    }
  }

  #[test]
  fn lifetime_requires_identifier() {
    assert_err("'");
    assert_err("'\n");
  }
}
