#[cfg(test)]
mod shebang_tests {
  use crate::{token::TokenKind, Lexer};

  fn lex_tokens(input: &str) -> Result<Vec<TokenKind>, ()> {
    use diagnostic::{DiagnosticEngine, SourceFile, SourceMap};
    use std::path::PathBuf;

    let mut engine = DiagnosticEngine::new();
    let mut _source_map = SourceMap::new();
    let test_file_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests")
      .join("files")
      .join("shebang_test_temp.lox");
    let path_str = test_file_path.to_str().unwrap().to_string();

    let source_file = SourceFile::new(path_str.clone(), input.to_string());
    _source_map.add_file(&path_str, input);
    engine.add_file(&path_str, input);

    let mut lexer = Lexer::new(source_file);
    let _ = lexer.scan_tokens(&mut engine);

    if engine.has_errors() {
      return Err(());
    }

    Ok(
      lexer
        .tokens
        .iter()
        .map(|t| t.kind)
        .filter(|k| !matches!(k, TokenKind::Whitespace | TokenKind::Eof))
        .collect(),
    )
  }

  #[test]
  fn shebang_only_valid_at_start() {
    let tokens = lex_tokens("#!/usr/bin/env rustrc").unwrap();
    assert_eq!(tokens[0], TokenKind::Shebang);
  }

  #[test]
  fn invalid_shebang_forms_error() {
    assert!(lex_tokens("#![allow(dead_code)]").is_err());
    assert!(lex_tokens("#!oops").is_err());
  }

  #[test]
  fn pound_followed_by_bang_in_code() {
    let tokens = lex_tokens("let x = #!false;").unwrap();
    let filtered = tokens;
    assert_eq!(
      filtered,
      vec![
        TokenKind::KwLet,
        TokenKind::Ident,
        TokenKind::Eq,
        TokenKind::Pound,
        TokenKind::Bang,
        TokenKind::KwFalse,
        TokenKind::Semi
      ]
    );
  }
}
