#[cfg(test)]
mod bitwise_tests {
  use crate::{token::TokenKind, Lexer};

  fn lex_tokens(input: &str) -> Vec<TokenKind> {
    use diagnostic::{DiagnosticEngine, SourceFile, SourceMap};
    use std::path::PathBuf;

    let mut engine = DiagnosticEngine::new();
    let mut _source_map = SourceMap::new();
    let test_file_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests")
      .join("files")
      .join("bitwise_test_temp.lox");
    let path_str = test_file_path.to_str().unwrap().to_string();

    let source_file = SourceFile::new(path_str.clone(), input.to_string());
    _source_map.add_file(&path_str, input);
    engine.add_file(&path_str, input);

    let mut lexer = Lexer::new(source_file);
    let _ = lexer.scan_tokens(&mut engine);
    assert!(
      !engine.has_errors(),
      "unexpected diagnostic lexing {input:?}"
    );

    lexer.tokens.iter().map(|t| t.kind).collect()
  }

  fn lex_one(input: &str) -> TokenKind {
    lex_tokens(input)
      .into_iter()
      .find(|k| !matches!(k, TokenKind::Eof))
      .unwrap()
  }

  #[test]
  fn and_variants() {
    assert_eq!(lex_one("&"), TokenKind::And);
    assert_eq!(lex_one("&="), TokenKind::AndEq);
    let tokens: Vec<_> = lex_tokens("&&")
      .into_iter()
      .filter(|k| !matches!(k, TokenKind::Eof))
      .collect();
    assert_eq!(tokens, vec![TokenKind::And, TokenKind::And]);
  }

  #[test]
  fn or_variants() {
    assert_eq!(lex_one("|"), TokenKind::Or);
    assert_eq!(lex_one("|="), TokenKind::OrEq);
    let tokens: Vec<_> = lex_tokens("||")
      .into_iter()
      .filter(|k| !matches!(k, TokenKind::Eof))
      .collect();
    assert_eq!(tokens, vec![TokenKind::Or, TokenKind::Or]);
  }

  #[test]
  fn caret_variants() {
    assert_eq!(lex_one("^"), TokenKind::Caret);
    assert_eq!(lex_one("^="), TokenKind::CaretEq);
  }
}
