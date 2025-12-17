#[cfg(test)]
mod arithmetic_tests {
  use crate::{token::TokenKind, Lexer};

  fn lex_single(input: &str) -> TokenKind {
    use diagnostic::{DiagnosticEngine, SourceFile, SourceMap};
    use std::path::PathBuf;

    let mut engine = DiagnosticEngine::new();
    let mut _source_map = SourceMap::new();

    let test_file_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests")
      .join("files")
      .join("arithmetic_test_temp.lox");
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

    lexer
      .tokens
      .iter()
      .map(|t| t.kind)
      .find(|k| !matches!(k, TokenKind::Eof))
      .unwrap()
  }

  #[test]
  fn plus_minus_and_arrows() {
    assert_eq!(lex_single("+"), TokenKind::Plus);
    assert_eq!(lex_single("+="), TokenKind::PlusEq);
    assert_eq!(lex_single("-"), TokenKind::Minus);
    assert_eq!(lex_single("-="), TokenKind::MinusEq);
    assert_eq!(lex_single("->"), TokenKind::ThinArrow);
  }

  #[test]
  fn star_slash_and_percent_variants() {
    assert_eq!(lex_single("*"), TokenKind::Star);
    assert_eq!(lex_single("*="), TokenKind::StarEq);
    assert_eq!(lex_single("/"), TokenKind::Slash);
    assert_eq!(lex_single("/="), TokenKind::SlashEq);
    assert_eq!(lex_single("%"), TokenKind::Percent);
    assert_eq!(lex_single("%="), TokenKind::PercentEq);
  }
}
