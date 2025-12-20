#[cfg(test)]
mod assignment_tests {
  use crate::{token::TokenKind, Lexer};

  fn lex_all(input: &str) -> Vec<TokenKind> {
    use diagnostic::{DiagnosticEngine, SourceFile, SourceMap};
    use std::{cell::RefCell, path::PathBuf, rc::Rc};

    let engine = Rc::new(RefCell::new(DiagnosticEngine::new()));
    let mut _source_map = SourceMap::new();
    let test_file_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests")
      .join("files")
      .join("assignment_test_temp.lox");
    let path_str = test_file_path.to_str().unwrap().to_string();

    let source_file = SourceFile::new(path_str.clone(), input.to_string());
    _source_map.add_file(&path_str, input);
    engine.borrow_mut().add_file(&path_str, input);

    let mut lexer = Lexer::new(source_file, engine.clone());
    let _ = lexer.scan_tokens();
    assert!(
      !engine.borrow().has_errors(),
      "unexpected diagnostic lexing {input:?}"
    );

    lexer
      .tokens
      .iter()
      .map(|t| t.kind)
      .filter(|k| !matches!(k, TokenKind::Eof | TokenKind::Whitespace))
      .collect()
  }

  fn lex_one(input: &str) -> TokenKind {
    let mut tokens = lex_all(input);
    assert_eq!(tokens.len(), 1, "expected single token for {input:?}");
    tokens.remove(0)
  }

  #[test]
  fn equals_and_bang_variants() {
    assert_eq!(lex_one("="), TokenKind::Eq);
    assert_eq!(lex_one("=="), TokenKind::EqEq);
    assert_eq!(lex_one("=>"), TokenKind::FatArrow);
    assert_eq!(lex_one("!"), TokenKind::Bang);
    assert_eq!(lex_one("!="), TokenKind::Ne);
  }

  #[test]
  fn less_than_family() {
    assert_eq!(lex_one("<"), TokenKind::Lt);
    assert_eq!(lex_one("<="), TokenKind::Le);
    assert_eq!(lex_one("<<="), TokenKind::ShiftLeftEq);
    // `<<` should be lexed as two `<` tokens.
    assert_eq!(lex_all("<<"), vec![TokenKind::Lt, TokenKind::Lt]);
  }

  #[test]
  fn greater_than_family() {
    assert_eq!(lex_one(">"), TokenKind::Gt);
    assert_eq!(lex_one(">="), TokenKind::Ge);
    assert_eq!(lex_one(">>="), TokenKind::ShiftRightEq);
    assert_eq!(lex_all(">>"), vec![TokenKind::Gt, TokenKind::Gt]);
  }
}
