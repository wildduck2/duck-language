#[cfg(test)]
mod punctuation_tests {
  use crate::{token::TokenKind, Lexer};

  fn lex_kinds(input: &str) -> Result<Vec<TokenKind>, ()> {
    use diagnostic::{DiagnosticEngine, SourceFile, SourceMap};
    use std::{cell::RefCell, path::PathBuf, rc::Rc};

    let engine = Rc::new(RefCell::new(DiagnosticEngine::new()));
    let mut _source_map = SourceMap::new();

    let test_file_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests")
      .join("files")
      .join("punctuation_test_temp.lox");
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

  fn lex_single_token(input: &str) -> Result<TokenKind, ()> {
    let toks = lex_kinds(input)?;
    let mut iter = toks.into_iter().filter(|k| !matches!(k, TokenKind::Eof));
    match iter.next() {
      Some(tok) => Ok(tok),
      None => Err(()),
    }
  }

  #[test]
  fn single_character_punctuation() {
    let cases = [
      (";", TokenKind::Semi),
      (",", TokenKind::Comma),
      ("(", TokenKind::LParen),
      (")", TokenKind::RParen),
      ("{", TokenKind::LBrace),
      ("}", TokenKind::RBrace),
      ("[", TokenKind::LBracket),
      ("]", TokenKind::RBracket),
      ("@", TokenKind::At),
      ("#", TokenKind::Pound),
      ("~", TokenKind::Tilde),
      ("?", TokenKind::Question),
      ("$", TokenKind::Dollar),
    ];

    for (src, expected) in cases {
      assert_eq!(lex_single_token(src).unwrap(), expected, "src={src}");
    }
  }

  #[test]
  fn dot_and_colon_variants() {
    assert_eq!(lex_single_token(".").unwrap(), TokenKind::Dot);
    assert_eq!(lex_single_token("..").unwrap(), TokenKind::DotDot);
    assert_eq!(lex_single_token("..=").unwrap(), TokenKind::DotDotEq);

    assert_eq!(lex_single_token(":").unwrap(), TokenKind::Colon);
    assert_eq!(lex_single_token("::").unwrap(), TokenKind::ColonColon);
  }

  #[test]
  fn punctuation_sequences_in_stream() {
    let tokens = lex_kinds("..=()[]{};,").unwrap();
    let filtered: Vec<_> = tokens
      .into_iter()
      .filter(|k| !matches!(k, TokenKind::Eof))
      .collect();
    assert_eq!(
      filtered,
      vec![
        TokenKind::DotDotEq,
        TokenKind::LParen,
        TokenKind::RParen,
        TokenKind::LBracket,
        TokenKind::RBracket,
        TokenKind::LBrace,
        TokenKind::RBrace,
        TokenKind::Semi,
        TokenKind::Comma
      ]
    );
  }
}
