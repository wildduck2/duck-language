#[cfg(test)]
mod keyword_tests {
  use crate::{token::TokenKind, Lexer};

  fn lex_single(input: &str) -> Result<TokenKind, ()> {
    use diagnostic::{DiagnosticEngine, SourceFile, SourceMap};
    use std::{cell::RefCell, path::PathBuf, rc::Rc};

    let engine = Rc::new(RefCell::new(DiagnosticEngine::new()));
    let mut _source_map = SourceMap::new();

    let test_file_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests")
      .join("files")
      .join("keyword_test_temp.lox");
    let path_str = test_file_path.to_str().unwrap().to_string();

    let source_file = SourceFile::new(path_str.clone(), input.to_string());
    _source_map.add_file(&path_str, input);
    engine.borrow_mut().add_file(&path_str, input);

    let mut lexer = Lexer::new(source_file, engine.clone());
    let _ = lexer.scan_tokens();

    if engine.borrow().has_errors() {
      return Err(());
    }

    let tokens: Vec<_> = lexer.tokens.iter().map(|t| t.kind).collect();
    tokens
      .into_iter()
      .find(|k| !matches!(k, TokenKind::Eof | TokenKind::Whitespace))
      .ok_or(())
  }

  fn assert_keyword(input: &str, expected: TokenKind) {
    let tok = lex_single(input).unwrap_or_else(|_| panic!("lexing failed for {input:?}"));
    assert_eq!(tok, expected);
  }

  #[test]
  fn recognizes_control_flow_keywords() {
    let cases = [
      ("if", TokenKind::KwIf),
      ("else", TokenKind::KwElse),
      ("match", TokenKind::KwMatch),
      ("loop", TokenKind::KwLoop),
      ("while", TokenKind::KwWhile),
      ("for", TokenKind::KwFor),
      ("break", TokenKind::KwBreak),
      ("continue", TokenKind::KwContinue),
      ("return", TokenKind::KwReturn),
    ];

    for (src, expected) in cases {
      assert_keyword(src, expected);
    }
  }

  #[test]
  fn recognizes_declaration_keywords() {
    let cases = [
      ("let", TokenKind::KwLet),
      ("fn", TokenKind::KwFn),
      ("struct", TokenKind::KwStruct),
      ("enum", TokenKind::KwEnum),
      ("union", TokenKind::KwUnion),
      ("trait", TokenKind::KwTrait),
      ("impl", TokenKind::KwImpl),
      ("type", TokenKind::KwType),
      ("mod", TokenKind::KwMod),
      ("use", TokenKind::KwUse),
      ("const", TokenKind::KwConst),
      ("static", TokenKind::KwStatic),
      ("extern", TokenKind::KwExtern),
      ("macro", TokenKind::KwMacro),
      ("auto", TokenKind::KwAuto),
      ("default", TokenKind::KwDefault),
    ];

    for (src, expected) in cases {
      assert_keyword(src, expected);
    }
  }

  #[test]
  fn recognizes_modifier_keywords() {
    let cases = [
      ("pub", TokenKind::Kwpub),
      ("mut", TokenKind::KwMut),
      ("ref", TokenKind::KwRef),
      ("move", TokenKind::KwMove),
      ("unsafe", TokenKind::KwUnsafe),
      ("async", TokenKind::KwAsync),
      ("await", TokenKind::KwAwait),
      ("dyn", TokenKind::KwDyn),
    ];

    for (src, expected) in cases {
      assert_keyword(src, expected);
    }
  }

  #[test]
  fn recognizes_special_and_literal_keywords() {
    let cases = [
      ("self", TokenKind::KwSelf),
      ("Self", TokenKind::KwSelfType),
      ("super", TokenKind::KwSuper),
      ("crate", TokenKind::KwCrate),
      ("true", TokenKind::KwTrue),
      ("false", TokenKind::KwFalse),
      ("as", TokenKind::KwAs),
      ("in", TokenKind::KwIn),
      ("where", TokenKind::KwWhere),
    ];

    for (src, expected) in cases {
      assert_keyword(src, expected);
    }
  }

  #[test]
  fn recognizes_reserved_keywords() {
    let cases = [
      ("abstract", TokenKind::KwAbstract),
      ("become", TokenKind::KwBecome),
      ("box", TokenKind::KwBox),
      ("do", TokenKind::KwDo),
      ("final", TokenKind::KwFinal),
      ("override", TokenKind::KwOverride),
      ("try", TokenKind::KwTry),
      ("typeof", TokenKind::KwTypeof),
      ("unsized", TokenKind::KwUnsized),
      ("virtual", TokenKind::KwVirtual),
      ("yield", TokenKind::KwYield),
    ];

    for (src, expected) in cases {
      assert_keyword(src, expected);
    }
  }

  #[test]
  fn identifier_variants() {
    assert_keyword("plain_ident", TokenKind::Ident);
    assert_keyword("r#type", TokenKind::RawIdent);
    assert_keyword("r#match", TokenKind::RawIdent);
  }
}
