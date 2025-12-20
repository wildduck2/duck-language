#[cfg(test)]
mod comment_tests {
  use crate::{
    token::{DocStyle, TokenKind},
    Lexer,
  };
  use diagnostic::{DiagnosticEngine, SourceFile};
  use std::{cell::RefCell, rc::Rc};

  fn lexer_for(input: &str) -> Lexer {
    let source = SourceFile::new("comment_test.lox".to_string(), input.to_string());
    let engine = Rc::new(RefCell::new(DiagnosticEngine::new()));
    Lexer::new(source, engine)
  }

  fn lex_comment_token(input: &str) -> TokenKind {
    let mut lexer = lexer_for(input);
    lexer.advance(); // consume initial '/'
    lexer.lex_slash().unwrap()
  }

  #[test]
  fn line_comment_doc_styles() {
    assert_eq!(
      lex_comment_token("// regular"),
      TokenKind::LineComment { doc_style: None }
    );
    assert_eq!(
      lex_comment_token("/// doc"),
      TokenKind::LineComment {
        doc_style: Some(DocStyle::Outer)
      }
    );
    assert_eq!(
      lex_comment_token("//! inner"),
      TokenKind::LineComment {
        doc_style: Some(DocStyle::Inner)
      }
    );
  }

  #[test]
  fn block_comment_doc_styles_and_termination() {
    assert_eq!(
      lex_comment_token("/* block */"),
      TokenKind::BlockComment {
        doc_style: None,
        terminated: true
      }
    );
    assert_eq!(
      lex_comment_token("/** outer */"),
      TokenKind::BlockComment {
        doc_style: Some(DocStyle::Outer),
        terminated: true
      }
    );
    assert_eq!(
      lex_comment_token("/*! inner */"),
      TokenKind::BlockComment {
        doc_style: Some(DocStyle::Inner),
        terminated: true
      }
    );
  }

  #[test]
  fn block_comment_nesting_and_unterminated() {
    assert_eq!(
      lex_comment_token("/* level /* nested */ still */"),
      TokenKind::BlockComment {
        doc_style: None,
        terminated: true
      }
    );

    assert_eq!(
      lex_comment_token("/* unterminated"),
      TokenKind::BlockComment {
        doc_style: None,
        terminated: false
      }
    );
  }
}
