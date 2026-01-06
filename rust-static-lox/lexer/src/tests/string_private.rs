#[cfg(test)]
mod string_private_tests {
  use crate::Lexer;
  use diagnostic::{DiagnosticEngine, SourceFile};
  use std::{cell::RefCell, path::PathBuf, rc::Rc};

  fn make_lexer(input: &str) -> (Lexer, Rc<RefCell<DiagnosticEngine>>) {
    let engine = Rc::new(RefCell::new(DiagnosticEngine::new()));
    let test_file_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests")
      .join("files")
      .join("string_private_test_temp.lox");
    let path_str = test_file_path.to_str().unwrap().to_string();

    let source_file = SourceFile::new(path_str.clone(), input.to_string());
    engine.borrow_mut().add_file(&path_str, input);

    (Lexer::new(source_file, engine.clone()), engine)
  }

  #[test]
  fn raw_string_missing_open_quote_errors() {
    let (mut lexer, engine) = make_lexer("r#noquote");
    lexer.start = 0;
    lexer.current = 1;
    lexer.column = 1;
    assert!(lexer.lex_raw_str().is_err());
    assert!(engine.borrow().has_errors());
  }

  #[test]
  fn byte_string_missing_quote_reports_error() {
    let (mut lexer, engine) = make_lexer("bq");
    lexer.start = 0;
    lexer.current = 1;
    lexer.column = 1;
    assert!(lexer.lex_bstr().is_err());
    assert!(engine.borrow().has_errors());
  }

  #[test]
  fn byte_char_missing_quote_reports_error() {
    let (mut lexer, engine) = make_lexer("bx");
    lexer.start = 0;
    lexer.current = 1;
    lexer.column = 1;
    assert!(lexer.lex_bchar().is_err());
    assert!(engine.borrow().has_errors());
  }
}
