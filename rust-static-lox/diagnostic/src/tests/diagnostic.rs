use crate::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  source_map::{Span, SourceMap},
  types::error::DiagnosticError,
};

#[test]
fn load_context_empty_labels_returns_empty() {
  let diag = Diagnostic::new(
    DiagnosticCode::Error(DiagnosticError::CodeNotFound),
    "msg".to_string(),
    "missing.rs".to_string(),
  );
  let map = SourceMap::new();
  let context = diag.load_context(&map);
  assert!(context.is_empty());
}

#[test]
fn load_context_missing_file_returns_empty() {
  let diag = Diagnostic::new(
    DiagnosticCode::Error(DiagnosticError::CodeNotFound),
    "msg".to_string(),
    "missing.rs".to_string(),
  )
  .with_label(Span::new(0, 1), None, LabelStyle::Primary);
  let map = SourceMap::new();
  let context = diag.load_context(&map);
  assert!(context.is_empty());
}
