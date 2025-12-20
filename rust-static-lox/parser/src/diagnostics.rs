use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
  Span,
};

// /// Trailing `+` in a bounds list (e.g., where-clause, trait bounds, type params).
// pub fn err_trailing_plus_in_bounds(&self, span: Span, context: &str) -> Diagnostic {
//   Diagnostic::new(
//     DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
//     format!("trailing `+` in {context} bounds"),
//     self.path.to_string(),
//   )
//   .with_label(
//     span,
//     Some("remove this trailing `+` or add another bound".to_string()),
//     LabelStyle::Primary,
//   )
//   .with_help("write bounds like `Clone + Copy` without a dangling `+`".to_string())
// }
//
// /// Missing bound after `+` where the next token is clearly not a bound start.
// pub fn err_expected_bound_after_plus(&self, span: Span, context: &str) -> Diagnostic {
//   Diagnostic::new(
//     DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
//     format!("expected another bound after `+` in {context}"),
//     self.path.to_string(),
//   )
//   .with_label(
//     span,
//     Some("add a bound after `+`, e.g. `Trait` or `'a`".to_string()),
//     LabelStyle::Primary,
//   )
//   .with_note("bounds are separated by `+`, such as `Clone + 'a`".to_string())
// }
