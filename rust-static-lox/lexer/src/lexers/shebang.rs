//! Lexer for shebang lines.
//!
//! Handles shebang lines (`#!/usr/bin/env ...`) which are only valid
//! at the very start of a source file.

use crate::{token::TokenKind, Lexer};
use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
  Span,
};

impl Lexer {
  /// Lexes a shebang line (`#!...`).
  ///
  /// Shebangs are only valid at the very beginning of a file (byte offset 0).
  /// Consumes all characters until newline or EOF.
  ///
  /// `Ok(TokenKind::Shebang)` if valid, `None` otherwise
  pub(crate) fn lex_shebang(&mut self) -> Result<TokenKind, ()> {
    // Only valid at very beginning of the file (before any other text)
    if self.start != 0 {
      return Err(());
    }

    // Look ahead to decide what kind of shebang it is
    match self.peek() {
      // OS interpreter shebang, e.g. "#!/usr/bin/env rustrc"
      Some('/') => {
        // Consume until newline or EOF
        while let Some(c) = self.peek() {
          if c == '\n' || c == '\r' {
            break;
          }
          self.advance();
        }

        Ok(TokenKind::Shebang)
      },

      // Compiler attribute shebang, e.g. "#![allow(dead_code)]"
      Some('[') => Err(()),

      // Anything else is invalid for a shebang
      _ => {
        let span = Span::new(self.start, self.current);
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidShebang),
          "invalid shebang: shebang must start with `#!` followed by `/` or `[`".to_string(),
          self.source.path.clone(),
        )
        .with_label(
          span,
          Some("shebang must start with `#!` followed by `/` or `[`".to_string()),
          LabelStyle::Primary,
        )
        .with_help("shebangs must start with `#!` and be on the first line".to_string());
        self.emit_diagnostic(diagnostic);
        Err(())
      },
    }
  }
}
