//! Lexer for shebang lines.
//!
//! Handles shebang lines (`#!/usr/bin/env ...`) which are only valid
//! at the very start of a source file.

use crate::{token::TokenKind, Lexer};
use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
  DiagnosticEngine, Span,
};

impl Lexer {
  /// Lexes a shebang line (`#!...`).
  ///
  /// Shebangs are only valid at the very beginning of a file (byte offset 1).
  /// Consumes all characters until newline or EOF.
  ///
  /// # Arguments
  ///
  /// * `engine` - Diagnostic engine for error reporting
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::Shebang)` if valid, `None` otherwise
  pub fn lex_shebang(&mut self, engine: &mut DiagnosticEngine) -> Result<TokenKind, ()> {
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
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidShebang),
          "invalid shebang".to_string(),
          "demo.lox".to_string(),
        )
        .with_label(
          Span::new(self.start, self.current),
          Some("invalid shebang".to_string()),
          LabelStyle::Primary,
        );

        engine.add(diagnostic);

        return Err(());
      },
    }
  }
}
