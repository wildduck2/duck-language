//! Lexer for shebang lines.
//!
//! Handles shebang lines (`#!/usr/bin/env ...`) which are only valid
//! at the very start of a source file.

use crate::{token::TokenKind, Lexer};
use diagnostic::Span;

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
        self.emit_diagnostic(self.err_invalid_shebang(span, "shebang must start with `#!` followed by `/` or `[`"));
        Err(())
      },
    }
  }
}
