use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
  DiagnosticEngine,
};

use crate::{token::TokenKind, Lexer};

impl Lexer {
  /// Lexes lifetime identifiers starting with `'`.
  ///
  /// Handles:
  /// - `'a`, `'static`, `'_'`
  /// - `'0abc` (invalid starts with number)
  /// - `'` alone (invalid / unterminated)
  ///
  /// Emits diagnostics for invalid or malformed lifetimes.
  ///
  /// Returns `TokenKind::Lifetime { starts_with_number }` or `TokenKind::Unknown` on error.
  pub fn lex_lifetime(&mut self, engine: &mut DiagnosticEngine) -> Result<TokenKind, ()> {
    let chars: Vec<char> = self.get_current_lexeme().chars().collect();

    // INFO: We’re iterating over the lexeme in reverse to handle potentially malformed lifetime
    // identifiers (e.g., `'a>;` or `'t):`). The goal is to isolate the valid lifetime portion (`'a`)
    // by trimming any trailing characters that aren't part of a proper lifetime name.
    //
    // The loop walks backward through the collected characters until it encounters the first
    // alphabetic character, which marks the end of the valid lifetime. At that point, we adjust
    // both `self.current` and `self.column` to discard the invalid suffix.
    //
    // This ensures that only syntactically valid lifetimes remain in the lexeme buffer before
    // further analysis or tokenization occurs.
    //
    // NOTE: Future enhancement — complex lifetime patterns (e.g., higher-ranked trait bounds like
    // `'a: 'b + 'c`) may require additional handling to ensure full correctness.

    let len = chars.len() - 1;
    for (i, c) in chars.into_iter().enumerate() {
      if c.is_ascii_alphabetic()
        || c.is_ascii_whitespace()
        || (c.is_ascii_punctuation() && matches!(c, '\'' | ','))
      {
        self.current -= len - i;
        self.column -= len - i;
        break;
      }
    }

    // NOTE: after we have consumed the appropriate number of characters, we need to consume
    // we consume the rest of the lifetime if there's any valid identifier after the apostrophe
    // for example 'wild
    // thus the life time without this would be like 'a only because the char only takes on char
    // and not a multi char identifier
    while let Some(c) = self.peek() {
      if c.is_ascii_alphabetic() || c == '_' {
        self.advance();
        continue;
      } else {
        break;
      }
    }

    let lexeme = self.get_current_lexeme();

    // Determine if the lifetime starts with a number after the apostrophe `'`
    let starts_with_number = lexeme
      .strip_prefix('\'') // remove the leading apostrophe
      .and_then(|rest| rest.chars().next()) // get the next char (if any)
      .map(|ch| ch.is_ascii_digit()) // check if it's a digit
      .unwrap_or(false); // default to false if no next char

    if starts_with_number {
      let diagnostic = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::InvalidLifetime),
        format!("'{}' is not a valid lifetime", lexeme),
        self.source.path.to_string(),
      )
      .with_label(
        diagnostic::Span::new(self.start, self.current),
        Some("Invalid lifetime start with number here".to_string()),
        LabelStyle::Primary,
      )
      .with_help("Lifetimes must not start with a number.".to_string());

      engine.add(diagnostic);
      return Err(());
    } else {
      // Return lifetime token directly no diagnostics at this stage
      Ok(TokenKind::Lifetime { starts_with_number })
    }
  }
}
