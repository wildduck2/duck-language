use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
  DiagnosticEngine,
};

use crate::{token::TokenKind, Lexer};

impl Lexer {
  /// Lexes lifetime identifiers that start with `'`.
  ///
  /// Supports common forms such as `'a`, `'static`, and `'_`, trims trailing
  /// punctuation like commas, and emits diagnostics for lone `'` tokens or
  /// lifetimes that begin with a digit.
  pub(crate) fn lex_lifetime(&mut self, engine: &mut DiagnosticEngine) -> Result<TokenKind, ()> {
    let chars: Vec<char> = self.get_current_lexeme().chars().collect();

    // Walk the collected characters to trim trailing punctuation so `'a,`
    // lexes the lifetime first and leaves the comma for the next token.
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

    // Consume any remaining identifier characters after the apostrophe.
    while let Some(c) = self.peek() {
      if c.is_ascii_alphabetic() || c == '_' {
        self.advance();
        continue;
      } else {
        break;
      }
    }

    let lexeme = self.get_current_lexeme();
    let after_quote = lexeme.strip_prefix('\'').unwrap_or("");
    let has_identifier = after_quote.chars().any(|ch| !ch.is_whitespace());

    if !has_identifier {
      let diagnostic = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::InvalidLifetime),
        "expected identifier after `'`".to_string(),
        self.source.path.to_string(),
      )
      .with_label(
        diagnostic::Span::new(self.start, self.current),
        Some("no lifetime name provided".to_string()),
        LabelStyle::Primary,
      )
      .with_help("try providing a name like `'a` or use `'_`".to_string());
      engine.add(diagnostic);
      return Err(());
    }

    // Determine if the lifetime starts with a number after the apostrophe `'`
    let starts_with_number = after_quote
      .chars()
      .next() // get the next char (if any)
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
        Some("lifetime names cannot start with digits".to_string()),
        LabelStyle::Primary,
      )
      .with_help("rename the lifetime so it begins with a letter or `_`".to_string());

      engine.add(diagnostic);
      Err(())
    } else {
      // Return lifetime token directly no diagnostics at this stage
      Ok(TokenKind::Lifetime { starts_with_number })
    }
  }
}
