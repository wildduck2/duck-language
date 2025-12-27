use diagnostic::Span;

use crate::{token::TokenKind, Lexer};

impl Lexer {
  /// Lexes lifetime identifiers that start with `'`.
  ///
  /// Supports common forms such as `'a`, `'static`, and `'_`, trims trailing
  /// punctuation like commas, and emits diagnostics for lone `'` tokens or
  /// lifetimes that begin with a digit.
  pub(crate) fn lex_lifetime(&mut self) -> Result<TokenKind, ()> {
    let raw_lexeme = self.get_current_lexeme();

    // Trim trailing punctuation so `'a,` lexes the lifetime first and leaves
    // the comma for the next token.
    let mut trim_chars = 0usize;
    for c in raw_lexeme.chars().rev() {
      if c.is_ascii_alphanumeric() || c == '_' || c == '\'' {
        break;
      }
      trim_chars += 1;
    }
    if trim_chars > 0 {
      self.current = self.current.saturating_sub(trim_chars);
      self.column = self.column.saturating_sub(trim_chars);
    }

    // Consume any remaining identifier characters after the apostrophe.
    while let Some(c) = self.peek() {
      if c.is_ascii_alphanumeric() || c == '_' {
        self.advance();
        continue;
      }
      break;
    }

    let lexeme = self.get_current_lexeme();
    let after_quote = lexeme.strip_prefix('\'').unwrap_or("");
    let has_identifier = after_quote.chars().any(|ch| !ch.is_whitespace());

    if !has_identifier {
      let span = Span::new(self.start, self.current);
      self.emit_diagnostic(self.err_lifetime_missing_name(span));
      return Err(());
    }

    // Determine if the lifetime starts with a number after the apostrophe `'`
    let starts_with_number = after_quote
      .chars()
      .next() // get the next char (if any)
      .map(|ch| ch.is_ascii_digit()) // check if it's a digit
      .unwrap_or(false); // default to false if no next char

    if starts_with_number {
      let span = Span::new(self.start, self.current);
      self.emit_diagnostic(self.err_lifetime_starts_with_digit(span, &lexeme));
      Err(())
    } else {
      // Return lifetime token directly no diagnostics at this stage
      Ok(TokenKind::Lifetime { starts_with_number })
    }
  }
}
