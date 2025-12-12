//! TODO: Lifetime parser does not yet match full Rust grammar.
//!
//! Missing features:
//!
//! 1. Lifetime parameter syntax inside for<...> is incomplete.
//!    Rust allows:  'a: 'b + 'c
//!    The current parse_for_lifetimes only parses bare lifetimes.
//!    It does not parse bounds for lifetime parameters inside the binder.
//!
//! 2. parse_lifetimes should reject invalid tokens inside <...>.
//!    Rust does strict validation of what can appear in generic parameter lists.
//!
//! 3. parse_lifetime must validate that the lifetime token is a valid named lifetime,
//!    not the anonymous lifetime '_ which behaves differently.
//!
//! 4. Lifetime bounds parsing is incomplete:
//!    - Missing recognition of closing paren ')' as a terminator.
//!    - Missing recognition of semicolon ';' as a terminator.
//!    - Does not allow higher ranked lifetimes inside lifetime bounds.
//!
//! 5. parse_for_lifetimes must allow spaces and comments between tokens exactly as Rust does.
//!
//! 6. Missing support for higher ranked lifetimes in type position:
//!    for<'a> fn(&'a T)
//!
//! 7. Does not validate that repeated lifetimes inside for<...> are forbidden:
//!    for<'a, 'a> is invalid.
//!
//! 8. Lifetime names must follow Rust rules:
//!    - after the quote, the first char must be alphabetic or underscore
//!    - subsequent chars must be alphanumeric or underscore
//!
//! 9. parse_lifetime_bounds must correctly handle multiple plus signs,
//!    and must produce diagnostics for trailing plus signs.
//!
//! 10. parse_lifetime should handle the anonymous lifetime '_ specially,
//!     because Rust treats it differently than named lifetimes.
//!

use crate::{DiagnosticEngine, Parser};
use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
};
use lexer::token::TokenKind;

impl Parser {
  /// Parse a list of lifetimes separated by commas.
  ///
  /// Grammar:
  /// ```
  /// lifetimes -> lifetime ("," lifetime)* ","?
  /// ```
  ///
  /// Example:
  /// ```rust
  /// <'a, 'b, 'c>
  /// ```
  ///
  /// Returns a vector of lifetime names as strings.
  pub(crate) fn parse_lifetimes(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Vec<String>, ()> {
    let mut lifetimes = vec![];
    while !self.is_eof() && matches!(self.current_token().kind, TokenKind::Lifetime { .. }) {
      lifetimes.push(self.parse_lifetime(engine)?);

      if matches!(self.current_token().kind, TokenKind::Comma) {
        self.advance(engine); // consume ','
      } else {
        break;
      }
    }
    Ok(lifetimes)
  }

  /// Parse a single lifetime token.
  ///
  /// Grammar:
  /// ```
  /// lifetime -> LIFETIME
  /// ```
  ///
  /// Example:
  /// ```rust
  /// 'a
  /// ```
  ///
  /// Returns the lifetime name (including the leading `'`).
  /// Emits a diagnostic if the token is not a valid lifetime.
  pub(crate) fn parse_lifetime(&mut self, engine: &mut DiagnosticEngine) -> Result<String, ()> {
    let token = self.current_token();

    if matches!(token.kind, TokenKind::Lifetime { .. }) {
      let lifetime = self.get_token_lexeme(&token);
      self.advance(engine); // consume the lifetime
      return Ok(lifetime);
    }

    let lexeme = self.get_token_lexeme(&self.current_token());
    let diagnostic = Diagnostic::new(
      DiagnosticCode::Error(DiagnosticError::InvalidLifetime),
      format!("unexpected token `{}` in lifetime", lexeme),
      self.source_file.path.clone(),
    )
    .with_label(
      token.span,
      Some("expected a valid lifetime here".to_string()),
      LabelStyle::Primary,
    )
    .with_note("a lifetime must be a valid identifier, like `'a` or `'b`".to_string());

    engine.add(diagnostic);
    Err(())
  }

  /// Parse a `for<'a, 'b>` higher-ranked lifetime binder used in type predicates.
  ///
  /// Grammar:
  /// ```
  /// forLifetimes  -> "for" "<" lifetimeParam ("," lifetimeParam)* ","? ">"
  /// lifetimeParam -> LIFETIME (":" lifetimeBounds)?
  /// ```
  ///
  /// Example:
  /// ```rust
  /// for<'a, 'b> F: Fn(&'a T, &'b U)
  /// ```
  ///
  /// Returns a vector of lifetime names within the `for` clause.
  pub(crate) fn parse_for_lifetimes(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Vec<String>, ()> {
    self.expect(TokenKind::KwFor, engine)?; // consume 'for'
    self.expect(TokenKind::Lt, engine)?; // consume '<'

    let lifetimes = self.parse_lifetimes(engine)?;
    self.expect(TokenKind::Gt, engine)?; // consume '>'
    Ok(lifetimes)
  }

  /// Parse a lifetime bound list such as `: 'a + 'b`.
  ///
  /// Grammar:
  /// ```
  /// lifetimeBounds -> lifetime ("+" lifetime)* "+"?
  /// ```
  ///
  /// Example:
  /// ```rust
  /// 'a: 'b + 'c
  /// ```
  ///
  /// Stops parsing when reaching delimiters such as `{`, `,`, or `>`.
  pub(crate) fn parse_lifetime_bounds(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Vec<String>, ()> {
    let mut bounds = vec![];
    while !self.is_eof()
      && !matches!(
        self.current_token().kind,
        TokenKind::OpenBrace | TokenKind::Comma | TokenKind::Gt
      )
    {
      let lifetime = self.parse_lifetime(engine)?;

      if matches!(self.current_token().kind, TokenKind::Plus) {
        self.advance(engine); // consume '+'
      }

      bounds.push(lifetime);
    }

    Ok(bounds)
  }
}
