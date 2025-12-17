//! Lexers for punctuation and delimiter tokens.
//!
//! Handles single-character tokens like semicolons, commas, brackets,
//! and multi-character sequences like `::` and `..=`.

use crate::{token::TokenKind, Lexer};
use diagnostic::DiagnosticEngine;

impl Lexer {
  /// Lexes a semicolon (`;`).
  ///
  /// Used to terminate statements and separate items.
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::Semi)`
  pub(crate) fn lex_semicolon(&mut self) -> Result<TokenKind, ()> {
    Ok(TokenKind::Semi)
  }

  /// Lexes a comma (`,`).
  ///
  /// Used to separate items in lists, function arguments, etc.
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::Comma)`
  pub(crate) fn lex_comma(&mut self) -> Result<TokenKind, ()> {
    Ok(TokenKind::Comma)
  }

  /// Lexes a dot (`.`) or dot sequences (`..`, `..=`).
  ///
  /// Handles:
  /// - `.` - Member access or decimal point
  /// - `..` - Range operator
  /// - `..=` - Inclusive range operator
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::Dot)`, `Some(TokenKind::DotDot)`, or `Some(TokenKind::DotDotEq)`
  pub(crate) fn lex_dot(&mut self) -> Result<TokenKind, ()> {
    if self.match_char('.') {
      if self.match_char('=') {
        return Ok(TokenKind::DotDotEq);
      }

      return Ok(TokenKind::DotDot);
    }

    Ok(TokenKind::Dot)
  }

  /// Lexes an open parenthesis (`(`).
  ///
  /// Used for grouping expressions, function calls, and tuples.
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::OpenParen)`
  pub(crate) fn lex_open_paren(&mut self) -> Result<TokenKind, ()> {
    Ok(TokenKind::OpenParen)
  }

  /// Lexes a close parenthesis (`)`).
  ///
  /// Closes grouping expressions, function calls, and tuples.
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::CloseParen)`
  pub(crate) fn lex_close_paren(&mut self) -> Result<TokenKind, ()> {
    Ok(TokenKind::CloseParen)
  }

  /// Lexes an open brace (`{`).
  ///
  /// Used for blocks, struct literals, and match arms.
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::OpenBrace)`
  pub(crate) fn lex_open_brace(&mut self) -> Result<TokenKind, ()> {
    Ok(TokenKind::OpenBrace)
  }

  /// Lexes a close brace (`}`).
  ///
  /// Closes blocks, struct literals, and match arms.
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::CloseBrace)`
  pub(crate) fn lex_close_brace(&mut self) -> Result<TokenKind, ()> {
    Ok(TokenKind::CloseBrace)
  }

  /// Lexes an open bracket (`[`).
  ///
  /// Used for array literals and indexing expressions.
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::OpenBracket)`
  pub(crate) fn lex_open_bracket(&mut self) -> Result<TokenKind, ()> {
    Ok(TokenKind::OpenBracket)
  }

  /// Lexes a close bracket (`]`).
  ///
  /// Closes array literals and indexing expressions.
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::CloseBracket)`
  pub(crate) fn lex_close_bracket(&mut self) -> Result<TokenKind, ()> {
    Ok(TokenKind::CloseBracket)
  }

  /// Lexes an at symbol (`@`).
  ///
  /// Used for pattern bindings and attributes.
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::At)`
  pub(crate) fn lex_at(&mut self) -> Result<TokenKind, ()> {
    Ok(TokenKind::At)
  }

  /// Lexes a pound symbol (`#`) or shebang (`#!`).
  ///
  /// Handles:
  /// - `#` - Attribute or pound token
  /// - `#!` - Shebang line (only valid at file start)
  ///
  /// # Arguments
  ///
  /// * `engine` - Diagnostic engine for error reporting
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::Pound)` or `Some(TokenKind::Shebang)`, or `None` for invalid shebang
  pub(crate) fn lex_pound(&mut self, engine: &mut DiagnosticEngine) -> Result<TokenKind, ()> {
    if self.start == 0 && self.match_char('!') {
      return self.lex_shebang(engine);
    }

    Ok(TokenKind::Pound)
  }

  /// Lexes a tilde symbol (`~`).
  ///
  /// Used for bitwise NOT operations.
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::Tilde)`
  pub(crate) fn lex_tilde(&mut self) -> Result<TokenKind, ()> {
    Ok(TokenKind::Tilde)
  }

  /// Lexes a question mark (`?`).
  ///
  /// Used for optional types and error propagation.
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::Question)`
  pub(crate) fn lex_question(&mut self) -> Result<TokenKind, ()> {
    Ok(TokenKind::Question)
  }

  /// Lexes a colon (`:`) or path separator (`::`).
  ///
  /// Handles:
  /// - `:` - Type annotations, match patterns
  /// - `::` - Path separator (e.g., `std::io`)
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::Colon)` or `Some(TokenKind::ColonColon)`
  pub(crate) fn lex_colon(&mut self) -> Result<TokenKind, ()> {
    if self.match_char(':') {
      return Ok(TokenKind::ColonColon);
    }

    Ok(TokenKind::Colon)
  }

  /// Lexes a dollar symbol (`$`).
  ///
  /// Used in macros and template contexts.
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::Dollar)`
  pub(crate) fn lex_dollar(&mut self) -> Result<TokenKind, ()> {
    Ok(TokenKind::Dollar)
  }
}
