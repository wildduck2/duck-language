//! Lexers for bitwise and logical operators.
//!
//! The lexer emits tokens for single `&`, `|`, and `^` glyphs as well as their
//! compound-assignment forms (`&=`, `|=`, `^=`). Sequences such as `&&` and `||`
//! are intentionally tokenized as two separate single-character tokens—the
//! parser decides whether they form a logical operator.

use crate::{token::TokenKind, Lexer};

impl Lexer {
  /// Lexes an ampersand (`&`) or compound assignment (`&=`).
  ///
  /// Handles:
  /// - `&` - Bitwise AND or borrow
  /// - `&=` - Bitwise AND assignment
  ///
  /// Repeated ampersands (`&&`) are tokenized as two `TokenKind::And` entries so
  /// the parser can decide whether they represent a logical-AND expression.
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::And)` or `Ok(TokenKind::AndEq)`
  pub(crate) fn lex_and(&mut self) -> Result<TokenKind, ()> {
    if self.match_char('=') {
      return Ok(TokenKind::AndEq);
    }

    Ok(TokenKind::Amp)
  }

  /// Lexes a pipe (`|`) or compound assignment (`|=`).
  ///
  /// Handles:
  /// - `|` - Bitwise OR or closure parameter
  /// - `|=` - Bitwise OR assignment
  ///
  /// Similar to ampersands, the lexer emits two `TokenKind::Or` tokens for `||`
  /// and leaves higher-level interpretation to the parser.
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::Or)` or `Ok(TokenKind::OrEq)`
  pub(crate) fn lex_or(&mut self) -> Result<TokenKind, ()> {
    if self.match_char('=') {
      return Ok(TokenKind::OrEq);
    }

    Ok(TokenKind::Or)
  }

  /// Lexes a caret (`^`) or compound assignment (`^=`).
  ///
  /// Used for bitwise XOR operations.
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::Caret)` or `Ok(TokenKind::CaretEq)`
  pub(crate) fn lex_caret(&mut self) -> Result<TokenKind, ()> {
    if self.match_char('=') {
      return Ok(TokenKind::CaretEq);
    }

    Ok(TokenKind::Caret)
  }
}
