//! Lexers for bitwise and logical operators.
//!
//! Handles `&`, `|`, `^` and their compound assignment and logical variants.

use crate::{token::TokenKind, Lexer};

impl Lexer {
  /// Lexes an ampersand (`&`), logical AND (`&&`), or compound assignment (`&=`).
  ///
  /// Handles:
  /// - `&` - Bitwise AND or borrow
  /// - `&&` - Logical AND
  /// - `&=` - Bitwise AND assignment
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::And)`, `Ok(TokenKind::AndAnd)`, or `Ok(TokenKind::AndEq)`
  pub fn lex_and(&mut self) -> Result<TokenKind, ()> {
    if self.match_char('=') {
      return Ok(TokenKind::AndEq);
    }

    Ok(TokenKind::And)
  }

  /// Lexes a pipe (`|`), logical OR (`||`), or compound assignment (`|=`).
  ///
  /// Handles:
  /// - `|` - Bitwise OR or closure parameter
  /// - `||` - Logical OR
  /// - `|=` - Bitwise OR assignment
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::Or)`, `Ok(TokenKind::OrOr)`, or `Ok(TokenKind::OrEq)`
  pub fn lex_or(&mut self) -> Result<TokenKind, ()> {
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
  pub fn lex_caret(&mut self) -> Result<TokenKind, ()> {
    if self.match_char('=') {
      return Ok(TokenKind::CaretEq);
    }

    Ok(TokenKind::Caret)
  }
}
