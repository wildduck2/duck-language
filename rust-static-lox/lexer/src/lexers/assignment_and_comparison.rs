//! Lexers for assignment and comparison operators.
//!
//! Handles `=`, `==`, `!=`, `<`, `<=`, `>`, `>=`, `<<`, `>>`, and their variants.

use crate::{token::TokenKind, Lexer};

impl Lexer {
  /// Lexes an equals sign (`=`), equality (`==`), or fat arrow (`=>`).
  ///
  /// Handles:
  /// - `=` - Assignment
  /// - `==` - Equality comparison
  /// - `=>` - Match arm arrow
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::Eq)`, `Ok(TokenKind::EqEq)`, or `Ok(TokenKind::FatArrow)`
  pub fn lex_equal(&mut self) -> Result<TokenKind, ()> {
    if self.match_char('=') {
      return Ok(TokenKind::EqEq);
    } else if self.match_char('>') {
      return Ok(TokenKind::FatArrow);
    }

    Ok(TokenKind::Eq)
  }

  /// Lexes an exclamation mark (`!`) or inequality (`!=`).
  ///
  /// Handles:
  /// - `!` - Logical NOT
  /// - `!=` - Inequality comparison
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::Bang)` or `Ok(TokenKind::Ne)`
  pub fn lex_bang(&mut self) -> Result<TokenKind, ()> {
    if self.match_char('=') {
      return Ok(TokenKind::Ne);
    }

    Ok(TokenKind::Bang)
  }

  /// Lexes a less-than sign (`<`), less-or-equal (`<=`), or left shift assignment (`<<=`).
  ///
  /// Handles:
  /// - `<` - Less than comparison
  /// - `<=` - Less than or equal
  /// - `<<=` - Left shift assignment
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::Lt)`, `Ok(TokenKind::Le)`, or `Ok(TokenKind::ShiftLeftEq)`
  pub fn lex_less(&mut self) -> Result<TokenKind, ()> {
    if self.match_char('=') {
      return Ok(TokenKind::Le);
    } else if self.match_char('<') {
      if self.match_char('=') {
        return Ok(TokenKind::ShiftLeftEq);
      }
      self.current -= 1; // put back the '<' when it's just `<<`
    }

    Ok(TokenKind::Lt)
  }

  /// Lexes a greater-than sign (`>`), greater-or-equal (`>=`), or right shift (`>>`, `>>=`).
  ///
  /// Handles:
  /// - `>` - Greater than comparison
  /// - `>=` - Greater than or equal
  /// - `>>=` - Right shift assignment
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::Gt)`, `Ok(TokenKind::Ge)`, `Ok(TokenKind::ShiftRight)`, or `Ok(TokenKind::ShiftRightEq)`
  pub fn lex_greater(&mut self) -> Result<TokenKind, ()> {
    if self.match_char('=') {
      return Ok(TokenKind::Ge);
    } else if self.match_char('>') {
      if self.match_char('=') {
        return Ok(TokenKind::ShiftRightEq);
      }
      self.current -= 1; // revert the '>' char
    }

    Ok(TokenKind::Gt)
  }
}
