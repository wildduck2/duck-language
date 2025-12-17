//! Lexers for arithmetic operators.
//!
//! Handles `+`, `-`, `*`, `/`, `%` and their compound assignment variants.

use crate::{token::TokenKind, Lexer};

impl Lexer {
  /// Lexes a plus sign (`+`) or compound assignment (`+=`).
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::Plus)` or `Ok(TokenKind::PlusEq)`
  pub fn lex_plus(&mut self) -> Result<TokenKind, ()> {
    if self.match_char('=') {
      return Ok(TokenKind::PlusEq);
    }

    Ok(TokenKind::Plus)
  }

  /// Lexes a minus sign (`-`), compound assignment (`-=`), or thin arrow (`->`).
  ///
  /// Handles:
  /// - `-` - Subtraction or unary negation
  /// - `-=` - Subtraction assignment
  /// - `->` - Function return type arrow
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::Minus)`, `Ok(TokenKind::MinusEq)`, or `Ok(TokenKind::ThinArrow)`
  pub fn lex_minus(&mut self) -> Result<TokenKind, ()> {
    if self.match_char('=') {
      return Ok(TokenKind::MinusEq);
    } else if self.match_char('>') {
      return Ok(TokenKind::ThinArrow);
    }

    Ok(TokenKind::Minus)
  }

  /// Lexes a star (`*`) or compound assignment (`*=`).
  ///
  /// Used for multiplication or dereferencing.
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::Star)` or `Ok(TokenKind::StarEq)`
  pub fn lex_star(&mut self) -> Result<TokenKind, ()> {
    if self.match_char('=') {
      return Ok(TokenKind::StarEq);
    }

    Ok(TokenKind::Star)
  }

  /// Lexes a slash (`/`), compound assignment (`/=`), or comment (`//`, `/*`).
  ///
  /// Handles:
  /// - `/` - Division operator
  /// - `/=` - Division assignment
  /// - `//` - Line comment
  /// - `/*` - Block comment
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::Slash)`, `Ok(TokenKind::SlashEq)`, or comment token
  pub fn lex_slash(&mut self) -> Result<TokenKind, ()> {
    if self.match_char('=') {
      return Ok(TokenKind::SlashEq);
    } else if self.match_char('/') {
      return self.lex_line_comment();
    } else if self.match_char('*') {
      return self.lex_multi_line_comment();
    }

    Ok(TokenKind::Slash)
  }

  /// Lexes a percent sign (`%`) or compound assignment (`%=`).
  ///
  /// Used for modulo operations.
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::Percent)` or `Ok(TokenKind::PercentEq)`
  pub fn lex_percent(&mut self) -> Result<TokenKind, ()> {
    if self.match_char('=') {
      return Ok(TokenKind::PercentEq);
    }

    Ok(TokenKind::Percent)
  }
}
