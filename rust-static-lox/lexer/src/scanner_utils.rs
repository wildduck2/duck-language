//! Scanner utilities for dispatching character-based lexing.
//!
//! This module contains the main dispatch logic that routes characters
//! to specialized lexer functions based on their type.

use diagnostic::Span;

use crate::{token::TokenKind, Lexer};

impl Lexer {
  /// Dispatches lexing based on the current character.
  ///
  /// Routes characters to appropriate lexer functions:
  /// - Punctuation and delimiters → specialized lexers
  /// - Operators → arithmetic/comparison/bitwise lexers
  /// - Digits → number lexer
  /// - Letters/underscore → keyword/identifier lexer
  /// - Quotes → string/character lexer
  /// - Whitespace → whitespace lexer
  ///
  /// Emits diagnostics for unexpected characters.
  ///
  /// # Arguments
  ///
  /// * `c` - The current character to lex
  /// * `engine` - Diagnostic engine for error reporting
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind)` for successfully lexed tokens, or `Err(())` after emitting
  /// a diagnostic for malformed input.
  pub(crate) fn lex_tokens(&mut self, c: char) -> Result<TokenKind, ()> {
    match c {
      // punctuation and delimiters
      ';' => self.lex_semicolon(),
      ',' => self.lex_comma(),
      '.' => self.lex_dot(),
      '(' => self.lex_open_paren(),
      ')' => self.lex_close_paren(),
      '{' => self.lex_open_brace(),
      '}' => self.lex_close_brace(),
      '[' => self.lex_open_bracket(),
      ']' => self.lex_close_bracket(),
      '@' => self.lex_at(),
      '#' => self.lex_pound(),
      '~' => self.lex_tilde(),
      '?' => self.lex_question(),
      ':' => self.lex_colon(),
      '$' => self.lex_dollar(),

      // Assignment & Comparison
      '=' => self.lex_equal(),
      '!' => self.lex_bang(),
      '<' => self.lex_less(),
      '>' => self.lex_greater(),

      // Arithmetic
      '+' => self.lex_plus(),
      '-' => self.lex_minus(),
      '*' => self.lex_star(),
      '/' => self.lex_slash(),
      '%' => self.lex_percent(),

      // Bitwise & Logical
      '&' => self.lex_and(),
      '|' => self.lex_or(),
      '^' => self.lex_caret(),

      // handle whitespace
      '\n' => {
        self.line += 1;
        self.column = 0;
        Ok(TokenKind::Whitespace)
      },
      '\r' | '\t' | ' ' => self.lex_whitespace(),

      // String and character literals
      '\'' => self.lex_string(), // Character literal

      // Handles b"", br"", b'c', etc.
      'b' if self.is_string_prefix('b') => self.lex_string(),

      // Handles c"", cr"", etc.
      'c' if self.is_string_prefix('c') => self.lex_string(),

      // Handles r"", r#"..."#, etc.
      'r' if self.is_string_prefix('r') => self.lex_string(),

      // Normal string
      '"' => self.lex_string(), // Regular string

      // Numbers
      '0'..='9' => self.lex_number(),
      // Keywords
      'A'..='Z' | 'a'..='z' | '_' => self.lex_keywords(),

      _ => {
        let span = Span::new(self.current, self.column + 1);
        self.emit_diagnostic(self.err_invalid_character(span, c));
        Err(())
      },
    }
  }

  /// Lexes whitespace characters (spaces, tabs, carriage returns).
  ///
  /// Consumes all consecutive whitespace characters into a single
  /// `Whitespace` token. Newlines are handled separately in the dispatch.
  ///
  /// # Returns
  ///
  /// `Ok(TokenKind::Whitespace)` after consuming all whitespace
  fn lex_whitespace(&mut self) -> Result<TokenKind, ()> {
    while let Some(c) = self.peek() {
      if c.is_whitespace() {
        self.advance();
      } else {
        break;
      }
    }
    Ok(TokenKind::Whitespace)
  }
}
