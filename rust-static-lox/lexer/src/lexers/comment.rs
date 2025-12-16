//! Lexers for comments.
//!
//! Handles both line comments (`//`) and block comments (`/* */`),
//! including documentation comments (`///`, `//!`, `/** */`, `/*! */`).

use crate::{
  token::{DocStyle, TokenKind},
  Lexer,
};

impl Lexer {
  /// Lexes a line comment (`//` or `///` or `//!`).
  ///
  /// Consumes all characters until a newline or EOF. Detects documentation
  /// comment styles:
  /// - `///` - Outer documentation comment
  /// - `//!` - Inner documentation comment
  ///
  /// # Returns
  ///
  /// `Some(TokenKind::LineComment { doc_style })`
  pub fn lex_line_comment(&mut self) -> Result<TokenKind, ()> {
    let doc_style = if self.match_char('/') {
      Some(DocStyle::Inner)
    } else if self.match_char('!') {
      Some(DocStyle::Outer)
    } else {
      None
    };

    while !self.is_eof() {
      if self.peek() == Some('\n') {
        break;
      }
      self.advance(); // consume the current char
    }
    Ok(TokenKind::LineComment { doc_style })
  }

  /// Lexes a block comment (`/* */` or `/** */` or `/*! */`).
  ///
  /// Handles nested comments and detects documentation comment styles:
  /// - `/** */` - Outer documentation comment
  /// - `/*! */` - Inner documentation comment
  ///
  /// Tracks nesting depth to correctly handle nested block comments.
  ///
  /// # Returns
  ///
  /// `Some(TokenKind::BlockComment { doc_style, terminated })`
  ///
  /// The `terminated` field is `false` if the closing `*/` is missing.
  pub fn lex_multi_line_comment(&mut self) -> Result<TokenKind, ()> {
    // Detect Rust-style doc comments: /*! ... */ (Outer) or /** ... */ (Inner)
    let doc_style = match self.peek() {
      Some('!') => {
        self.advance(); // consume '!'
        Some(DocStyle::Outer)
      },
      Some('*') => {
        self.advance(); // consume second '*'
        Some(DocStyle::Inner)
      },
      _ => None,
    };

    let mut terminated = false;
    let mut depth = 1; // track nested comment depth

    while !self.is_eof() {
      let current = self.peek();
      let next = self.peek_next(1);

      // Handle newlines
      if current == Some('\n') {
        self.line += 1;
      }

      // Detect nested comment start "/*"
      if current == Some('/') && next == Some('*') {
        self.advance(); // consume '/'
        self.advance(); // consume '*'
        depth += 1;
        continue;
      }

      // Detect comment end "*/"
      if current == Some('*') && next == Some('/') {
        self.advance(); // consume '*'
        self.advance(); // consume '/'
        depth -= 1;

        if depth == 0 {
          terminated = true;
          break;
        }
        continue;
      }

      self.advance(); // consume any other char
    }

    Ok(TokenKind::BlockComment {
      doc_style,
      terminated,
    })
  }
}
