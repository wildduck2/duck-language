//! Lexer module for tokenizing source code.
//!
//! The lexer converts raw source text into a stream of tokens, handling:
//! - Keywords, identifiers, and literals
//! - Operators and punctuation
//! - Comments and whitespace
//! - String and character literals with escape sequences
//! - Numeric literals in various bases (decimal, binary, octal, hexadecimal)
//!
//! # Architecture
//!
//! The lexer uses a cursor-based approach with byte offsets for accurate
//! UTF-8 handling. It maintains state for the current position (`current`),
//! token start position (`start`), and line/column tracking.

use diagnostic::{DiagnosticEngine, SourceFile, Span};

use crate::token::{LiteralKind, Token, TokenKind};

mod lexers;
mod scanner_utils;
pub mod token;

/// Lexer for converting source code into tokens.
///
/// Maintains position tracking and emits tokens with source spans.
#[derive(Debug)]
pub struct Lexer {
  pub source: SourceFile,
  pub tokens: Vec<Token>,
  pub start: usize,   // Start byte offset of current token
  pub current: usize, // Current byte offset in source
  pub line: usize,    // Current line (1-indexed)
  pub column: usize,  // Current column (1-indexed)
}

impl Lexer {
  /// Creates a new lexer for the given source file.
  ///
  /// Initializes all position counters to zero. The lexer is ready
  /// to tokenize the source after construction.
  ///
  /// # Arguments
  ///
  /// * `source` - The source file to tokenize
  ///
  /// # Examples
  ///
  /// ```rust
  /// let source = SourceFile::new("main.rs".to_string(), "fn main() {}".to_string());
  /// let mut lexer = Lexer::new(source);
  /// ```
  pub fn new(source: SourceFile) -> Self {
    Self {
      source,
      tokens: Vec::new(),
      start: 0,
      current: 0,
      line: 0,
      column: 0,
    }
  }

  /// Tokenizes the entire source file, producing a stream of tokens.
  ///
  /// Processes characters sequentially, delegating to specialized lexer
  /// functions based on the current character. Emits diagnostics for
  /// invalid characters or malformed tokens.
  ///
  /// # Arguments
  ///
  /// * `engine` - Diagnostic engine for reporting errors and warnings
  ///
  /// # Examples
  ///
  /// ```rust
  /// let mut engine = DiagnosticEngine::new();
  /// lexer.scan_tokens(&mut engine);
  /// // lexer.tokens now contains all tokens from the source
  /// ```
  pub fn scan_tokens(&mut self, engine: &mut DiagnosticEngine) {
    while !self.is_eof() {
      self.start = self.current;
      let c = self.advance();

      let token = self.lex_tokens(c, engine);

      if let Ok(token_type) = token {
        self.emit(token_type);
      };
    }

    self.emit(TokenKind::Eof);
  }

  /// Conditionally consumes the next character if it matches the expected character.
  ///
  /// This is a lookahead operation: if the next character matches, it is consumed
  /// and `true` is returned. Otherwise, the cursor is unchanged and `false` is returned.
  ///
  /// # Arguments
  ///
  /// * `match_char` - The character to match against
  ///
  /// # Returns
  ///
  /// `true` if the character matched and was consumed, `false` otherwise
  ///
  /// # Examples
  ///
  /// ```rust
  /// // If source is "==", after match_char('='):
  /// // - First call: matches '=', consumes it, returns true
  /// // - Second call: matches '=', consumes it, returns true
  /// ```
  fn match_char(&mut self, match_char: char) -> bool {
    if let Some(char) = self.peek() {
      if char == match_char {
        self.advance();
        return true;
      }
    }
    false
  }

  /// Emits a token with a span covering the text from `start` to `current`.
  ///
  /// Trivia tokens (whitespace, comments) are filtered out and not added
  /// to the token stream. After emitting, `start` is updated to `current`.
  ///
  /// # Arguments
  ///
  /// * `kind` - The kind of token to emit
  ///
  /// # Examples
  ///
  /// ```rust
  /// // After lexing "fn", start=0, current=2:
  /// lexer.emit(TokenKind::KwFn);
  /// // Token added with span (0, 2)
  /// // start is now 2
  /// ```
  fn emit(&mut self, kind: TokenKind) {
    // ignore comments
    if kind.is_trivia() {
      return;
    }

    let span = match &kind {
      TokenKind::Literal { kind } => match &kind {
        LiteralKind::Char => Span {
          start: self.start + 1,
          end: self.current - 1,
        },
        LiteralKind::Str => Span {
          start: self.start + 1,
          end: self.current - 1,
        },
        LiteralKind::CStr => Span {
          start: self.start + 2,
          end: self.current - 1,
        },
        LiteralKind::RawStr { n_hashes } => Span {
          start: self.start + 2 + n_hashes,
          end: self.current - 1 - n_hashes,
        },
        LiteralKind::RawByteStr { n_hashes } => Span {
          start: self.start + 3 + n_hashes,
          end: self.current - 1 - n_hashes,
        },
        LiteralKind::RawCStr { n_hashes } => Span {
          start: self.start + 3 + n_hashes,
          end: self.current - 1 - n_hashes,
        },
        _ => Span {
          start: self.start,
          end: self.current,
        },
      },
      _ => Span {
        start: self.start,
        end: self.current,
      },
    };

    self.tokens.push(Token { kind, span });
    self.start = self.current;
  }

  /// Returns the next character without consuming it.
  ///
  /// This is a pure lookahead operation that does not advance the cursor.
  /// Returns `None` if at end of file.
  ///
  /// # Returns
  ///
  /// `Some(char)` if a character is available, `None` at EOF
  ///
  /// # Examples
  ///
  /// ```rust
  /// // If source is "abc" and current=0:
  /// lexer.peek() // Some('a')
  /// lexer.current // still 0
  /// ```
  fn peek(&self) -> Option<char> {
    if self.is_eof() {
      return None;
    }

    let char = self.source.src[self.current..].chars().next().unwrap();

    Some(char)
  }

  /// Returns a character at a specified offset from the current position.
  ///
  /// Useful for multi-character lookahead (e.g., checking if `//` starts a comment).
  /// Does not advance the cursor.
  ///
  /// # Arguments
  ///
  /// * `offset` - Byte offset from current position (0 = current char, 1 = next char)
  ///
  /// # Returns
  ///
  /// `Some(char)` if available at that offset, `None` otherwise
  ///
  /// # Examples
  ///
  /// ```rust
  /// // If source is "abc" and current=0:
  /// lexer.peek_next(0) // Some('a')
  /// lexer.peek_next(1) // Some('b')
  /// lexer.peek_next(2) // Some('c')
  /// ```
  fn peek_next(&self, offset: usize) -> Option<char> {
    if self.is_eof() {
      return None;
    }

    self.source.src[(self.current + offset)..].chars().next()
  }

  /// Advances the cursor by one character and returns it.
  ///
  /// Handles UTF-8 correctly by computing the byte offset of the next character.
  /// Updates `current` (byte offset) and `column` (character column) accordingly.
  ///
  /// # Returns
  ///
  /// The consumed character, or `'\0'` if at EOF
  ///
  /// # Examples
  ///
  /// ```rust
  /// // If source is "abc" and current=0:
  /// lexer.advance() // 'a', current=1, column=1
  /// lexer.advance() // 'b', current=2, column=2
  /// ```
  fn advance(&mut self) -> char {
    if self.is_eof() {
      return '\0';
    }

    // get remaining string slice
    let remaining = &self.source.src[self.current..];
    let mut iter = remaining.char_indices();

    // the first character and its byte offset (always 0)
    let (_, ch) = iter.next().unwrap();

    // compute byte offset of next character (to move current forward)
    if let Some((next_byte_idx, _)) = iter.next() {
      self.current += next_byte_idx;
    } else {
      self.current = self.source.src.len();
    }

    // update column count
    self.column += 1;

    ch
  }

  /// Returns the text slice for the current token being lexed.
  ///
  /// The slice spans from `start` (inclusive) to `current` (exclusive).
  /// Returns an empty string if the range is invalid.
  ///
  /// # Returns
  ///
  /// A string slice of the current token's text
  ///
  /// # Examples
  ///
  /// ```rust
  /// // After lexing "fn" (start=0, current=2):
  /// lexer.get_current_lexeme() // "fn"
  /// ```
  fn get_current_lexeme(&self) -> &str {
    self.source.src.get(self.start..self.current).unwrap_or("")
  }

  /// Checks if the cursor has reached the end of the source file.
  ///
  /// # Returns
  ///
  /// `true` if `current >= source.len()`, `false` otherwise
  fn is_eof(&self) -> bool {
    self.current >= self.source.src.len()
  }

  /// Retrieves a specific line from the source file.
  ///
  /// # Arguments
  ///
  /// * `line_num` - Zero-indexed line number
  ///
  /// # Returns
  ///
  /// The line content as a `String`, or empty string if out of range
  ///
  /// # Examples
  ///
  /// ```rust
  /// // For source "line1\nline2\nline3":
  /// lexer.get_line(0) // "line1"
  /// lexer.get_line(1) // "line2"
  /// ```
  pub fn get_line(&self, line_num: usize) -> String {
    self
      .source
      .src
      .lines()
      .nth(line_num)
      .unwrap_or("")
      .to_string()
  }
}
