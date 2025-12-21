//! # String, Raw String, Byte String, C-String, and Char Literal Rules
//!
//! This lexer implements literal syntax following Rust's rules
//! (plus optional language extensions noted where present).
//!
//! ## 1. Normal Strings: `"..."`
//! - Content is UTF-8 text.
//! - Allowed escapes:
//!   - Simple: `\\`, `\"`, `\n`, `\r`, `\t`, `\0`
//!   - Hex byte: `\xNN` (exactly 2 hex digits, value ≤ 0x7F)
//!   - Unicode: `\u{H…H}` (1–6 hex digits, valid scalar ≤ `0x10FFFF` and not a surrogate)
//! - **Line continuation**: `\<LF><WS*>` is removed (produces no characters).
//! - **LF allowed**, **bare CR forbidden** except in continuation.
//! - Terminates only on unescaped `"`. Multi-line allowed.
//!
//! ## 2. Raw Strings: `r"..."`, `r#"..."#`, `r##"..."##`, …
//! - Prefix: `r` + zero or more `#` characters.
//! - Must have opening `"` immediately after the `#` run.
//! - No escapes are processed—content is literal bytes/UTF-8 text.
//! - Closing delimiter is `"` followed by exactly the same number of `#`.
//! - Any characters allowed inside, including newlines and CR.
//!
//! ## 3. Byte Strings: `b"..."`
//! - Content must be **ASCII only** (0x00–0x7F), except via escapes.
//! - Allowed escapes:
//!   - Simple: `\\`, `\"`, `\n`, `\r`, `\t`, `\0`
//!   - Hex: `\xNN` (two hex digits, produces one byte)
//! - **Unicode escapes (`\u{}`) are forbidden.**
//! - **Line continuation**: same as normal strings (`\<LF><WS*>`).
//! - **LF allowed**, **bare CR forbidden** except in continuation.
//!
//! ## 4. Raw Byte Strings: `br"..."`, `br#"..."#`, …
//! - Same rules as raw strings (`r#"... "#`), but produces a byte slice.
//! - No escapes; interior bytes must be valid ASCII (Rust requires ASCII-only).
//!
//! ## 5. C Strings: `c"..."`
//! - Behaves like a normal string literal but intended to map to C string data.
//! - Same escapes as normal `"..."` literals (`\n`, `\t`, `\xNN`, `\u{}`).
//! - **Line continuation** supported (`\<LF><WS*>`).
//! - **LF allowed**, **bare CR forbidden** except in continuation.
//! - Semantic note (not enforced purely lexically): interior `\0` is usually undesirable.
//!
//! ## 6. Raw C Strings: `cr"..."`, `cr#"..."#`, …
//! - Same as raw strings, no escapes.
//! - Closing delimiter uses same `#` count.
//!
//! ## 7. Char Literals: `'x'`, `'\n'`, `'\xNN'`, `'\u{1F980}'`
//! - Must represent exactly **one Unicode scalar value**.
//! - Allowed escapes: same as normal strings (except continuation).
//!   - `\\`, `\'`, `\n`, `\r`, `\t`, `\0`, `\xNN`, `\u{H…}`
//! - Cannot contain newlines; literal must close on next `'`.
//! - Otherwise interpreted as a **lifetime** (e.g. `'a`, `'static`).
//!
//! ## 8. Byte Char Literals: `b'X'`, `b'\n'`, `b'\xNN'`
//! - Must encode exactly **one byte** (ASCII).
//! - Allowed escapes:
//!   - `\\`, `\'`, `\n`, `\r`, `\t`, `\0`, `\xNN`
//! - **Unicode escapes forbidden.**
//! - No continuation escapes.
//!
//! ## 9. Literal Suffixes
//! - Rust's lexer accepts identifier-like suffixes on all literals
//!   (e.g. `"foo"bar`, `b"data"_tag`).
//! - This lexer may optionally reject or ignore suffixes depending on language design.
//!
//! ## 10. Prefix Recognition
//! - Valid literal prefixes: `"`, `r`, `b`, `br`, `c`, `cr`.
//! - Unknown/reserved prefixes (`f`, `cf`, `rf`, etc.) may produce lexer diagnostics
//!   in this implementation (extension). Rust would treat them as identifiers.
//!

use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError, Span};

use crate::{
  token::{LiteralKind, TokenKind},
  Lexer,
};

impl Lexer {
  /// Dispatch string-like and character-like literals based on the current prefix.
  ///
  /// Routes (roughly matching Rust):
  /// - `"`          -> normal string        (`"..."`)
  /// - `b"` / `br`  -> byte / raw byte str  (`b"..."`, `br#"..."#`)
  /// - `c"` / `cr`  -> C / raw C string     (`c"..."`, `cr#"..."#`)
  /// - `r` + `#*`   -> raw string           (`r"..."`, `r#"..."#`)
  /// - `'`          -> character / lifetime (delegates to `lex_char`)
  ///
  /// For prefixes like `f`, `cf`, `rf`, we emit “reserved / unknown prefix”
  /// diagnostics (this is a language extension; Rust itself would just lex
  /// identifiers in those cases).
  pub(crate) fn lex_string(&mut self) -> Result<TokenKind, ()> {
    let first = self.get_current_lexeme(); // e.g. "b", "c", "r", "\"", or "'"
    let second = self.peek(); // next char, e.g. 'r', '"', etc.

    // Character / lifetime literals are handled separately.
    if first == "'" {
      return self.lex_char();
    }

    // Combine prefix (1–2 chars, e.g. "b", "br", "c", "cr").
    let mut prefix = first.to_string();
    if let Some(ch) = second {
      if ch.is_ascii_alphabetic() {
        prefix.push(ch);
      }
    }

    // Language-level prefixes we recognize.
    const VALID_PREFIXES: &[&str] = &["b", "br", "c", "cr", "r", "\""];
    const RESERVED_PREFIXES: &[&str] = &["f", "cf", "rf"];

    // Prefix validation only for potentially prefixed literals (`b`, `c`, `r`, etc.).
    if first != "\"" {
      if RESERVED_PREFIXES.contains(&prefix.as_str()) {
        let span = diagnostic::Span::new(self.start, self.current);
        self.emit_diagnostic(self.err_reserved_prefix(span, &prefix));
        return Err(());
      }

      if !VALID_PREFIXES.contains(&prefix.as_str()) {
        let span = diagnostic::Span::new(self.start, self.current);
        self.emit_diagnostic(self.err_unknown_prefix(span, &prefix));
        return Err(());
      }
    }

    // Dispatch to the right literal lexer. At this point:
    // - `self.current` is just after the first char of the prefix (`b`, `c`, `r`, or `"`),
    // - `second` is the next character.
    match (first, second) {
      ("b", Some('"')) => self.lex_bstr(),     // b"..."
      ("b", Some('r')) => self.lex_bstr(),     // br"..."
      ("b", Some('\'')) => self.lex_bchar(),   // b'X'
      ("c", Some('"')) => self.lex_cstr(),     // c"..."
      ("c", Some('r')) => self.lex_craw_str(), // cr"..."
      ("r", Some('"')) | ("r", Some('#')) => self.lex_raw_str(), // r"..." or r#"..."#
      ("\"", _) => self.lex_str(),             // "..."
      _ => Err(()),
    }
  }

  /// Lex a **raw C string**: `cr"..."`, `cr#"...\"..."#`, etc.
  ///
  /// Counts `#` fences, requires an opening `"`, then scans until a matching
  /// closing `"###…###`. No escapes are processed.
  fn lex_craw_str(&mut self) -> Result<TokenKind, ()> {
    // The 'c' prefix has been consumed; we're currently at 'r'.
    self.advance(); // consume 'r'

    const MAX_HASHES: usize = 255;

    // We're now at the first `#` or `"` character.
    let mut n_hashes: usize = 0;
    while self.peek() == Some('#') {
      n_hashes = n_hashes.saturating_add(1);
      self.advance();
    }

    if n_hashes > MAX_HASHES {
      let span = Span::new(self.start, self.current);
      self.emit_diagnostic(self.err_too_many_raw_str_hashes(span, n_hashes, MAX_HASHES));
      n_hashes = MAX_HASHES;
    }

    // Expect opening quote after `cr###`.
    if self.peek() != Some('"') {
      let span = Span::new(self.start, self.current);
      self.emit_diagnostic(self.err_invalid_string_start(span, "expected '\"' after raw C string prefix"));
      return Err(());
    }

    // Consume opening quote.
    self.advance();

    let mut found_end = false;

    // Scan until we see a closing `"` followed by exactly `n_hashes` `#`.
    'outer: while let Some(c) = self.peek() {
      if c == '\r' {
        let span = Span::new(self.current, self.current + 1);
        self.emit_diagnostic(self.err_invalid_escape(span, "\\r", Some("raw C string literals")));
        self.advance();
        return Err(());
      }

      if c == '\0' {
        self.report_nul_in_string(
          "raw C string",
          diagnostic::Span::new(self.current, self.current + 1),
        );
        self.advance();
        return Err(());
      }

      if c == '"' {
        let saved = self.current;
        self.advance(); // consume quote

        let mut matched = 0usize;
        while matched < n_hashes && self.peek() == Some('#') {
          matched += 1;
          self.advance();
        }
        if matched == n_hashes {
          found_end = true;
          break 'outer;
        } else {
          // Not a real closing delimiter; reset to one past the quote.
          self.current = saved + 1;
        }
      } else {
        self.advance();
      }
    }

    if !found_end {
      let span = Span::new(self.start, self.current);
      self.emit_diagnostic(self.err_unterminated_string(span, "raw C string"));
    }

    Ok(TokenKind::Literal {
      kind: LiteralKind::RawCStr { n_hashes },
    })
  }

  /// Lex a **raw string**: `r"..."`, `r#"...\"..."#`, multi-line allowed.
  ///
  /// Counts `#` fences, requires an opening `"`, then scans until a matching
  /// closing `"###…###`. Escapes are not processed.
  fn lex_raw_str(&mut self) -> Result<TokenKind, ()> {
    const MAX_HASHES: usize = 255;

    // We're just after the 'r' and at zero or more '#'.
    let mut n_hashes: usize = 0;
    while self.peek() == Some('#') {
      n_hashes = n_hashes.saturating_add(1);
      self.advance();
    }

    if n_hashes > MAX_HASHES {
      let span = Span::new(self.start, self.current);
      self.emit_diagnostic(self.err_too_many_raw_str_hashes(span, n_hashes, MAX_HASHES));
      n_hashes = MAX_HASHES;
    }

    // Expect opening quote `"` after `r###`.
    if self.peek() != Some('"') {
      let span = Span::new(self.start, self.current);
      self.emit_diagnostic(self.err_invalid_string_start(span, "expected '\"' after raw string prefix"));
      return Err(());
    }

    // Consume opening quote.
    self.advance();
    let mut found_end = false;

    // Scan until we find the closing delimiter: '"' + n_hashes of '#'.
    'outer: while let Some(c) = self.peek() {
      // Lookahead for probable next-line raw prefix to improve recovery
      // (this is a non-Rust extension for nicer diagnostics).
      if c == '\r' {
        let span = Span::new(self.current, self.current + 1);
        self.emit_diagnostic(self.err_invalid_escape(span, "\\r", Some("raw string literals")));
        self.advance();
        return Err(());
      }

      if c == '\n' {
        let saved = self.current;

        self.advance(); // consume '\n'

        while matches!(self.peek(), Some(' ' | '\t')) {
          self.advance();
        }

        let mut looks_like_raw_prefix = false;
        if self.peek() == Some('r') {
          self.advance(); // consume 'r'
          while self.peek() == Some('#') {
            self.advance();
          }
          looks_like_raw_prefix = self.peek() == Some('"');
        }

        self.current = saved;

        if looks_like_raw_prefix {
          break 'outer;
        } else {
          self.advance(); // consume '\n'
          continue;
        }
      }

      if c == '"' {
        let saved = self.current;
        self.advance(); // consume '"'

        let mut matched: usize = 0;
        while matched < n_hashes && self.peek() == Some('#') {
          matched += 1;
          self.advance();
        }

        if matched == n_hashes {
          found_end = true;
          break 'outer;
        } else {
          self.current = saved + 1;
        }
      } else {
        self.advance();
      }
    }

    if !found_end {
      let span = Span::new(self.start, self.current);
      self.emit_diagnostic(self.err_unterminated_string(span, "raw string"));
      return Err(());
    }

    Ok(TokenKind::Literal {
      kind: LiteralKind::RawStr { n_hashes },
    })
  }

  /// Lex a **C string**: `c"..."`.
  ///
  /// Semantics mirror Rust's non-raw strings:
  /// - Supports `\\`, `\"`, `\n`, `\r`, `\t`, `\0`, `\xNN` (ASCII), `\u{...}`.
  /// - Supports line-continuation escapes: `\` + LF + following whitespace.
  /// - Allows LF inside the literal, but rejects bare CR outside continuation.
  fn lex_cstr(&mut self) -> Result<TokenKind, ()> {
    // The 'c' prefix has been consumed; we're at the opening '"'.
    self.advance(); // consume '"'

    let terminated = self.scan_string_body("C string", true, true);

    if !terminated {
      let span = Span::new(self.start, self.current);
      self.emit_diagnostic(self.err_unterminated_string(span, "C string"));
      return Err(());
    }

    Ok(TokenKind::Literal {
      kind: LiteralKind::CStr,
    })
  }

  /// Lex a byte string: `b"..."` or raw byte string: `br#"..."#`.
  ///
  /// For `b"..."`:
  /// - Only ASCII bytes are allowed.
  /// - Escapes: `\\`, `\"`, `\n`, `\r`, `\t`, `\0`, `\xNN` (any byte).
  /// - Line continuation: `\` + LF + following whitespace is removed.
  ///
  /// For `br...`:
  /// - Acts like a raw string; bytes are taken verbatim.
  fn lex_bstr(&mut self) -> Result<TokenKind, ()> {
    // The 'b' prefix has already been consumed.
    if self.peek() == Some('r') {
      // --- RAW BYTE STRING: br"..." or br#"..."# ---
      self.advance(); // consume 'r'

      const MAX_HASHES: usize = 255;

      let mut n_hashes: usize = 0;
      while self.peek() == Some('#') {
        n_hashes = n_hashes.saturating_add(1);
        self.advance();
      }

      if n_hashes > MAX_HASHES {
        let span = Span::new(self.start, self.current);
        self.emit_diagnostic(self.err_too_many_raw_str_hashes(span, n_hashes, MAX_HASHES));
        n_hashes = MAX_HASHES;
      }

      if self.peek() != Some('"') {
        let span = Span::new(self.start, self.current);
        self.emit_diagnostic(self.err_invalid_string_start(span, "expected '\"' after raw byte string prefix"));
        return Ok(TokenKind::Literal {
          kind: LiteralKind::RawByteStr { n_hashes },
        });
      }

      self.advance(); // consume opening quote

      let mut found_end = false;
      'outer: while let Some(c) = self.peek() {
        if c == '\r' {
          let span = Span::new(self.current, self.current + 1);
          self.emit_diagnostic(self.err_invalid_escape(span, "\\r", Some("raw byte string literals")));
          self.advance();
          return Err(());
        }

        if !c.is_ascii() {
          let span = Span::new(self.current, self.current + c.len_utf8());
          self.emit_diagnostic(self.err_invalid_escape(span, &c.to_string(), Some("raw byte strings must contain only ASCII")));
          self.advance();
          return Err(());
        }

        if c == '"' {
          let saved = self.current;
          self.advance();
          let mut matched = 0usize;
          while matched < n_hashes && self.peek() == Some('#') {
            matched += 1;
            self.advance();
          }
          if matched == n_hashes {
            found_end = true;
            break 'outer;
          } else {
            self.current = saved + 1;
          }
        } else {
          self.advance();
        }
      }

      if !found_end {
        let span = Span::new(self.start, self.current);
        self.emit_diagnostic(self.err_unterminated_string(span, "raw byte string"));
      }

      return Ok(TokenKind::Literal {
        kind: LiteralKind::RawByteStr { n_hashes },
      });
    }

    // --- NORMAL BYTE STRING: b"..." ---
    if self.peek() != Some('"') {
      let span = Span::new(self.start, self.current);
      self.emit_diagnostic(self.err_invalid_string_start(span, "expected '\"' after 'b' in byte string literal"));
      return Err(());
    }

    self.advance(); // consume opening quote
    let mut terminated = false;

    while let Some(c) = self.peek() {
      self.advance();

      match c {
        '\\' => {
          // Escapes inside b"..."
          match self.peek() {
            // Line continuation: remove backslash + LF + following whitespace.
            Some('\n') => {
              self.advance(); // consume newline
              while matches!(self.peek(), Some(' ' | '\t' | '\n' | '\r')) {
                self.advance();
              }
            },
            Some('n') | Some('r') | Some('t') | Some('\\') | Some('"') | Some('\'') | Some('0') => {
              self.advance(); // simple escape
            },
            Some('x') => {
              self.advance(); // consume 'x'
              let mut count = 0;
              while count < 2
                && self
                  .peek()
                  .map(|ch| ch.is_ascii_hexdigit())
                  .unwrap_or(false)
              {
                self.advance();
                count += 1;
              }
              if count < 2 {
                let span = Span::new(self.current.saturating_sub(2), self.current);
                self.emit_diagnostic(self.err_invalid_escape(span, "\\x", Some("hex escape must have exactly two hex digits")));
                return Err(());
              }
            },
            Some('u') if self.peek_next(1) == Some('{') => {
              let span = Span::new(self.current.saturating_sub(1), self.current + 1);
              self.emit_diagnostic(self.err_invalid_escape(span, "\\u", Some("unicode escapes are not allowed in byte strings")));
              return Err(());
            },
            _ => {
              let escape_char = self.peek().map(|c| c.to_string()).unwrap_or_else(|| "unknown".to_string());
              let span = Span::new(self.current.saturating_sub(1), self.current);
              self.emit_diagnostic(self.err_invalid_escape(span, &escape_char, Some("byte string")));
              if self.peek().is_some() {
                self.advance();
              }
              return Err(());
            },
          }
        },
        '"' => {
          terminated = true;
          break;
        },
        '\r' => {
          let span = Span::new(self.current.saturating_sub(1), self.current);
          self.emit_diagnostic(self.err_invalid_escape(span, "\\r", Some("byte string literals")));
          return Err(());
        },
        _ => {
          if !c.is_ascii() {
            let span = Span::new(self.current.saturating_sub(1), self.current);
            self.emit_diagnostic(self.err_invalid_escape(span, &c.to_string(), Some("byte strings must contain only ASCII characters")));
            return Err(());
          }
        },
      }
    }

    if !terminated {
      let span = Span::new(self.start, self.current);
      self.emit_diagnostic(self.err_unterminated_string(span, "byte string"));
      return Err(());
    }

    Ok(TokenKind::Literal {
      kind: LiteralKind::ByteStr,
    })
  }

  /// Lex a **byte character**: `b'X'` or escaped (`b'\xNN'`).
  ///
  /// - Must encode exactly one byte.
  /// - Content must be ASCII.
  /// - Allowed escapes: `\\`, `\'`, `\n`, `\r`, `\t`, `\0`, `\xNN`.
  fn lex_bchar(&mut self) -> Result<TokenKind, ()> {
    // We are just after 'b'; expect opening `'`.
    if self.peek() != Some('\'') {
      let span = Span::new(self.start, self.current);
      self.emit_diagnostic(self.err_invalid_string_start(span, "expected '\'' after 'b' in byte char literal"));
      return Err(());
    }

    self.advance(); // consume opening '\''

    let mut terminated = false;
    let mut produced_bytes: u8 = 0;

    while let Some(c) = self.peek() {
      match c {
        '\'' => {
          self.advance();
          terminated = true;
          break;
        },
        '\\' => {
          self.advance(); // consume '\'
          match self.peek() {
            Some('n' | 'r' | 't' | '\\' | '\'' | '0') => {
              self.advance();
              produced_bytes = produced_bytes.saturating_add(1);
            },
            Some('x') => {
              self.advance(); // consume 'x'
              let mut count = 0;
              while count < 2
                && self
                  .peek()
                  .map(|ch| ch.is_ascii_hexdigit())
                  .unwrap_or(false)
              {
                self.advance();
                count += 1;
              }
              if count < 2 {
                let span = Span::new(self.current.saturating_sub(2), self.current);
                self.emit_diagnostic(self.err_invalid_escape(span, "\\x", Some("hex escape must have exactly two hex digits")));
              } else {
                produced_bytes = produced_bytes.saturating_add(1);
              }
            },
            Some('u') if self.peek_next(1) == Some('{') => {
              let span = Span::new(self.current.saturating_sub(1), self.current + 1);
              self.emit_diagnostic(self.err_invalid_escape(span, "\\u", Some("unicode escapes are not allowed in byte char literals")));

              // Recovery: consume until '}' or newline.
              self.advance(); // 'u'
              self.advance(); // '{'
              while let Some(ch) = self.peek() {
                self.advance();
                if ch == '}' || ch == '\n' {
                  break;
                }
              }
            },
            Some('\n') => {
              let span = Span::new(self.current.saturating_sub(1), self.current);
              self.emit_diagnostic(self.err_invalid_escape(span, "\\", Some("line continuation escapes are not supported in byte char literals")));
              self.advance();
            },
            _ => {
              let escape_char = self.peek().map(|c| c.to_string()).unwrap_or_else(|| "unknown".to_string());
              let span = Span::new(self.current.saturating_sub(1), self.current);
              self.emit_diagnostic(self.err_invalid_escape(span, &escape_char, Some("byte char literal")));
              if self.peek().is_some() {
                self.advance();
              }
            },
          }
        },
        '\n' | '\r' => {
          // Unterminated literal.
          break;
        },
        _ => {
          if !c.is_ascii() {
            let span = Span::new(self.current, self.current + c.len_utf8());
            self.emit_diagnostic(self.err_invalid_escape(span, &c.to_string(), Some("byte char literals must be ASCII")));
          }
          self.advance();
          produced_bytes = produced_bytes.saturating_add(1);
        },
      }
    }

    if !self.get_current_lexeme().ends_with('\'') || !terminated {
      let span = Span::new(self.start, self.current);
      self.emit_diagnostic(self.err_unterminated_string(span, "byte char"));
      return Err(());
    }

    if produced_bytes == 0 {
      let span = Span::new(self.start, self.current);
      self.emit_diagnostic(self.err_empty_char(span));
      return Err(());
    }

    if produced_bytes > 1 {
      let span = Span::new(self.start, self.current);
      self.emit_diagnostic(self.err_invalid_literal(span, "byte char literal must encode exactly one byte"));
      return Err(());
    }

    Ok(TokenKind::Literal {
      kind: LiteralKind::Byte,
    })
  }

  /// Lex a **character literal**: `'x'`, `'\n'`, `'\x7F'`, `'\u{1F980}'`.
  ///
  /// - Validates escapes and reports errors for unterminated forms or bad escapes.
  /// - If no closing `'` is found, falls back to `lex_lifetime` (`'a`, `'static`, etc.).
  /// - Ensures there is at most one Unicode scalar value (or a single escape).
  fn lex_char(&mut self) -> Result<TokenKind, ()> {
    let mut terminated = false;

    // The opening `'` has already been consumed by the caller (`lex_string`),
    // so we start at the first character *after* the quote.
    while let Some(c) = self.peek() {
      match c {
        '\'' => {
          self.advance();
          terminated = true;
          break;
        },
        '\\' => {
          self.advance(); // consume '\'
          match self.peek() {
            Some('n' | 'r' | 't' | '\\' | '\'' | '0') => {
              self.advance();
            },
            Some('x') => {
              // Hex escape: \xNN (two hex digits).
              self.advance();
              let mut count = 0;
              let mut value: u32 = 0;
              while count < 2
                && self
                  .peek()
                  .map(|ch| ch.is_ascii_hexdigit())
                  .unwrap_or(false)
              {
                value = (value << 4) | self.peek().unwrap().to_digit(16).unwrap();
                self.advance();
                count += 1;
              }
              if count < 2 {
                let span = Span::new(self.current.saturating_sub(2), self.current);
                self.emit_diagnostic(self.err_invalid_escape(span, "\\x", Some("hex escape must have exactly two hex digits")));
                return Err(());
              } else if value > 0x7F {
                let span = Span::new(self.start, self.current);
                self.emit_diagnostic(self.err_invalid_escape(span, "\\x", Some("characters must be in ASCII range (<= 0x7F)")));
                return Err(());
              }
            },
            Some('u') if self.peek_next(1) == Some('{') => {
              // Unicode escape: \u{...}, 1–6 hex digits, valid scalar.
              self.advance(); // 'u'
              self.advance(); // '{'
              let mut digits = 0;
              let mut value: u32 = 0;
              while let Some(ch) = self.peek() {
                if ch == '}' {
                  break;
                }
                match ch.to_digit(16) {
                  Some(d) => {
                    if digits < 6 {
                      value = (value << 4) | d;
                    }
                    digits += 1;
                    self.advance();
                  },
                  None => {
                    let span = Span::new(self.current.saturating_sub(3), self.current);
                    self.emit_diagnostic(self.err_invalid_escape(span, "\\u", Some("unicode escape must contain only hex digits")));
                    self.advance();
                    return Err(());
                  },
                }
              }
              if self.peek() == Some('}') {
                self.advance(); // consume '}'
                if digits == 0
                  || digits > 6
                  || value > 0x10FFFF
                  || (0xD800..=0xDFFF).contains(&value)
                {
                  let span = Span::new(self.current.saturating_sub(10), self.current);
                  self.emit_diagnostic(self.err_invalid_escape(span, "\\u", Some("invalid unicode escape code point")));
                  return Err(());
                }
              } else {
                let span = Span::new(self.current.saturating_sub(10), self.current);
                self.emit_diagnostic(self.err_invalid_escape(span, "\\u", Some("unterminated unicode escape, missing '}'")));
                return Err(());
              }
            },
            _ => {
              let escape_char = self.peek().map(|c| c.to_string()).unwrap_or_else(|| "unknown".to_string());
              let span = Span::new(self.current.saturating_sub(1), self.current);
              self.emit_diagnostic(self.err_invalid_escape(span, &escape_char, Some("char literal")));
              if self.peek().is_some() {
                self.advance();
              }
              return Err(());
            },
          }
        },
        '\n' => {
          let span = Span::new(self.current, self.current + 1);
          self.emit_diagnostic(self.err_invalid_character(span, '\n'));
          self.advance();
          return Err(());
        },
        ':' | ',' | ' ' => {
          // Likely a lifetime or unterminated literal.
          break;
        },
        _ => {
          // Plain Unicode scalar.
          self.advance();
        },
      }
    }

    let lexeme = self.get_current_lexeme();

    // Empty: "''"
    if lexeme == "''" {
      let span = Span::new(self.current.saturating_sub(2), self.current);
      self.emit_diagnostic(self.err_empty_char(span));
      return Err(());
    }

    if !terminated {
      // Treat as a lifetime token (`'a`, `'static`, etc.).
      return self.lex_lifetime();
    }

    // Ensure at most one scalar (or a single escape) between the quotes.
    if let Some(inner) = lexeme.strip_prefix('\'').and_then(|s| s.strip_suffix('\'')) {
      // If it doesn't start with an escape, it must be exactly one scalar.
      if !inner.starts_with('\\') && inner.chars().count() != 1 {
        let span = Span::new(self.start, self.current);
        self.emit_diagnostic(self.err_invalid_literal(span, "character literal must contain exactly one Unicode scalar value"));
        return Err(());
      }
    }

    Ok(TokenKind::Literal {
      kind: LiteralKind::Char,
    })
  }

  /// Lex a **normal string**: `"..."`.
  ///
  /// Matches Rust's non-raw string rules:
  /// - Supports the usual escapes plus `\xNN` (ASCII) and `\u{...}`.
  /// - Supports line-continuation escapes (`\` + LF + following whitespace).
  /// - Allows LF, forbids bare CR.
  fn lex_str(&mut self) -> Result<TokenKind, ()> {
    let terminated = self.scan_string_body("string", false, false);

    if !terminated {
      let span = Span::new(self.start, self.current);
      self.emit_diagnostic(self.err_unterminated_string(span, "string"));
      return Err(());
    }

    Ok(TokenKind::Literal {
      kind: LiteralKind::Str,
    })
  }

  /// Helper that checks if the next two characters form a string prefix.
  ///
  /// Returns `true` if the current character plus lookahead are a string prefix.
  pub(crate) fn is_string_prefix(&mut self, first: char) -> bool {
    let next = self.peek();
    let next2 = self.peek_next(1);

    match first {
      // b" or b' or br" or br#"
      'b' => {
        matches!(next, Some('"') | Some('\''))
          || (matches!(next, Some('r')) && matches!(next2, Some('"') | Some('#')))
      },

      // c" or cr" or cr#" (C strings)
      'c' => {
        matches!(next, Some('"'))
          || (matches!(next, Some('r')) && matches!(next2, Some('"') | Some('#')))
      },

      // r" or r#" (raw strings)
      'r' => {
        if matches!(next, Some('"')) {
          true
        } else if matches!(next, Some('#')) {
          let mut offset = 1;
          while self.peek_next(offset) == Some('#') {
            offset += 1;
          }
          matches!(self.peek_next(offset), Some('"'))
        } else {
          false
        }
      },

      _ => false,
    }
  }

  /// Shared implementation for non-raw `"..."` and `c"..."` bodies.
  ///
  /// Returns `true` if a closing `"` was found, `false` on EOF.
  fn scan_string_body(
    &mut self,

    context: &str,
    forbid_nul: bool,
    allow_full_byte_hex: bool,
  ) -> bool {
    let mut terminated = false;
    let max_hex_escape = if allow_full_byte_hex { 0xFF } else { 0x7F };

    while let Some(c) = self.peek() {
      match c {
        '"' => {
          self.advance();
          terminated = true;
          break;
        },
        '\\' => {
          self.advance(); // consume '\'
          let escape_start = self.current - 1;
          match self.peek() {
            // Line continuation: `\` + LF + trailing whitespace.
            Some('\n') => {
              self.advance(); // consume newline
              while matches!(self.peek(), Some(' ' | '\t' | '\n' | '\r')) {
                self.advance();
              }
            },
            Some('n' | 'r' | 't' | '\\' | '"' | '\'' | '0') => {
              let esc = self.peek().unwrap();
              self.advance();
              if forbid_nul && esc == '0' {
                self
                  .report_nul_in_string(context, diagnostic::Span::new(escape_start, self.current));
              }
            },
            Some('x') => {
              self.advance(); // consume 'x'
              let mut count = 0;
              let mut value: u32 = 0;
              while count < 2 {
                match self.peek().and_then(|ch| ch.to_digit(16)) {
                  Some(d) => {
                    value = (value << 4) | d;
                    self.advance();
                    count += 1;
                  },
                  None => break,
                }
              }
              if count < 2 || value > max_hex_escape {
                let span = Span::new(escape_start, self.current);
                self.emit_diagnostic(self.err_invalid_escape(span, "\\x", Some(&format!("{} literal", context))));
              } else if forbid_nul && value == 0 {
                self
                  .report_nul_in_string(context, diagnostic::Span::new(escape_start, self.current));
              }
            },
            Some('u') if self.peek_next(1) == Some('{') => {
              self.advance(); // 'u'
              self.advance(); // '{'
              let mut digits = 0;
              let mut value: u32 = 0;
              while let Some(ch) = self.peek() {
                if ch == '}' {
                  break;
                }
                match ch.to_digit(16) {
                  Some(d) => {
                    if digits < 6 {
                      value = (value << 4) | d;
                    }
                    digits += 1;
                    self.advance();
                  },
                  None => {
                    let span = Span::new(escape_start, self.current);
                    self.emit_diagnostic(self.err_invalid_escape(span, "\\u", Some(&format!("invalid unicode escape in {} literal", context))));
                    self.advance();
                    break;
                  },
                }
              }
              if self.peek() != Some('}') {
                let span = Span::new(escape_start, self.current);
                self.emit_diagnostic(self.err_invalid_escape(span, "\\u", Some("unterminated unicode escape, missing '}'")));
              } else {
                self.advance(); // consume '}'
                if digits == 0
                  || digits > 6
                  || value > 0x10FFFF
                  || (0xD800..=0xDFFF).contains(&value)
                {
                  let span = Span::new(escape_start, self.current);
                  self.emit_diagnostic(self.err_invalid_escape(span, "\\u", Some("invalid unicode escape code point")));
                }
                if forbid_nul && value == 0 {
                  self.report_nul_in_string(
                    context,
                    diagnostic::Span::new(escape_start, self.current),
                  );
                }
              }
            },
            _ => {
              let escape_char = self.peek().map(|c| c.to_string()).unwrap_or_else(|| "unknown".to_string());
              let span = Span::new(escape_start, self.current);
              self.emit_diagnostic(self.err_invalid_escape(span, &escape_char, Some(&format!("{} literal", context))));
              if self.peek().is_some() {
                self.advance();
              }
            },
          }
        },
        '\r' => {
          let span = Span::new(self.current, self.current + 1);
          self.emit_diagnostic(self.err_invalid_escape(span, "\\r", Some(&format!("{} literals", context))));
          self.advance();
        },
        _ => {
          if forbid_nul && c == '\0' {
            self.report_nul_in_string(
              context,
              diagnostic::Span::new(self.current, self.current + 1),
            );
          }
          self.advance();
        },
      }
    }

    terminated
  }

  fn report_nul_in_string(&mut self, context: &str, span: Span) {
    let diag = self
      .diagnostic(
        DiagnosticError::InvalidCharacter,
        format!("{context} literal cannot contain interior NUL bytes"),
      )
      .with_label(
        span,
        Some("NUL byte is not allowed here".to_string()),
        LabelStyle::Primary,
      )
      .with_help(
        "Split the literal or avoid escapes that evaluate to zero when targeting C strings."
          .to_string(),
      );
    self.emit_diagnostic(diag);
  }
}
