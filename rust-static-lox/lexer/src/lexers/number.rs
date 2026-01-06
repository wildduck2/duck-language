use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
  Span,
};

use crate::{
  token::{Base, LiteralKind, TokenKind},
  Lexer,
};

impl Lexer {
  /// Lex a numeric literal starting at the current offset.
  ///
  /// - Detects base by prefix:
  ///   - `0b` -> binary
  ///   - `0o` -> octal
  ///   - `0x` -> hexadecimal
  ///   - otherwise -> decimal
  ///
  /// - For decimal / hex, also detects floats (fraction + exponent).
  /// - After lexing the numeric core, rejects leading/trailing `_` which
  ///   are not allowed in Rust (underscores must be **between digits**).
  pub(crate) fn lex_number(&mut self) -> Result<TokenKind, ()> {
    let kind = if self.get_current_lexeme() == "0" {
      if self.match_char('b') {
        self.lex_binary()
      } else if self.match_char('o') {
        self.lex_octal()
      } else if self.match_char('x') {
        self.lex_hexadecimal()
      } else {
        self.lex_decimal()
      }
    } else {
      self.lex_decimal()
    };

    let number = self.get_current_lexeme();
    if number.starts_with('_') || number.ends_with('_') {
      let bad_span = Span::new(self.start, self.current);
      let diagnostic = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::InvalidInteger),
        format!("`{number}` is not a valid integer literal"),
        self.source.path.clone(),
      )
      .with_label(
        bad_span,
        Some("underscores may be used only between digits, never at the start or end".to_string()),
        LabelStyle::Primary,
      )
      .with_help("examples of valid literals: `1_000`, `0xFF_A0`, `123`".to_string())
      .with_note("examples of invalid literals: `_123`, `123_`, `0x_12`".to_string());
      self.emit_diagnostic(diagnostic);
      return Err(());
    }

    Ok(TokenKind::Literal { kind })
  }

  /// Lex a binary integer: `0b[01_]+`.
  ///
  /// Accepts `_` separators (not doubled). Records `empty_int` if no
  /// digits follow `0b`. Also probes for an optional integer suffix.
  fn lex_binary(&mut self) -> LiteralKind {
    let mut empty_int = false;
    let mut suffix_start = 0;
    while let Some(c) = self.peek() {
      if c == '0' || c == '1' {
        self.advance();
        empty_int = true;
      } else if c == '_' && self.peek_next(1) != Some('_') {
        if !empty_int {
          let span = Span::new(self.current, self.current + 1);
          let diagnostic = Diagnostic::new(
            DiagnosticCode::Error(DiagnosticError::InvalidInteger),
            "0b literal cannot start with `_` after the prefix".to_string(),
            self.source.path.clone(),
          )
          .with_label(
            span,
            Some("remove this underscore".to_string()),
            LabelStyle::Primary,
          )
          .with_help("add digits after `0b` before any underscores".to_string());
          self.emit_diagnostic(diagnostic);
          break;
        }
        self.advance();
        continue;
      } else if c == '_' {
        let span = Span::new(self.current, self.current + 1);
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidInteger),
          "invalid `_` placement in binary literal".to_string(),
          self.source.path.clone(),
        )
        .with_label(
          span,
          Some("consecutive underscores are not allowed".to_string()),
          LabelStyle::Primary,
        )
        .with_help("underscores can only appear between digits, not consecutively".to_string());
        self.emit_diagnostic(diagnostic);
        break;
      } else {
        if c == 'u' || c == 'i' {
          self.check_suffix_type(c, &mut suffix_start, false);
        } else if c.is_ascii_alphanumeric() || c == '_' {
          let span = Span::new(self.current, self.current + c.len_utf8());
          let diagnostic = Diagnostic::new(
            DiagnosticCode::Error(DiagnosticError::InvalidInteger),
            format!("invalid digit `{c}` in binary literal"),
            self.source.path.clone(),
          )
          .with_label(
            span,
            Some("expected a valid binary digit here".to_string()),
            LabelStyle::Primary,
          );
          self.emit_diagnostic(diagnostic);
        }
        break;
      }
    }

    if suffix_start == 0 {
      suffix_start = self.current;
    }

    if !empty_int {
      let span = Span::new(self.start, self.current);
      let diagnostic = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::InvalidInteger),
        "binary literal has no digits after `0b` prefix".to_string(),
        self.source.path.clone(),
      )
      .with_label(
        span,
        Some("expected binary digits (`0` or `1`) after `0b`".to_string()),
        LabelStyle::Primary,
      )
      .with_help("add binary digits after `0b`, e.g. `0b1010`".to_string());
      self.emit_diagnostic(diagnostic);
    }

    LiteralKind::Integer {
      base: Base::Binary,
      empty_int: !empty_int,
      suffix_start,
    }
  }

  /// Lex an octal integer: `0o[0-7_]+`.
  ///
  /// Accepts `_` separators (not doubled). Records `empty_int` if no
  /// digits follow `0o`. Also probes for an optional integer suffix.
  fn lex_octal(&mut self) -> LiteralKind {
    let mut empty_int = false;
    let mut suffix_start = 0;
    while let Some(c) = self.peek() {
      if ('0'..='7').contains(&c) {
        self.advance();
        empty_int = true;
      } else if c == '_' && self.peek_next(1) != Some('_') {
        if !empty_int {
          let span = Span::new(self.current, self.current + 1);
          let diagnostic = Diagnostic::new(
            DiagnosticCode::Error(DiagnosticError::InvalidInteger),
            "0o literal cannot start with `_` after the prefix".to_string(),
            self.source.path.clone(),
          )
          .with_label(
            span,
            Some("remove this underscore".to_string()),
            LabelStyle::Primary,
          )
          .with_help("add digits after `0o` before any underscores".to_string());
          self.emit_diagnostic(diagnostic);
          break;
        }
        self.advance();
        continue;
      } else if c == '_' {
        let span = Span::new(self.current, self.current + 1);
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidInteger),
          "invalid `_` placement in octal literal".to_string(),
          self.source.path.clone(),
        )
        .with_label(
          span,
          Some("consecutive underscores are not allowed".to_string()),
          LabelStyle::Primary,
        )
        .with_help("underscores can only appear between digits, not consecutively".to_string());
        self.emit_diagnostic(diagnostic);
        break;
      } else {
        if c == 'u' || c == 'i' {
          self.check_suffix_type(c, &mut suffix_start, false);
        } else if c.is_ascii_alphanumeric() || c == '_' {
          let span = Span::new(self.current, self.current + c.len_utf8());
          let diagnostic = Diagnostic::new(
            DiagnosticCode::Error(DiagnosticError::InvalidInteger),
            format!("invalid digit `{c}` in octal literal"),
            self.source.path.clone(),
          )
          .with_label(
            span,
            Some("expected a valid octal digit here".to_string()),
            LabelStyle::Primary,
          );
          self.emit_diagnostic(diagnostic);
        }
        break;
      }
    }

    if suffix_start == 0 {
      suffix_start = self.current;
    }

    if !empty_int {
      let span = Span::new(self.start, self.current);
      let diagnostic = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::InvalidInteger),
        "octal literal has no digits after `0o` prefix".to_string(),
        self.source.path.clone(),
      )
      .with_label(
        span,
        Some("expected octal digits (`0`-`7`) after `0o`".to_string()),
        LabelStyle::Primary,
      )
      .with_help("add octal digits after `0o`, e.g. `0o755`".to_string());
      self.emit_diagnostic(diagnostic);
    }

    LiteralKind::Integer {
      base: Base::Octal,
      empty_int: !empty_int,
      suffix_start,
    }
  }

  /// Lex a decimal number: integer or float.
  ///
  /// Integer: `[0-9][0-9_]*`
  ///
  /// Float forms:
  /// - `123.45`
  /// - `1e10`, `2.5E-3`
  /// - supports underscores in integer, fraction and exponent parts
  ///   (`1_000.0`, `1.0_0`, `1e+_2`, etc.)
  ///
  /// Uses a lookahead around `.` so that `1.foo` / `1._foo` lex as
  /// `1` `.` `foo` (not a float literal).
  fn lex_decimal(&mut self) -> LiteralKind {
    let prev_is_dot = matches!(self.tokens.last().map(|t| t.kind), Some(TokenKind::Dot));
    let allow_float = !prev_is_dot;
    let mut has_dot = false;
    let mut has_exponent = false;
    let mut suffix_start = 0;

    while let Some(c) = self.peek() {
      if c.is_ascii_digit() {
        self.advance();
      } else if c == '_' && self.peek_next(1) != Some('_') {
        // underscore separator in integer / fractional / exponent digits
        self.advance();
        continue;
      } else if c == '.' && !has_dot && !has_exponent && allow_float {
        // Decide whether '.' starts a fractional part or belongs to the next token.
        // Rust rule: treat it as fractional if there is a digit somewhere right after
        // either directly, or after a single underscore:
        //
        //   1.0      -> float
        //   1._0     -> float
        //   1.foo    -> int + '.' + ident
        //   1._foo   -> int + '.' + ident
        let next = self.peek_next(1);
        let lookahead_digit = match next {
          Some(n) if n.is_ascii_digit() => true,
          Some('_') => {
            // Allow '.' '_' digit as fractional, but only if a digit follows.
            matches!(self.peek_next(2), Some(d) if d.is_ascii_digit())
          },
          _ => false,
        };

        if lookahead_digit {
          has_dot = true;
          self.advance(); // consume '.'
        } else {
          break; // '.' belongs to the next token (field access, range, etc.)
        }
      } else if (c == 'e' || c == 'E') && !has_exponent && allow_float {
        has_exponent = true;
        self.advance();

        // Optional sign after e/E
        if let Some(sign) = self.peek() {
          if sign == '+' || sign == '-' {
            self.advance();
          }
        }

        // Exponent digits + underscores.
        // We deliberately do *not* enforce "at least one digit" here:
        // Rust allows the lexer to accept tokens like `1e` and leave
        // the semantic error to later stages.
        while let Some(ec) = self.peek() {
          if ec.is_ascii_digit() {
            self.advance();
          } else {
            match ec == '_' && self.peek_next(1) != Some('_') {
              true => {
                self.advance();
              },
              false => break,
            }
          }
        }
      } else {
        self.check_suffix_type(c, &mut suffix_start, has_dot || has_exponent);
        break;
      }
    }

    if suffix_start == 0 {
      suffix_start = self.current;
    }

    if has_dot || has_exponent {
      LiteralKind::Float {
        base: Base::Decimal,
        suffix_start,
      }
    } else {
      LiteralKind::Integer {
        base: Base::Decimal,
        empty_int: false, // decimal integers are never "empty" once here
        suffix_start,
      }
    }
  }

  /// Internal: recognize and consume a numeric **suffix** at the current position.
  ///
  /// Supports:
  /// - integers: `u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize`
  /// - floats: `f32|f64` (only if `is_float == true`)
  ///
  /// Returns `true` if a valid suffix was fully consumed, `false` otherwise.
  /// Emits a diagnostic only when a *recognized* suffix (`u`/`i`) is malformed.
  fn check_suffix_type(&mut self, c: char, suffix_start: &mut usize, is_float: bool) -> bool {
    *suffix_start = self.current;

    // Float suffix: f32 / f64
    if c == 'f' && is_float {
      self.advance(); // consume 'f'

      if self.peek() == Some('3') {
        self.advance();
        if self.peek() == Some('2') {
          self.advance();
          return true; // f32
        }
      } else if self.peek() == Some('6') {
        self.advance();
        if self.peek() == Some('4') {
          self.advance();
          return true; // f64
        }
      }

      // `f` that doesn't form `f32` or `f64` is simply not a valid suffix;
      // we return false and let the caller treat it as the next token.
      return false;
    }

    // Integer suffix: u* / i*
    if (c == 'u' || c == 'i') && is_float {
      let span = Span::new(*suffix_start, self.current + c.len_utf8());
      let diagnostic = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::InvalidLiteral),
        "invalid literal: float literals cannot have integer suffixes".to_string(),
        self.source.path.clone(),
      )
      .with_label(
        span,
        Some("float literals cannot have integer suffixes".to_string()),
        LabelStyle::Primary,
      );
      self.emit_diagnostic(diagnostic);
      return false;
    }

    if c == 'u' || c == 'i' {
      let ok = self.inner_check_suffix_type(c, suffix_start);
      return ok;
    }

    // Not a suffix start: leave it to the parser / next token.
    false
  }

  /// Internal: helper used by `check_suffix_type` to parse concrete integer suffixes.
  ///
  /// Consumes characters after the leading `u`/`i`. Returns `true` on success.
  /// On failure, emits a focused diagnostic pointing at the suffix span.
  fn inner_check_suffix_type(&mut self, c: char, suffix_start: &mut usize) -> bool {
    // consume 'u' or 'i'
    self.advance();

    match self.peek() {
      // u8 / i8
      Some('8') => {
        self.advance();
        true
      },

      // 16, 128
      Some('1') => {
        self.advance();

        if self.peek() == Some('6') {
          // 16
          self.advance();
          return true;
        } else if self.peek() == Some('2') {
          self.advance();

          if self.peek() == Some('8') {
            // 128
            self.advance();
            return true;
          }
        }

        self.report_invalid_suffix(c, *suffix_start);
        false
      },

      // 32
      Some('3') => {
        self.advance();

        if self.peek() == Some('2') {
          self.advance();
          return true;
        }

        self.report_invalid_suffix(c, *suffix_start);
        false
      },

      // 64
      Some('6') => {
        self.advance();

        if self.peek() == Some('4') {
          self.advance();
          return true;
        }

        self.report_invalid_suffix(c, *suffix_start);
        false
      },

      // isize / usize
      Some('s') => {
        self.advance();
        if self.peek() == Some('i') {
          self.advance();

          if self.peek() == Some('z') {
            self.advance();

            if self.peek() == Some('e') {
              self.advance();
              return true;
            }
          }
        }

        self.report_invalid_suffix(c, *suffix_start);
        false
      },

      _ => {
        // Only emit a diagnostic because we *know* we're in a `u`/`i` suffix;
        // otherwise this would just be another token.
        self.report_invalid_suffix(c, *suffix_start);
        false
      },
    }
  }

  fn report_invalid_suffix(&mut self, c: char, suffix_start: usize) {
    let span = Span::new(suffix_start, self.current);
    let diagnostic = Diagnostic::new(
      DiagnosticCode::Error(DiagnosticError::InvalidCharacter),
      format!("invalid character: `{c}`"),
      self.source.path.clone(),
    )
    .with_label(
      span,
      Some(format!("character `{c}` is not valid in source code")),
      LabelStyle::Primary,
    )
    .with_help("remove this character or replace it with a valid one".to_string());
    self.emit_diagnostic(diagnostic);
  }

  /// Lex a hexadecimal number (`0x`/`0X`), supporting **ints and hex floats**.
  ///
  /// Integer: `0x[0-9A-Fa-f_]+`
  ///
  /// Hex-float (C/Rust-style): `0xA.BCp±E`
  /// - optional fraction after `.`
  /// - exponent is base-2 (`p` / `P`)
  /// - optional `+` / `-` sign
  /// - underscores allowed inside digits
  fn lex_hexadecimal(&mut self) -> LiteralKind {
    let mut empty_int = false;
    let mut has_dot = false;
    let mut has_exponent = false;
    let mut has_exp_digits = false;
    let mut suffix_start = 0;

    // consume hex digits and optional dot
    while let Some(c) = self.peek() {
      if c.is_ascii_hexdigit() {
        self.advance();
        empty_int = true;
      } else if c == '_' && self.peek_next(1) != Some('_') {
        if !empty_int {
          let span = Span::new(self.current, self.current + 1);
          let diagnostic = Diagnostic::new(
            DiagnosticCode::Error(DiagnosticError::InvalidInteger),
            "0x literal cannot start with `_` after the prefix".to_string(),
            self.source.path.clone(),
          )
          .with_label(
            span,
            Some("remove this underscore".to_string()),
            LabelStyle::Primary,
          )
          .with_help("add digits after `0x` before any underscores".to_string());
          self.emit_diagnostic(diagnostic);
          break;
        }
        self.advance();
      } else if c == '_' {
        let span = Span::new(self.current, self.current + 1);
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidInteger),
          "invalid `_` placement in hexadecimal literal".to_string(),
          self.source.path.clone(),
        )
        .with_label(
          span,
          Some("consecutive underscores are not allowed".to_string()),
          LabelStyle::Primary,
        )
        .with_help("underscores can only appear between digits, not consecutively".to_string());
        self.emit_diagnostic(diagnostic);
        break;
      } else if c == '.' && !has_dot {
        has_dot = true;
        self.advance();
      } else {
        break;
      }
    }

    // check for exponent part (p or P)
    if let Some(c) = self.peek() {
      if c == 'p' || c == 'P' {
        has_exponent = true;
        self.advance();

        // optional sign
        if let Some(sign) = self.peek() {
          if sign == '+' || sign == '-' {
            self.advance();
          }
        }

        // exponent digits
        while let Some(ec) = self.peek() {
          if ec.is_ascii_digit() {
            self.advance();
            has_exp_digits = true;
          } else if ec == '_' && self.peek_next(1) != Some('_') {
            self.advance();
          } else {
            break;
          }
        }

        if !has_exp_digits {
          let span = Span::new(self.start, self.current);
          let diagnostic = Diagnostic::new(
            DiagnosticCode::Error(DiagnosticError::InvalidCharacter),
            "invalid character: `p`".to_string(),
            self.source.path.clone(),
          )
          .with_label(
            span,
            Some("character `p` is not valid in source code".to_string()),
            LabelStyle::Primary,
          )
          .with_help("remove this character or replace it with a valid one".to_string());
          self.emit_diagnostic(diagnostic);
        }
      }
    }

    // suffix check (like u8 or f64)
    if let Some(c) = self.peek() {
      if c == 'u' || c == 'i' || (c == 'f' && (has_dot || has_exponent)) {
        self.check_suffix_type(c, &mut suffix_start, has_dot || has_exponent);
      } else if c.is_ascii_alphanumeric() || c == '_' {
        let span = Span::new(self.current, self.current + c.len_utf8());
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidInteger),
          format!("invalid digit `{c}` in hexadecimal literal"),
          self.source.path.clone(),
        )
        .with_label(
          span,
          Some("expected a valid hexadecimal digit here".to_string()),
          LabelStyle::Primary,
        );
        self.emit_diagnostic(diagnostic);
      }
    }

    // if suffix_start is 0, set it to current: end of numeric core
    if suffix_start == 0 {
      suffix_start = self.current;
    }

    if !empty_int {
      let span = Span::new(self.start, self.current);
      let diagnostic = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::InvalidInteger),
        "hexadecimal literal has no digits after `0x` prefix".to_string(),
        self.source.path.clone(),
      )
      .with_label(
        span,
        Some("expected hexadecimal digits (`0`-`9`, `a`-`f`, `A`-`F`) after `0x`".to_string()),
        LabelStyle::Primary,
      )
      .with_help("add hexadecimal digits after `0x`, e.g. `0xFF`".to_string());
      self.emit_diagnostic(diagnostic);
    }

    if has_dot || has_exponent {
      LiteralKind::Float {
        base: Base::Hexadecimal,
        suffix_start,
      }
    } else {
      LiteralKind::Integer {
        base: Base::Hexadecimal,
        empty_int: !empty_int,
        suffix_start,
      }
    }
  }
}
