use std::iter::Peekable;
use std::str::Chars;

use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
  DiagnosticEngine,
  Span,
};

pub(crate) struct Decoder;

impl Decoder {
  pub(crate) fn decode_string(
    input: &str,
    path: &str,
    span: Span,
    engine: &mut DiagnosticEngine,
  ) -> Result<String, ()> {
    let mut out = String::new();
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
      if c != '\\' {
        out.push(c);
      } else {
        out.push(Self::decode_escape(&mut chars, path, span, engine, true)?);
      }
    }

    Ok(out)
  }

  pub(crate) fn decode_byte_string(
    input: &str,
    path: &str,
    span: Span,
    engine: &mut DiagnosticEngine,
  ) -> Result<Vec<u8>, ()> {
    let mut out = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
      let ch = if c != '\\' {
        c
      } else {
        Self::decode_escape(&mut chars, path, span, engine, false)?
      };

      if ch as u32 > 0xFF {
        engine.add(
          Diagnostic::new(
            DiagnosticCode::Error(DiagnosticError::InvalidLiteral),
            "byte string contains non-byte value".to_string(),
            path.to_string(),
          )
          .with_label(
            span,
            Some("value does not fit in u8 (0-255)".to_string()),
            LabelStyle::Primary,
          )
          .with_help("byte strings can only contain values in the range 0-255".to_string()),
        );
        return Err(());
      }
      out.push(ch as u8);
    }

    if !out.is_empty() && out[0] == b'b' {
      out.remove(0);
    }
    if !out.is_empty() && out[0] == b'\"' {
      out.remove(0);
    }
    if !out.is_empty() && out[out.len() - 1] == b'\"' {
      out.remove(out.len() - 1);
    }

    Ok(out)
  }

  pub(crate) fn decode_char(
    input: &str,
    path: &str,
    span: Span,
    engine: &mut DiagnosticEngine,
  ) -> Result<char, ()> {
    let mut chars = input.chars().peekable();
    let mut out = Vec::new();

    while let Some(c) = chars.next() {
      if c != '\\' {
        out.push(c);
      } else {
        out.push(Self::decode_escape(&mut chars, path, span, engine, true)?);
      }
    }

    if !out.is_empty() && out[0] == 'b' {
      out.remove(0);
    }
    if !out.is_empty() && out[0] == '\'' {
      out.remove(0);
    }
    if !out.is_empty() && out[out.len() - 1] == '\'' {
      out.remove(out.len() - 1);
    }

    if out.len() != 1 {
      engine.add(
        Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::EmptyChar),
          format!(
            "character literal contains {} character(s), expected exactly 1",
            out.len()
          ),
          path.to_string(),
        )
        .with_label(
          span,
          Some("character literal must contain exactly one Unicode scalar value".to_string()),
          LabelStyle::Primary,
        )
        .with_help(
          "character literals must be a single Unicode scalar or a single escape sequence"
            .to_string(),
        ),
      );
      return Err(());
    }

    Ok(out[0])
  }

  pub(crate) fn decode_byte(
    input: &str,
    path: &str,
    span: Span,
    engine: &mut DiagnosticEngine,
  ) -> Result<u8, ()> {
    let ch = Self::decode_char(input, path, span, engine)?;

    if ch as u32 > 0xFF {
      engine.add(
        Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidLiteral),
          "byte literal out of range".to_string(),
          path.to_string(),
        )
        .with_label(
          span,
          Some("value does not fit in u8 (0-255)".to_string()),
          LabelStyle::Primary,
        )
        .with_help("byte literals must be in the range 0-255".to_string()),
      );
      return Err(());
    }

    Ok(ch as u8)
  }

  fn decode_escape(
    chars: &mut Peekable<Chars>,
    path: &str,
    span: Span,
    engine: &mut DiagnosticEngine,
    allow_unicode: bool,
  ) -> Result<char, ()> {
    match chars.next() {
      Some('n') => Ok('\n'),
      Some('r') => Ok('\r'),
      Some('t') => Ok('\t'),
      Some('0') => Ok('\0'),
      Some('\\') => Ok('\\'),
      Some('"') => Ok('"'),
      Some('\'') => Ok('\''),

      Some('x') => Self::decode_hex_escape(chars, path, span, engine),

      Some('u') if allow_unicode => Self::decode_unicode_escape(chars, path, span, engine),

      Some('u') => {
        let mut diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidEscape),
          "invalid escape sequence `\\u` in byte string".to_string(),
          path.to_string(),
        )
        .with_label(
          span,
          Some("unknown escape sequence: `\\u`".to_string()),
          LabelStyle::Primary,
        )
        .with_note("in byte string, only certain escape sequences are allowed".to_string());
        diagnostic = diagnostic.with_help(
          "valid escape sequences are: `\\n`, `\\r`, `\\t`, `\\\\`, `\\'`, `\\\"`, `\\xHH`, and `\\u{HHHH}`"
            .to_string(),
        );
        engine.add(diagnostic);
        Err(())
      },

      Some(other) => {
        let escape = other.to_string();
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidEscape),
          format!("invalid escape sequence `\\{escape}`"),
          path.to_string(),
        )
        .with_label(
          span,
          Some(format!("unknown escape sequence: `\\{escape}`")),
          LabelStyle::Primary,
        )
        .with_help(
          "valid escape sequences are: `\\n`, `\\r`, `\\t`, `\\\\`, `\\'`, `\\\"`, `\\xHH`, and `\\u{HHHH}`"
            .to_string(),
        );
        engine.add(diagnostic);
        Err(())
      },

      None => {
        let mut diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidEscape),
          "invalid escape sequence `\\` in unterminated".to_string(),
          path.to_string(),
        )
        .with_label(
          span,
          Some("unknown escape sequence: `\\`".to_string()),
          LabelStyle::Primary,
        )
        .with_note("in unterminated, only certain escape sequences are allowed".to_string());
        diagnostic = diagnostic.with_help(
          "valid escape sequences are: `\\n`, `\\r`, `\\t`, `\\\\`, `\\'`, `\\\"`, `\\xHH`, and `\\u{HHHH}`"
            .to_string(),
        );
        engine.add(diagnostic);
        Err(())
      },
    }
  }

  fn decode_hex_escape(
    chars: &mut Peekable<Chars>,
    path: &str,
    span: Span,
    engine: &mut DiagnosticEngine,
  ) -> Result<char, ()> {
    let a = chars.next();
    let b = chars.next();

    match (a, b) {
      (Some(a), Some(b)) if a.is_ascii_hexdigit() && b.is_ascii_hexdigit() => {
        let value = u8::from_str_radix(&format!("{}{}", a, b), 16).unwrap();
        Ok(value as char)
      },
      _ => {
        let mut diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidEscape),
          "invalid escape sequence `\\x` in hex escape must have exactly two hex digits"
            .to_string(),
          path.to_string(),
        )
        .with_label(
          span,
          Some("unknown escape sequence: `\\x`".to_string()),
          LabelStyle::Primary,
        )
        .with_note(
          "in hex escape must have exactly two hex digits, only certain escape sequences are allowed"
            .to_string(),
        );
        diagnostic = diagnostic.with_help(
          "valid escape sequences are: `\\n`, `\\r`, `\\t`, `\\\\`, `\\'`, `\\\"`, `\\xHH`, and `\\u{HHHH}`"
            .to_string(),
        );
        engine.add(diagnostic);
        Err(())
      },
    }
  }

  fn decode_unicode_escape(
    chars: &mut Peekable<Chars>,
    path: &str,
    span: Span,
    engine: &mut DiagnosticEngine,
  ) -> Result<char, ()> {
    match chars.next() {
      Some('{') => {},
      _ => {
        let mut diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidEscape),
          "invalid escape sequence `\\u` in unicode escape must start with `{`".to_string(),
          path.to_string(),
        )
        .with_label(
          span,
          Some("unknown escape sequence: `\\u`".to_string()),
          LabelStyle::Primary,
        )
        .with_note(
          "in unicode escape must start with `{`, only certain escape sequences are allowed"
            .to_string(),
        );
        diagnostic = diagnostic.with_help(
          "valid escape sequences are: `\\n`, `\\r`, `\\t`, `\\\\`, `\\'`, `\\\"`, `\\xHH`, and `\\u{HHHH}`"
            .to_string(),
        );
        engine.add(diagnostic);
        return Err(());
      },
    }

    let mut value: u32 = 0;
    let mut digits = 0;

    while let Some(&c) = chars.peek() {
      if c == '}' {
        break;
      }
      if let Some(d) = c.to_digit(16) {
        value = (value << 4) | d;
        digits += 1;
        chars.next();
      } else {
        break;
      }
    }

    match chars.next() {
      Some('}') => {},
      _ => {
        let mut diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidEscape),
          "invalid escape sequence `\\u` in unicode escape must end with `}`".to_string(),
          path.to_string(),
        )
        .with_label(
          span,
          Some("unknown escape sequence: `\\u`".to_string()),
          LabelStyle::Primary,
        )
        .with_note(
          "in unicode escape must end with `}`, only certain escape sequences are allowed"
            .to_string(),
        );
        diagnostic = diagnostic.with_help(
          "valid escape sequences are: `\\n`, `\\r`, `\\t`, `\\\\`, `\\'`, `\\\"`, `\\xHH`, and `\\u{HHHH}`"
            .to_string(),
        );
        engine.add(diagnostic);
        return Err(());
      },
    }

    if digits == 0 || digits > 6 || value > 0x10FFFF || (0xD800..=0xDFFF).contains(&value) {
      let mut diagnostic = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::InvalidEscape),
        "invalid escape sequence `\\u` in invalid unicode scalar value".to_string(),
        path.to_string(),
      )
      .with_label(
        span,
        Some("unknown escape sequence: `\\u`".to_string()),
        LabelStyle::Primary,
      )
      .with_note(
        "in invalid unicode scalar value, only certain escape sequences are allowed".to_string(),
      );
      diagnostic = diagnostic.with_help(
        "valid escape sequences are: `\\n`, `\\r`, `\\t`, `\\\\`, `\\'`, `\\\"`, `\\xHH`, and `\\u{HHHH}`"
          .to_string(),
      );
      engine.add(diagnostic);
      return Err(());
    }

    char::from_u32(value).ok_or_else(|| {
      let mut diagnostic = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::InvalidEscape),
        "invalid escape sequence `\\u` in invalid unicode scalar value".to_string(),
        path.to_string(),
      )
      .with_label(
        span,
        Some("unknown escape sequence: `\\u`".to_string()),
        LabelStyle::Primary,
      )
      .with_note(
        "in invalid unicode scalar value, only certain escape sequences are allowed".to_string(),
      );
      diagnostic = diagnostic.with_help(
        "valid escape sequences are: `\\n`, `\\r`, `\\t`, `\\\\`, `\\'`, `\\\"`, `\\xHH`, and `\\u{HHHH}`"
          .to_string(),
      );
      engine.add(diagnostic);
    })
  }
}
