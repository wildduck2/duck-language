use std::iter::Peekable;
use std::str::Chars;

use diagnostic::code::DiagnosticCode;
use diagnostic::diagnostic::{Diagnostic, LabelStyle};
use diagnostic::types::error::DiagnosticError;
use diagnostic::{DiagnosticEngine, Span};

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
            "byte string contains non byte value".into(),
            path.to_string(),
          )
          .with_label(
            span,
            Some("value does not fit in u8".into()),
            LabelStyle::Primary,
          ),
        );
        return Err(());
      }
      out.push(ch as u8);
    }

    if out[0] == b'b' {
      out.remove(0);
    }
    if out[0] == b'\"' {
      out.remove(0);
    }
    if out[out.len() - 1] == b'\"' {
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

    if out[0] == 'b' {
      out.remove(0);
    }
    if out[0] == '\'' {
      out.remove(0);
    }
    if out[out.len() - 1] == '\'' {
      out.remove(out.len() - 1);
    }

    if out.len() != 1 {
      let diagnostic = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::UnterminatedString),
        format!("Too many characters in character literal: {:?}", out),
        path.to_string(),
      )
      .with_label(
        span,
        Some("This character literal is too long".to_string()),
        LabelStyle::Primary,
      )
      .with_help(
        "Character literals must be a single Unicode scalar or a single escape.".to_string(),
      );
      engine.add(diagnostic);
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
          "byte literal out of range".into(),
          path.to_string(),
        )
        .with_label(
          span,
          Some("value does not fit in u8".into()),
          LabelStyle::Primary,
        ),
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
        engine.add(
          Diagnostic::new(
            DiagnosticCode::Error(DiagnosticError::InvalidEscape),
            "unicode escape not allowed here".into(),
            path.to_string(),
          )
          .with_label(span, Some("invalid escape".into()), LabelStyle::Primary),
        );
        Err(())
      },

      Some(other) => {
        engine.add(
          Diagnostic::new(
            DiagnosticCode::Error(DiagnosticError::InvalidEscape),
            format!("unknown escape \\{}", other),
            path.to_string(),
          )
          .with_label(span, Some("invalid escape".into()), LabelStyle::Primary),
        );
        Err(())
      },

      None => {
        engine.add(
          Diagnostic::new(
            DiagnosticCode::Error(DiagnosticError::InvalidEscape),
            "unterminated escape sequence".into(),
            path.to_string(),
          )
          .with_label(
            span,
            Some("unterminated escape".into()),
            LabelStyle::Primary,
          ),
        );
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
        engine.add(
          Diagnostic::new(
            DiagnosticCode::Error(DiagnosticError::InvalidEscape),
            "invalid hex escape".into(),
            path.to_string(),
          )
          .with_label(
            span,
            Some("expected two hex digits".into()),
            LabelStyle::Primary,
          ),
        );
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
        engine.add(
          Diagnostic::new(
            DiagnosticCode::Error(DiagnosticError::InvalidEscape),
            "invalid unicode escape".into(),
            path.to_string(),
          )
          .with_label(span, Some("expected `{`".into()), LabelStyle::Primary),
        );
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
        engine.add(
          Diagnostic::new(
            DiagnosticCode::Error(DiagnosticError::InvalidEscape),
            "unterminated unicode escape".into(),
            path.to_string(),
          )
          .with_label(span, Some("missing `}`".into()), LabelStyle::Primary),
        );
        return Err(());
      },
    }

    if digits == 0 || digits > 6 || value > 0x10FFFF || (0xD800..=0xDFFF).contains(&value) {
      engine.add(
        Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidEscape),
          "invalid unicode scalar value".into(),
          path.to_string(),
        )
        .with_label(span, Some("invalid code point".into()), LabelStyle::Primary),
      );
      return Err(());
    }

    char::from_u32(value).ok_or_else(|| {
      engine.add(
        Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidEscape),
          "invalid unicode scalar value".into(),
          path.to_string(),
        )
        .with_label(span, Some("invalid code point".into()), LabelStyle::Primary),
      );
    })
  }
}
