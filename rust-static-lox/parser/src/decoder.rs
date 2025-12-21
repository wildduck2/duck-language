use std::iter::Peekable;
use std::str::Chars;

use diagnostic::DiagnosticEngine;
use diagnostic::Span;

use crate::Parser;

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
        engine.add(Parser::decoder_err_byte_string_non_byte(path, span));
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
      engine.add(Parser::decoder_err_char_too_long(path, span, &out));
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
      engine.add(Parser::decoder_err_byte_out_of_range(path, span));
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
        engine.add(Parser::decoder_err_invalid_escape(path, span, "u", Some("byte string")));
        Err(())
      },

      Some(other) => {
        engine.add(Parser::decoder_err_invalid_escape(path, span, &other.to_string(), None));
        Err(())
      },

      None => {
        engine.add(Parser::decoder_err_invalid_escape(path, span, "", Some("unterminated")));
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
        engine.add(Parser::decoder_err_invalid_escape(path, span, "x", Some("hex escape must have exactly two hex digits")));
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
        engine.add(Parser::decoder_err_invalid_escape(path, span, "u", Some("unicode escape must start with `{`")));
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
        engine.add(Parser::decoder_err_invalid_escape(path, span, "u", Some("unicode escape must end with `}`")));
        return Err(());
      },
    }

    if digits == 0 || digits > 6 || value > 0x10FFFF || (0xD800..=0xDFFF).contains(&value) {
      engine.add(Parser::decoder_err_invalid_escape(path, span, "u", Some("invalid unicode scalar value")));
      return Err(());
    }

    char::from_u32(value).ok_or_else(|| {
      engine.add(Parser::decoder_err_invalid_escape(path, span, "u", Some("invalid unicode scalar value")));
    })
  }
}
