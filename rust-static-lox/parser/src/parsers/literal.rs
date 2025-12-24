use lexer::token::{LiteralKind, Token};

use crate::{
  ast::{Expr, ExprKind, Lit},
  decoder::Decoder,
  Parser,
};

impl Parser {
  pub(crate) fn parser_literal(&mut self, kind: LiteralKind) -> Result<Expr, ()> {
    let mut token = self.current_token();
    self.advance();

    match kind {
      LiteralKind::Integer {
        suffix_start,
        base,
        empty_int,
      } => self.parser_integer(&mut token, empty_int, suffix_start, base),

      LiteralKind::Float { suffix_start, base } => {
        self.parser_float(&mut token, suffix_start, base)
      },

      LiteralKind::Str => self.parser_string(&mut token),

      LiteralKind::RawStr { n_hashes } => self.parser_raw_string(&mut token, n_hashes),

      LiteralKind::ByteStr => self.parser_byte_string(&mut token),

      LiteralKind::RawByteStr { n_hashes } => self.parser_raw_byte_string(&mut token, n_hashes),

      LiteralKind::CStr => self.parser_c_string(&mut token),

      LiteralKind::RawCStr { n_hashes } => self.parser_raw_c_string(&mut token, n_hashes),

      LiteralKind::Char => self.parser_char(&mut token),

      LiteralKind::Byte => self.parser_byte(&mut token),
    }
  }

  fn parser_integer(
    &mut self,
    token: &mut Token,
    empty_int: bool,
    suffix_start: usize,
    base: lexer::token::Base,
  ) -> Result<Expr, ()> {
    if empty_int {
      self.emit(self.err_invalid_literal(token.span, "integer literal has no digits"));
      return Err(());
    }

    // Extract suffix + numeric portion
    let src = &self.source_file.src;

    let suffix = if suffix_start == token.span.end {
      None
    } else {
      Some(src[suffix_start..token.span.end].to_string())
    };
    let value_str = &src[token.span.start..suffix_start].replace('_', "");

    let parsed = match base {
      lexer::token::Base::Binary => i128::from_str_radix(&value_str[2..], 2),
      lexer::token::Base::Octal => i128::from_str_radix(&value_str[2..], 8),
      lexer::token::Base::Hexadecimal => i128::from_str_radix(&value_str[2..], 16),
      lexer::token::Base::Decimal => value_str.parse::<i128>(),
    };

    let value = match parsed {
      Ok(v) => v,
      Err(_) => {
        self
          .emit(self.err_invalid_literal(token.span, "integer literal is too large or malformed"));
        return Err(());
      },
    };

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Literal(Lit::Integer { value, suffix }),
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parser_float(
    &mut self,
    token: &mut Token,
    suffix_start: usize,
    base: lexer::token::Base,
  ) -> Result<Expr, ()> {
    let src = &self.source_file.src;
    let suffix = if suffix_start == token.span.end {
      None
    } else {
      Some(src[suffix_start..token.span.end].to_string())
    };

    let value_str = &src[token.span.start..suffix_start];

    let cleaned = value_str.replace('_', "");
    let parsed = match base {
      lexer::token::Base::Decimal => cleaned.parse::<f64>().map_err(|_| ()),
      lexer::token::Base::Hexadecimal => Self::parse_hex_float_literal(&cleaned),
      _ => Err(()),
    };

    let value = match parsed {
      Ok(v) => v,
      Err(_) => {
        self.emit(self.err_invalid_literal(token.span, "float literal is too large or malformed"));
        return Err(());
      },
    };

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Literal(Lit::Float { value, suffix }),
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_hex_float_literal(value: &str) -> Result<f64, ()> {
    let mut parts = value.split(['p', 'P']);
    let mantissa = parts.next().ok_or(())?;
    let exponent_part = parts.next().ok_or(())?;
    if parts.next().is_some() {
      return Err(());
    }

    if mantissa.len() <= 2 || !mantissa.starts_with("0x") && !mantissa.starts_with("0X") {
      return Err(());
    }

    let exponent: i32 = exponent_part.parse().map_err(|_| ())?;

    let digits = &mantissa[2..];
    let mut seen_digit = false;
    let mut seen_dot = false;
    let mut int_part = 0f64;
    let mut frac_part = 0f64;
    let mut frac_weight = 1f64;

    for ch in digits.chars() {
      if ch == '.' {
        if seen_dot {
          return Err(());
        }
        seen_dot = true;
        continue;
      }

      let digit = ch.to_digit(16).ok_or(())? as f64;
      seen_digit = true;
      if !seen_dot {
        int_part = int_part * 16.0 + digit;
      } else {
        frac_weight /= 16.0;
        frac_part += digit * frac_weight;
      }
    }

    if !seen_digit {
      return Err(());
    }

    let mantissa_value = int_part + frac_part;
    Ok(mantissa_value * 2f64.powi(exponent))
  }

  fn parser_string(&mut self, token: &mut Token) -> Result<Expr, ()> {
    let value = self.get_token_lexeme(token);
    let value = Decoder::decode_string(
      &value,
      &self.source_file.path.clone(),
      token.span,
      &mut self.engine.borrow_mut(),
    )?;

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Literal(Lit::String {
        value,
        raw_hashes: None,
      }),
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parser_byte_string(&mut self, token: &mut Token) -> Result<Expr, ()> {
    let value = self.get_token_lexeme(token);
    let value = Decoder::decode_byte_string(
      &value,
      &self.source_file.path.clone(),
      token.span,
      &mut self.engine.borrow_mut(),
    )?;

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Literal(Lit::ByteString {
        value,
        raw_hashes: None,
      }),
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parser_c_string(&mut self, token: &mut Token) -> Result<Expr, ()> {
    let value = self.get_token_lexeme(token);
    let value = Decoder::decode_string(
      &value,
      &self.source_file.path.clone(),
      token.span,
      &mut self.engine.borrow_mut(),
    )?;

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Literal(Lit::String {
        value,
        raw_hashes: None,
      }),
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parser_raw_string(&mut self, token: &mut Token, n_hashes: usize) -> Result<Expr, ()> {
    let value = self.get_token_lexeme(token);
    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Literal(Lit::String {
        value,
        raw_hashes: Some(n_hashes),
      }),
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parser_raw_byte_string(&mut self, token: &mut Token, n_hashes: usize) -> Result<Expr, ()> {
    let value = self.get_token_lexeme(token);

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Literal(Lit::ByteString {
        value: value.bytes().collect(),
        raw_hashes: Some(n_hashes),
      }),
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parser_raw_c_string(&mut self, token: &mut Token, n_hashes: usize) -> Result<Expr, ()> {
    let value = self.get_token_lexeme(token);

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Literal(Lit::String {
        value,
        raw_hashes: Some(n_hashes),
      }),
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parser_char(&mut self, token: &mut Token) -> Result<Expr, ()> {
    let char = self.get_token_lexeme(token);
    let char = Decoder::decode_char(
      &char,
      &self.source_file.path.clone(),
      token.span,
      &mut self.engine.borrow_mut(),
    )?;

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Literal(Lit::Char(char)),
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parser_byte(&mut self, token: &mut Token) -> Result<Expr, ()> {
    let byte = self.get_token_lexeme(token);
    let byte = Decoder::decode_byte(
      &byte,
      &self.source_file.path.clone(),
      token.span,
      &mut self.engine.borrow_mut(),
    )?;

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Literal(Lit::Byte(byte)),
      span: *token.span.merge(self.current_token().span),
    })
  }

  pub(crate) fn parser_bool(&mut self) -> Result<Expr, ()> {
    let mut token = self.current_token();
    let value = self.get_token_lexeme(&token).parse::<bool>().unwrap();
    self.advance();
    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Literal(Lit::Bool(value)),
      span: *token.span.merge(self.current_token().span),
    })
  }
}
