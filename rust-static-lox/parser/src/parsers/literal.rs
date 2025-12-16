use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
  DiagnosticEngine,
};
use lexer::token::{LiteralKind, Token};

use crate::{
  ast::{Expr, ExprKind, Lit},
  Parser,
};

impl Parser {
  pub(crate) fn parser_literal(
    &mut self,
    kind: LiteralKind,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();
    self.advance(engine);

    match kind {
      LiteralKind::Integer {
        suffix_start,
        base,
        empty_int,
      } => self.parser_integer(engine, &mut token, empty_int, suffix_start, base),

      LiteralKind::Float { suffix_start, base } => {
        self.parser_float(engine, &mut token, suffix_start, base)
      },

      LiteralKind::Str => self.parser_string(engine, &mut token),

      LiteralKind::ByteStr => self.parser_byte_string(engine, &mut token),

      LiteralKind::CStr => self.parser_c_string(engine, &mut token),

      LiteralKind::RawStr { n_hashes } => self.parser_raw_string(engine, &mut token, n_hashes),

      LiteralKind::RawByteStr { n_hashes } => {
        self.parser_raw_byte_string(engine, &mut token, n_hashes)
      },

      LiteralKind::RawCStr { n_hashes } => self.parser_raw_c_string(engine, &mut token, n_hashes),

      LiteralKind::Char => self.parser_char(engine, &mut token),

      LiteralKind::Byte => self.parser_byte(engine, &mut token),
    }
  }

  fn parser_integer(
    &mut self,
    engine: &mut DiagnosticEngine,
    token: &mut Token,
    empty_int: bool,
    suffix_start: usize,
    base: lexer::token::Base,
  ) -> Result<Expr, ()> {
    if empty_int {
      let diagnostic = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::InvalidLiteral),
        "Invalid integer literal".into(),
        self.source_file.path.clone(),
      )
      .with_label(
        token.span,
        Some("Integer literal has no digits".into()),
        LabelStyle::Primary,
      );
      engine.add(diagnostic);
      return Err(());
    }

    // Extract suffix + numeric portion
    let src = &self.source_file.src;

    let suffix = Some(src[suffix_start..token.span.end].to_string());
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
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidLiteral),
          "Invalid integer literal".into(),
          self.source_file.path.clone(),
        )
        .with_label(
          token.span,
          Some("Integer literal is too large or malformed".into()),
          LabelStyle::Primary,
        );
        engine.add(diagnostic);
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
    engine: &mut DiagnosticEngine,
    token: &mut Token,
    suffix_start: usize,
    base: lexer::token::Base,
  ) -> Result<Expr, ()> {
    let src = &self.source_file.src;
    let suffix = Some(src[suffix_start..token.span.end].to_string());
    let value_str = &src[token.span.start..suffix_start];

    let parsed = match base {
      lexer::token::Base::Decimal => value_str.replace('_', "").parse::<f64>(),
      _ => Err(())?, // hex/other floats disallowed
    };

    let value = match parsed {
      Ok(v) => v,
      Err(_) => {
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidLiteral),
          "Invalid float literal".into(),
          self.source_file.path.clone(),
        )
        .with_label(
          token.span,
          Some("Float literal is too large or malformed".into()),
          LabelStyle::Primary,
        );
        engine.add(diagnostic);
        return Err(());
      },
    };

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Literal(Lit::Float { value, suffix }),
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parser_string(
    &mut self,
    _engine: &mut DiagnosticEngine,
    token: &mut Token,
  ) -> Result<Expr, ()> {
    let raw = &self.source_file.src[token.span.start..token.span.end];

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Literal(Lit::String {
        value: raw.into(),
        raw_hashes: None,
      }),
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parser_byte_string(
    &mut self,
    _engine: &mut DiagnosticEngine,
    token: &mut Token,
  ) -> Result<Expr, ()> {
    let raw = &self.source_file.src[token.span.start..token.span.end];

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Literal(Lit::String {
        value: raw.into(),
        raw_hashes: None,
      }),
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parser_c_string(
    &mut self,
    _engine: &mut DiagnosticEngine,
    token: &mut Token,
  ) -> Result<Expr, ()> {
    let raw = &self.source_file.src[token.span.start..token.span.end];
    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Literal(Lit::String {
        value: raw.into(),
        raw_hashes: None,
      }),
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parser_raw_string(
    &mut self,
    _engine: &mut DiagnosticEngine,
    token: &mut Token,
    n_hashes: usize,
  ) -> Result<Expr, ()> {
    let raw = &self.source_file.src[token.span.start..token.span.end];
    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Literal(Lit::String {
        value: raw.into(),
        raw_hashes: Some(n_hashes),
      }),
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parser_raw_byte_string(
    &mut self,
    _engine: &mut DiagnosticEngine,
    token: &mut Token,
    n_hashes: usize,
  ) -> Result<Expr, ()> {
    let raw = &self.source_file.src[token.span.start..token.span.end];

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Literal(Lit::String {
        value: raw.into(),
        raw_hashes: Some(n_hashes),
      }),
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parser_raw_c_string(
    &mut self,
    _engine: &mut DiagnosticEngine,
    token: &mut Token,
    n_hashes: usize,
  ) -> Result<Expr, ()> {
    let raw = &self.source_file.src[token.span.start..token.span.end];
    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Literal(Lit::String {
        value: raw.into(),
        raw_hashes: Some(n_hashes),
      }),
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parser_char(&mut self, _engine: &mut DiagnosticEngine, token: &mut Token) -> Result<Expr, ()> {
    let ch = self.source_file.src[token.span.start + 1..token.span.end]
      .chars()
      .next()
      .unwrap();

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Literal(Lit::Char(ch)),
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parser_byte(&mut self, engine: &mut DiagnosticEngine, token: &mut Token) -> Result<Expr, ()> {
    if token.span.start == token.span.end - 1 {
      let diag = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::EmptyChar),
        "Invalid byte literal".into(),
        self.source_file.path.clone(),
      )
      .with_label(
        token.span,
        Some("Byte literal is malformed".into()),
        LabelStyle::Primary,
      );
      engine.add(diag);
      return Err(());
    }

    let byte = self.source_file.src[token.span.start + 2..token.span.end]
      .chars()
      .next()
      .unwrap()
      .to_string()
      .as_bytes()[0];

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Literal(Lit::Byte(byte)),
      span: *token.span.merge(self.current_token().span),
    })
  }

  pub(crate) fn parser_bool(&mut self, engine: &mut DiagnosticEngine) -> Result<Expr, ()> {
    let mut token = self.current_token();
    let value = self.get_token_lexeme(&token).parse::<bool>().unwrap();
    self.advance(engine);
    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Literal(Lit::Bool(value)),
      span: *token.span.merge(self.current_token().span),
    })
  }
}
