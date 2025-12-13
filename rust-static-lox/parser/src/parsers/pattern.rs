use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
  DiagnosticEngine,
};
use lexer::token::{Token, TokenKind};

use crate::{
  ast::{path::Path, pattern::*, Expr, Mutability, QSelfHeader, RangeKind, Type},
  match_and_consume,
  parser_utils::ExprContext,
  Parser,
};

impl Parser {
  pub(crate) fn parse_pattern_with_or(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Pattern, ()> {
    let mut token = self.current_token();
    let mut patterns = vec![self.parse_pattern(context, engine)?];

    while !self.is_eof() && matches!(self.current_token().kind, TokenKind::Or) {
      self.advance(engine); // consume the '|'
      patterns.push(self.parse_pattern(context, engine)?);
    }

    if patterns.len() == 1 {
      return Ok(patterns.pop().unwrap());
    }

    Ok(Pattern::Or {
      patterns,
      span: *token.span.merge(self.current_token().span),
    })
  }

  pub(crate) fn parse_pattern(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Pattern, ()> {
    let reference = match_and_consume!(self, engine, TokenKind::KwRef)?;
    let mutability = self.parse_mutability(engine)?;

    let mut token = self.current_token();

    match token.kind {
      TokenKind::Literal { .. } | TokenKind::Ident
        if matches!(self.peek(1).kind, TokenKind::DotDot | TokenKind::DotDotEq) =>
      {
        self.parse_range_pattern(context, engine)
      },

      TokenKind::Literal { .. } | TokenKind::KwTrue | TokenKind::KwFalse | TokenKind::Minus => {
        self.parse_literal_pattern(context, engine)
      },

      TokenKind::OpenParen => self.parse_tuple_pattern(context, engine),

      TokenKind::And => self.parse_reference_pattern(context, engine),

      TokenKind::Ident
      | TokenKind::KwCrate
      | TokenKind::Lt
      | TokenKind::KwSuper
      | TokenKind::KwSelf
      | TokenKind::Dollar
      | TokenKind::ColonColon => {
        self.parse_path_or_struct_or_tuple_struct_pattern(reference, mutability, context, engine)
      },

      TokenKind::OpenBracket => self.parse_slice_pattern(context, engine),

      TokenKind::DotDot => self.parse_rest_pattern(engine),

      _ => {
        self.advance(engine);
        Ok(Pattern::Wildcard {
          span: *token.span.merge(self.current_token().span),
        })
      },
    }
  }

  fn parse_path_or_struct_or_tuple_struct_pattern(
    &mut self,
    reference: bool,
    mutability: Mutability,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Pattern, ()> {
    if self.looks_like_path_pattern() {
      return self.parse_path_based_pattern(context, engine);
    }

    // fallback: identifier binding pattern
    self.parse_identifier_binding_pattern(reference, mutability, context, engine)
  }

  fn looks_like_path_pattern(&mut self) -> bool {
    matches!(
      self.current_token().kind,
      TokenKind::ColonColon | TokenKind::Lt
    ) || matches!(
      self.peek(1).kind,
      TokenKind::ColonColon | TokenKind::OpenParen | TokenKind::OpenBrace | TokenKind::Bang
    ) || (matches!(self.current_token().kind, TokenKind::Dollar)
      && matches!(self.peek(1).kind, TokenKind::KwCrate))
  }

  fn parse_path_based_pattern(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Pattern, ()> {
    let mut start = self.current_token();

    let qself_header = self.parse_qself_type_header(engine)?;
    let path = self.parse_path(true, engine)?;

    // merge qself with path
    let (qself, path) = self.merge_qself_with_path(qself_header, path);

    // macro pattern: foo!(...)
    if match_and_consume!(self, engine, TokenKind::Bang)? {
      let mac = self.parse_macro_invocation(path, qself, engine)?;
      return Ok(Pattern::Macro { mac });
    }

    // tuple struct pattern: Foo(...)
    if match_and_consume!(self, engine, TokenKind::OpenParen)? {
      return self.parse_tuple_struct_pattern(qself, path, &mut start, context, engine);
    }

    // struct pattern: Foo { ... }
    if match_and_consume!(self, engine, TokenKind::OpenBrace)? {
      return self.parse_struct_pattern(qself, path, &mut start, context, engine);
    }

    // plain path pattern: Foo::Bar
    Ok(Pattern::Path {
      qself: None,
      path,
      span: *start.span.merge(self.current_token().span),
    })
  }

  fn merge_qself_with_path(
    &mut self,
    qself_header: Option<QSelfHeader>,
    mut path: Path,
  ) -> (Option<Box<Type>>, Path) {
    match qself_header {
      None => (None, path),
      Some(QSelfHeader { self_ty, trait_ref }) => match trait_ref {
        Some(mut trait_path) => {
          trait_path.segments.extend(path.segments);
          path.leading_colon = trait_path.leading_colon;
          (Some(self_ty), trait_path)
        },
        None => (Some(self_ty), path),
      },
    }
  }

  fn parse_tuple_struct_pattern(
    &mut self,
    qself: Option<Box<Type>>,
    path: Path,
    token: &mut Token,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Pattern, ()> {
    let mut patterns = vec![];

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::CloseParen) {
      patterns.push(self.parse_pattern(context, engine)?);
      match_and_consume!(self, engine, TokenKind::Comma)?;
    }

    self.expect(TokenKind::CloseParen, engine)?;
    Ok(Pattern::TupleStruct {
      qself,
      path,
      patterns,
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_struct_pattern(
    &mut self,
    qself: Option<Box<Type>>,
    path: Path,
    token: &mut Token,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Pattern, ()> {
    let mut fields = vec![];
    let mut has_rest = false;

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::CloseBrace) {
      if matches!(self.current_token().kind, TokenKind::DotDot) {
        has_rest = true;
        fields.push(self.parse_field_pattern(context, engine)?);
        match_and_consume!(self, engine, TokenKind::Comma)?;
        break;
      }

      fields.push(self.parse_field_pattern(context, engine)?);
      match_and_consume!(self, engine, TokenKind::Comma)?;
    }

    self.expect(TokenKind::CloseBrace, engine)?;
    Ok(Pattern::Struct {
      qself,
      path,
      fields,
      has_rest,
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_identifier_binding_pattern(
    &mut self,
    reference: bool,
    mutability: Mutability,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Pattern, ()> {
    let mut token = self.current_token();
    let name = self.get_token_lexeme(&token);

    self.advance(engine);

    let subpattern = if match_and_consume!(self, engine, TokenKind::At)? {
      Some(Box::new(self.parse_pattern(context, engine)?))
    } else {
      None
    };

    if name == "_" {
      return Ok(Pattern::Wildcard { span: token.span });
    };

    Ok(Pattern::Ident {
      binding: if reference {
        BindingMode::ByRef(mutability)
      } else {
        BindingMode::ByValue(mutability)
      },
      name,
      subpattern,
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_reference_pattern(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Pattern, ()> {
    let mut token = self.current_token();
    let mut depth = 0;
    while !self.is_eof() && matches!(self.current_token().kind, TokenKind::And) {
      depth += 1;
      self.advance(engine);
    }
    let pattern = self.parse_pattern(context, engine)?;

    Ok(Pattern::Reference {
      depth,
      pattern: Box::new(pattern),
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_range_pattern(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Pattern, ()> {
    let mut token = self.current_token();
    let (start, end, kind) = match self.parse_expression(vec![], context, engine)? {
      Expr::Range {
        start, end, kind, ..
      } => (start, end, kind),
      _ => {
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
          "expected a range expression".to_string(),
          self.source_file.path.clone(),
        )
        .with_label(
          token.span,
          Some("found a literal pattern here".to_string()),
          LabelStyle::Primary,
        )
        .with_help("a range expression is expected here".to_string());

        engine.add(diagnostic);
        return Err(());
      },
    };

    Ok(Pattern::Range {
      start,
      end,
      kind,
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_literal_pattern(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Pattern, ()> {
    let mut token = self.current_token();

    let expr = self.parse_expression(vec![], context, engine)?;
    Ok(Pattern::Literal {
      expr: Box::new(expr),
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_tuple_pattern(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Pattern, ()> {
    let mut token = self.current_token();
    self.advance(engine); // consume the token "("

    let attributes = if matches!(self.current_token().kind, TokenKind::Pound) {
      self.parse_attributes(engine)?
    } else {
      vec![]
    };

    let mut patterns = vec![];
    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::CloseParen) {
      patterns.push(self.parse_pattern(context, engine)?);
      match_and_consume!(self, engine, TokenKind::Comma)?;
    }
    self.expect(TokenKind::CloseParen, engine)?;
    Ok(Pattern::Tuple {
      patterns,
      attributes,
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_slice_pattern(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Pattern, ()> {
    let mut token = self.current_token();
    self.advance(engine); // consume the token "["
    let mut before = vec![];
    let mut after = vec![];
    let mut middle: Option<Box<Pattern>> = None;

    // Empty slice: []
    if matches!(self.current_token().kind, TokenKind::CloseBracket) {
      let close = self.current_token().span;
      self.advance(engine);
      return Ok(Pattern::Slice {
        before,
        middle,
        after,
        span: *token.span.merge(close),
      });
    }

    // Parse patterns until we hit ']'
    let mut seen_middle = false;

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::CloseBracket) {
      if self.current_token().kind == TokenKind::DotDot {
        if seen_middle {
          println!("Unexpected token \"..\"");
          return Err(());
        }

        middle = Some(Box::new(self.parse_pattern(context, engine)?));

        seen_middle = true;
        continue;
      }

      if !seen_middle {
        before.push(self.parse_pattern(context, engine)?);
      } else {
        after.push(self.parse_pattern(context, engine)?);
      }

      match_and_consume!(self, engine, TokenKind::Comma)?;

      if self.current_token().kind == TokenKind::CloseBracket {
        break;
      }
    }

    // Expect closing bracket
    self.expect(TokenKind::CloseBracket, engine)?;
    let close = self.current_token().span;

    Ok(Pattern::Slice {
      before,
      middle,
      after,
      span: *token.span.merge(close),
    })
  }

  fn parse_rest_pattern(&mut self, engine: &mut DiagnosticEngine) -> Result<Pattern, ()> {
    let span = self.current_token().span;
    self.advance(engine); // consume the token ".."
    let name = if matches!(self.current_token().kind, TokenKind::Ident) {
      let na_ = self.get_token_lexeme(&self.current_token());
      self.advance(engine);
      Some(na_)
    } else {
      None
    };

    Ok(Pattern::Rest { name, span })
  }

  fn parse_field_pattern(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<FieldPattern, ()> {
    let attributes = if matches!(self.current_token().kind, TokenKind::Pound) {
      self.parse_attributes(engine)?
    } else {
      vec![]
    };

    if matches!(self.current_token().kind, TokenKind::KwRef) {
      let mut name = String::from("");

      let mut pointer = 0;
      while !self.is_eof()
        && !matches!(
          self.peek(pointer).kind,
          TokenKind::Colon | TokenKind::CloseBrace | TokenKind::Comma
        )
      {
        let token = self.peek(pointer);
        if token.kind == TokenKind::Ident {
          let na_ = self.get_token_lexeme(&token);
          name = na_;
        }
        pointer += 1;
      }

      let pattern = Some(self.parse_pattern(context, engine)?);

      return Ok(FieldPattern {
        attributes,
        name,
        pattern,
        is_shorthand: true,
      });
    }

    if matches!(self.current_token().kind, TokenKind::DotDot) {
      let pattern = Some(self.parse_pattern(context, engine)?);
      return Ok(FieldPattern {
        attributes,
        name: "".to_string(),
        pattern,
        is_shorthand: true,
      });
    }

    let name = self.parse_name(false, engine)?;

    let (pattern, is_shorthand) = if match_and_consume!(self, engine, TokenKind::Colon)? {
      (Some(self.parse_pattern(context, engine)?), false)
    } else {
      (None, true)
    };

    Ok(FieldPattern {
      attributes,
      name,
      pattern,
      is_shorthand,
    })
  }
}
