use lexer::token::{Token, TokenKind};

use crate::{
  ast::{path::Path, pattern::*, BindingMode, ExprKind, Mutability, QSelf, Type},
  match_and_consume,
  parser_utils::ExprContext,
  Parser,
};

impl Parser {
  pub(crate) fn parse_pattern_with_or(&mut self, context: ExprContext) -> Result<Pattern, ()> {
    let mut token = self.current_token();
    let mut patterns = vec![self.parse_pattern(context)?];

    while matches!(self.current_token().kind, TokenKind::Or) {
      self.advance();
      patterns.push(self.parse_pattern(context)?);
    }

    if patterns.len() == 1 {
      return Ok(patterns.pop().unwrap());
    }

    Ok(Pattern::Or {
      patterns,
      span: *token.span.merge(self.current_token().span),
    })
  }

  pub(crate) fn parse_pattern(&mut self, context: ExprContext) -> Result<Pattern, ()> {
    let reference = match_and_consume!(self, TokenKind::KwRef)?;
    let mutability = self.parse_mutability()?;
    let mut token = self.current_token();

    match token.kind {
      TokenKind::Literal { .. } | TokenKind::Ident
        if matches!(self.peek(1).kind, TokenKind::DotDot | TokenKind::DotDotEq) =>
      {
        self.parse_range_pattern(context)
      },

      TokenKind::Literal { .. } | TokenKind::KwTrue | TokenKind::KwFalse | TokenKind::Minus => {
        self.parse_literal_pattern(context)
      },

      TokenKind::LParen => self.parse_tuple_or_group_pattern(context),

      TokenKind::Amp => self.parse_reference_pattern(context),

      TokenKind::Ident
      | TokenKind::KwCrate
      | TokenKind::Lt
      | TokenKind::KwSuper
      | TokenKind::KwSelf
      | TokenKind::Dollar
      | TokenKind::ColonColon => self.parse_path_or_binding_pattern(reference, mutability, context),

      TokenKind::LBracket => self.parse_slice_pattern(context),

      TokenKind::DotDot => self.parse_rest_pattern(),

      _ => {
        self.advance();
        Ok(Pattern::Wildcard {
          span: *token.span.merge(self.current_token().span),
        })
      },
    }
  }

  fn parse_path_or_binding_pattern(
    &mut self,
    reference: bool,
    mutability: Mutability,
    context: ExprContext,
  ) -> Result<Pattern, ()> {
    if self.looks_like_path_pattern() {
      return self.parse_path_based_pattern(context);
    }

    self.parse_identifier_binding_pattern(reference, mutability, context)
  }

  fn looks_like_path_pattern(&mut self) -> bool {
    matches!(
      self.current_token().kind,
      TokenKind::ColonColon | TokenKind::Lt
    ) || matches!(
      self.peek(1).kind,
      TokenKind::ColonColon | TokenKind::LParen | TokenKind::LBrace | TokenKind::Bang
    ) || (matches!(self.current_token().kind, TokenKind::Dollar)
      && matches!(self.peek(1).kind, TokenKind::KwCrate))
  }

  fn parse_path_based_pattern(&mut self, context: ExprContext) -> Result<Pattern, ()> {
    let mut start = self.current_token();

    let qself_header = self.parse_qself_type_header(context)?;
    let path = self.parse_path(true, context)?;
    let (qself, path) = self.merge_qself_with_path(qself_header, path);

    if match_and_consume!(self, TokenKind::Bang)? {
      return Ok(Pattern::Macro {
        path,
        span: *start.span.merge(self.current_token().span),
      });
    }

    if match_and_consume!(self, TokenKind::LParen)? {
      return self.parse_tuple_struct_pattern(qself, path, &mut start, context);
    }

    if match_and_consume!(self, TokenKind::LBrace)? {
      return self.parse_struct_pattern(qself, path, &mut start, context);
    }

    Ok(Pattern::Path {
      qself,
      path,
      span: *start.span.merge(self.current_token().span),
    })
  }

  fn merge_qself_with_path(
    &mut self,
    qself_header: QSelf,
    mut path: Path,
  ) -> (Option<Box<Type>>, Path) {
    let QSelf { self_ty, as_trait } = qself_header;
    match as_trait {
      Some(mut trait_path) => {
        trait_path.segments.extend(path.segments);
        path.leading_colon = trait_path.leading_colon;
        (Some(self_ty), trait_path)
      },
      None => (Some(self_ty), path),
    }
  }

  fn parse_tuple_struct_pattern(
    &mut self,
    qself: Option<Box<Type>>,
    path: Path,
    start: &mut Token,
    context: ExprContext,
  ) -> Result<Pattern, ()> {
    let mut patterns = vec![];

    while !matches!(self.current_token().kind, TokenKind::RParen) {
      patterns.push(self.parse_pattern(context)?);
      match_and_consume!(self, TokenKind::Comma)?;
    }

    self.expect(TokenKind::RParen)?;

    Ok(Pattern::TupleStruct {
      qself,
      path,
      patterns,
      span: *start.span.merge(self.current_token().span),
    })
  }

  fn parse_struct_pattern(
    &mut self,
    qself: Option<Box<Type>>,
    path: Path,
    start: &mut Token,
    context: ExprContext,
  ) -> Result<Pattern, ()> {
    let mut fields = vec![];
    let mut has_rest = false;

    while !matches!(self.current_token().kind, TokenKind::RBrace) {
      if matches!(self.current_token().kind, TokenKind::DotDot) {
        has_rest = true;
        self.advance();
        match_and_consume!(self, TokenKind::Comma)?;
        break;
      }

      fields.push(self.parse_field_pattern(context)?);
      match_and_consume!(self, TokenKind::Comma)?;
    }

    self.expect(TokenKind::RBrace)?;

    Ok(Pattern::Struct {
      qself,
      path,
      fields,
      has_rest,
      span: *start.span.merge(self.current_token().span),
    })
  }

  fn parse_identifier_binding_pattern(
    &mut self,
    reference: bool,
    mutability: Mutability,
    context: ExprContext,
  ) -> Result<Pattern, ()> {
    let mut token = self.current_token();
    let name = self.get_token_lexeme(&token);

    self.advance();

    let subpattern = if match_and_consume!(self, TokenKind::At)? {
      Some(Box::new(self.parse_pattern(context)?))
    } else {
      None
    };

    if name == "_" {
      return Ok(Pattern::Wildcard { span: token.span });
    }

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

  fn parse_reference_pattern(&mut self, context: ExprContext) -> Result<Pattern, ()> {
    let mut token = self.current_token();
    let mut depth = 0;

    while matches!(self.current_token().kind, TokenKind::Amp) {
      depth += 1;
      self.advance();
    }

    let mutability = self.parse_mutability()?;
    let pattern = self.parse_pattern(context)?;

    Ok(Pattern::Reference {
      depth,
      mutability,
      pattern: Box::new(pattern),
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_range_pattern(&mut self, context: ExprContext) -> Result<Pattern, ()> {
    let mut token = self.current_token();
    let expr = self.parse_expression(vec![], context)?;

    let (start, end, kind) = match &expr.kind {
      ExprKind::Range { start, end, kind } => {
        let kind = match kind {
          crate::ast::RangeExprKind::Exclusive => RangeKind::Exclusive,
          crate::ast::RangeExprKind::Inclusive => RangeKind::Inclusive,
          crate::ast::RangeExprKind::From => RangeKind::From,
          crate::ast::RangeExprKind::To => RangeKind::To,
          crate::ast::RangeExprKind::ToInclusive => RangeKind::ToInclusive,
          crate::ast::RangeExprKind::Full => RangeKind::Full,
          crate::ast::RangeExprKind::FromInclusive => todo!(),
        };
        (start.clone(), end.clone(), kind)
      },
      _ => {
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

  fn parse_literal_pattern(&mut self, context: ExprContext) -> Result<Pattern, ()> {
    let mut token = self.current_token();
    let expr = self.parse_expression(vec![], context)?;

    Ok(Pattern::Literal {
      expr: Box::new(expr),
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_tuple_or_group_pattern(&mut self, context: ExprContext) -> Result<Pattern, ()> {
    let mut token = self.current_token();
    self.advance();

    let mut patterns = vec![];
    while !matches!(self.current_token().kind, TokenKind::RParen) {
      patterns.push(self.parse_pattern(context)?);
      match_and_consume!(self, TokenKind::Comma)?;
    }

    self.expect(TokenKind::RParen)?;

    if patterns.len() == 1 {
      Ok(Pattern::Group {
        pattern: Box::new(patterns.pop().unwrap()),
        span: *token.span.merge(self.current_token().span),
      })
    } else {
      Ok(Pattern::Tuple {
        patterns,
        span: *token.span.merge(self.current_token().span),
      })
    }
  }

  fn parse_slice_pattern(&mut self, context: ExprContext) -> Result<Pattern, ()> {
    let mut token = self.current_token();
    self.advance();

    let mut before = vec![];
    let mut after = vec![];
    let mut has_rest = false;

    while !matches!(self.current_token().kind, TokenKind::RBracket) {
      if matches!(self.current_token().kind, TokenKind::DotDot) {
        has_rest = true;
        self.advance();
        match_and_consume!(self, TokenKind::Comma)?;
        continue;
      }

      if !has_rest {
        before.push(self.parse_pattern(context)?);
      } else {
        after.push(self.parse_pattern(context)?);
      }

      match_and_consume!(self, TokenKind::Comma)?;
    }

    self.expect(TokenKind::RBracket)?;

    Ok(Pattern::Slice {
      before,
      has_rest,
      after,
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_rest_pattern(&mut self) -> Result<Pattern, ()> {
    let span = self.current_token().span;
    self.advance();
    Ok(Pattern::Rest { span })
  }

  fn parse_field_pattern(&mut self, context: ExprContext) -> Result<FieldPattern, ()> {
    let mut token = self.current_token();
    let attributes = if matches!(self.current_token().kind, TokenKind::Pound) {
      self.parse_outer_attributes(context)?
    } else {
      vec![]
    };

    let name = self.parse_name(false)?;

    let (pattern, is_shorthand) = if match_and_consume!(self, TokenKind::Colon)? {
      (Some(self.parse_pattern(context)?), false)
    } else {
      (None, true)
    };

    Ok(FieldPattern {
      attributes,
      name,
      pattern,
      is_shorthand,
      span: *token.span.merge(self.current_token().span),
    })
  }
}
