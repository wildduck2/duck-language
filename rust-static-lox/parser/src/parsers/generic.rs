use crate::{
  ast::{generic::*, Path, PathSegment, PathSegmentKind, Type},
  match_and_consume,
  parser_utils::ParserContext,
  Parser,
};
use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::{Token, TokenKind};

impl Parser {
  pub(crate) fn parse_generic_params(
    &mut self,
    token: &mut Token,
    context: ParserContext,
  ) -> Result<Option<GenericParams>, ()> {
    if !matches!(self.current_token().kind, TokenKind::Lt) {
      return Ok(None);
    }

    let mut params = Vec::<GenericParam>::new();

    self.expect(TokenKind::Lt)?; // consume the "<"

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::Gt) {
      params.push(self.parse_generic_param(context)?);
      match_and_consume!(self, TokenKind::Comma)?;
    }

    self.expect(TokenKind::Gt)?; // consume the ">"

    token.span.merge(self.last_token_span());
    Ok(Some(GenericParams {
      params,
      span: token.span,
    }))
  }

  /// Parses a single generic parameter (type, lifetime, or const).
  pub(crate) fn parse_generic_param(&mut self, context: ParserContext) -> Result<GenericParam, ()> {
    let attributes = self.parse_outer_attributes(context)?;
    let token = self.current_token();

    match token.kind {
      // const generic: const N: usize = 3
      TokenKind::KwConst => {
        self.advance(); // consume "const"
        let name = self.parse_name(false)?;

        self.expect(TokenKind::Colon)?; // must have ":"
        let ty = self.parse_type(context)?;

        let default = if matches!(self.current_token().kind, TokenKind::Eq) {
          self.advance();
          Some(self.parse_expression(vec![], context)?)
        } else {
          None
        };

        Ok(GenericParam::Const {
          attributes,
          name,
          ty,
          default,
        })
      },

      // lifetime generic: 'a or 'a: 'b + 'c
      TokenKind::Lifetime { .. } => {
        let mut bounds = vec![];
        let name = self.get_token_lexeme(&self.current_token());
        self.advance(); // consume the lifetime

        if matches!(self.current_token().kind, TokenKind::Colon) {
          self.advance(); // consume the colon
          while !self.is_eof() && !Self::is_bound_terminator(&self.current_token().kind) {
            let name = self.get_token_lexeme(&self.current_token());
            self.advance(); // consume the lifetime
            bounds.push(name);

            if !self
              .consume_plus_and_require_bound("lifetime bounds", Self::is_path_start_or_lifetime)?
            {
              break;
            }
          }
        }

        Ok(GenericParam::Lifetime {
          attributes,
          name,
          bounds,
        })
      },

      // type generic: T, U: Bound, T = Default
      TokenKind::Ident => {
        let name = self.parse_name(false)?;
        let bounds = self.parse_trait_bounds("generic parameter", context)?;

        let default = if matches!(self.current_token().kind, TokenKind::Eq) {
          self.advance();
          Some(self.parse_type(context)?)
        } else {
          None
        };

        Ok(GenericParam::Type {
          attributes,
          name,
          bounds,
          default,
        })
      },

      _ => {
        let lexeme = self.get_token_lexeme(&token);
        self.emit(self.err_unexpected_token(
          token.span,
          "type, lifetime, or const parameter",
          &lexeme,
        ));
        Err(())
      },
    }
  }

  /// Parses either lifetime or trait bounds that follow a colon.
  pub(crate) fn parse_trait_bounds(
    &mut self,
    context_text: &str,
    context: ParserContext,
  ) -> Result<Vec<TypeBound>, ()> {
    let mut bounds = vec![];
    if !matches!(self.current_token().kind, TokenKind::Colon) {
      return Ok(bounds);
    }

    self.expect(TokenKind::Colon)?;

    while !self.is_eof() && !Self::is_bound_terminator(&self.current_token().kind) {
      match self.current_token().kind {
        TokenKind::Lifetime { .. } => {
          let name = self.get_token_lexeme(&self.current_token());
          self.advance(); // consume the lifetime
          bounds.push(TypeBound::Lifetime { name });
        },
        _ => {
          let modifier = self.parse_trait_bound_modifier()?;

          let for_lifetimes = self.parse_for_lifetimes()?;
          let path = self.parse_path(false, context)?;

          bounds.push(TypeBound::Trait {
            modifier,
            path,
            for_lifetimes,
          });
        },
      }

      self.consume_plus_and_require_bound(context_text, Self::is_bound_start)?;
    }

    Ok(bounds)
  }

  fn parse_trait_bound_modifier(&mut self) -> Result<TraitBoundModifier, ()> {
    match self.current_token().kind {
      TokenKind::Tilde => {
        self.advance(); // consume the "~"

        if matches!(self.current_token().kind, TokenKind::KwConst) {
          self.advance(); // consume the "const"
          Ok(TraitBoundModifier::Const)
        } else {
          let found = self.get_token_lexeme(&self.current_token());
          self.emit(
            self.err_invalid_trait_bound_modifier(self.current_token().span, &format!("~{found}")),
          );
          Err(())
        }
      },
      TokenKind::Question => {
        self.advance(); // consume the "?"

        if matches!(self.current_token().kind, TokenKind::Tilde) {
          self.advance(); // consume the "~"

          if matches!(self.current_token().kind, TokenKind::KwConst) {
            // (e.g., `~const Clone`)
            self.advance(); // consume the "const"
            Ok(TraitBoundModifier::MaybeConst)
          } else {
            let found = self.get_token_lexeme(&self.current_token());
            self.emit(
              self
                .err_invalid_trait_bound_modifier(self.current_token().span, &format!("?~{found}")),
            );
            Err(())
          }
        } else {
          // (e.g., `?Clone`)
          Ok(TraitBoundModifier::Maybe)
        }
      },
      _ => Ok(TraitBoundModifier::None),
    }
  }

  pub(crate) fn parse_generic_args(
    &mut self,
    context: ParserContext,
  ) -> Result<Option<GenericArgs>, ()> {
    match self.current_token().kind {
      TokenKind::LParen => {
        let token = self.current_token();
        let mut inputs = Vec::<Type>::new();
        self.advance();

        while !self.is_eof() && self.current_token().kind != TokenKind::RParen {
          inputs.push(self.parse_type(context)?);
          let token = self.current_token();
          match_and_consume!(self, TokenKind::Comma)?;

          if matches!(self.current_token().kind, TokenKind::RParen)
            && matches!(token.kind, TokenKind::Comma)
          {
            self.emit(self.err_invalid_trailing_comma(token.span, "generic argument list"));
            return Err(());
          }
        }

        if inputs.is_empty() {
          self.emit(self.err_empty_generic_args(token.span));
          return Err(());
        }

        self.expect(TokenKind::RParen)?;
        self.expect(TokenKind::ThinArrow)?;
        let output = self.parse_type(context)?;

        Ok(Some(GenericArgs::Parenthesized {
          inputs,
          output: Some(Box::new(output)),
        }))
      },

      TokenKind::Lt | TokenKind::ColonColon => {
        let args = self.parse_angle_bracketed_generic_args_inner(context)?;
        Ok(Some(GenericArgs::AngleBracketed { args }))
      },

      _ => Ok(None),
    }
  }

  /// Parses a single generic argument (type, lifetime, const, binding, …).
  pub(crate) fn parse_generic_arg(&mut self, context: ParserContext) -> Result<GenericArg, ()> {
    let mut token = self.current_token();
    let name = self.get_token_lexeme(&token);

    // 1. lifetime argument
    if let TokenKind::Lifetime { .. } = token.kind {
      self.advance();
      return Ok(GenericArg::Lifetime(name));
    }

    // 2. const generic argument
    if matches!(token.kind, TokenKind::Literal { .. } | TokenKind::LBrace) {
      let expr = self.parse_expression(vec![], ParserContext::Default)?;
      return Ok(GenericArg::Const(expr));
    }

    // 3. parse a full type first
    // println!("type: {:?}", self.current_token().kind);
    let r#type = self.parse_type(context)?;

    // reject invalid types in generic args
    match r#type {
      Type::Unit | Type::Slice { .. } => {
        let found = self.get_token_lexeme(&self.current_token());
        self.emit(self.err_unexpected_token(
          *token.span.merge(self.last_token_span()),
          "valid generic argument",
          &found,
        ));
        return Err(());
      },
      _ => {},
    }

    match self.current_token().kind {
      TokenKind::Eq => {
        self.advance();
        let (name, args) = self.extract_assoc_head(r#type)?;

        if let Some(args) = &args {
          match &args {
            GenericArgs::AngleBracketed { args } if !args.is_empty() => {
              let diagnostic = self.diagnostic(
                DiagnosticError::InvalidGenericArg,
                "generic arguments are not allowed on associated type bindings",
              )
              .with_label(
                *token.span.merge(self.last_token_span()),
                Some(
                  "associated type bindings must use a bare identifier like `Item = Type` or `Assoc: Trait`"
                    .to_string(),
                ),
                LabelStyle::Primary,
              )
              .with_help(
                "remove the generic arguments or move this constraint into a where clause"
                  .to_string(),
              );
              self.emit(diagnostic);
              return Err(());
            },
            _ => {
              panic!("have not implemented this case yet");
            },
          }
        }

        let ty = self.parse_type(context)?;
        Ok(GenericArg::Binding { name, args, ty })
      },

      TokenKind::Colon => {
        let (name, args) = self.extract_assoc_head(r#type)?;
        let bounds = self.parse_trait_bounds("generic argument", context)?;
        Ok(GenericArg::Constraint { name, args, bounds })
      },

      _ => Ok(GenericArg::Type(r#type)),
    }
  }

  fn extract_assoc_head(&mut self, ty: Type) -> Result<(String, Option<GenericArgs>), ()> {
    match ty {
      Type::Path(Path { segments, .. }) => {
        let PathSegment { args, kind } = segments[0].clone();
        let name = match kind {
          PathSegmentKind::Ident(name) => name,
          _ => return Err(()),
        };

        Ok((name, args))
      },
      _ => Err(()),
    }
  }

  fn can_start_generic_arg(kind: &TokenKind) -> bool {
    matches!(
      kind,
      // lifetimes
      TokenKind::Lifetime { .. }

        // type starts
        | TokenKind::Ident
        | TokenKind::KwSelf
        | TokenKind::KwSelfType
        | TokenKind::KwSuper
        | TokenKind::KwCrate
        | TokenKind::ColonColon
        | TokenKind::Lt
        | TokenKind::LParen
        | TokenKind::Amp
        | TokenKind::Star
        | TokenKind::LBracket

        // const generic starts
        | TokenKind::Literal { .. }
        | TokenKind::Minus
    )
  }

  fn parse_angle_bracketed_generic_args_inner(
    &mut self,
    context: ParserContext,
  ) -> Result<Vec<GenericArg>, ()> {
    let token = self.current_token();
    let mut args = Vec::<GenericArg>::new();

    self.expect(TokenKind::Lt)?;

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::Gt) {
      if !Self::can_start_generic_arg(&self.current_token().kind) {
        let bad = self.current_token();
        let found = self.get_token_lexeme(&bad);
        self.emit(self.err_unexpected_token(bad.span, "generic argument", &found));
        return Err(());
      }

      args.push(self.parse_generic_arg(context)?);
      let token = self.current_token();

      if !matches!(self.current_token().kind, TokenKind::Comma | TokenKind::Gt) {
        let bad = self.current_token();
        let found = self.get_token_lexeme(&bad);
        self
          .emit(self.err_invalid_comma(bad.span, &format!("expected ',' or '>', found `{found}`")));
        return Err(());
      }

      match_and_consume!(self, TokenKind::Comma)?;

      if matches!(self.current_token().kind, TokenKind::Gt)
        && matches!(token.kind, TokenKind::Comma)
      {
        self.emit(self.err_invalid_trailing_comma(token.span, "generic argument list"));
        return Err(());
      }
    }

    if args.is_empty() {
      self.emit(self.err_empty_generic_args(token.span));
      return Err(());
    }

    self.expect(TokenKind::Gt)?;
    Ok(args)
  }

  pub(crate) fn is_bound_start(kind: &TokenKind) -> bool {
    matches!(
      kind,
      TokenKind::Lifetime { .. }
        | TokenKind::Tilde
        | TokenKind::Question
        | TokenKind::KwFor
        | TokenKind::ColonColon
        | TokenKind::Dollar
        | TokenKind::KwCrate
        | TokenKind::KwSelf
        | TokenKind::KwSelfType
        | TokenKind::KwSuper
        | TokenKind::Ident
    )
  }

  pub(crate) fn is_path_start_or_lifetime(kind: &TokenKind) -> bool {
    matches!(kind, TokenKind::Lifetime { .. }) || kind.can_start_path()
  }

  // pub(crate) fn is_lifetime_start(kind: &TokenKind) -> bool {
  //
  // }

  pub(crate) fn is_bound_terminator(kind: &TokenKind) -> bool {
    matches!(
      kind,
      TokenKind::LBrace
        | TokenKind::RBrace
        | TokenKind::Comma
        | TokenKind::Gt
        | TokenKind::Eq
        | TokenKind::RParen
        | TokenKind::Semi
        | TokenKind::Eof
        | TokenKind::KwWhere
    )
  }

  pub(crate) fn consume_plus_and_require_bound<F>(
    &mut self,
    context: &str,
    is_valid_start: F,
  ) -> Result<bool, ()>
  where
    F: Fn(&TokenKind) -> bool,
  {
    let consumed_plus = match_and_consume!(self, TokenKind::Plus)?;

    if !consumed_plus {
      return Ok(false);
    }

    let next = self.current_token();
    if Self::is_bound_terminator(&next.kind) {
      let prev = self.peek_prev(0);
      self.emit(self.err_trailing_plus_in_bounds(prev.span, context));
      return Err(());
    }

    if !is_valid_start(&next.kind) {
      self.emit(self.err_expected_bound_after_plus(self.peek_prev(0).span, context));
      return Err(());
    }

    Ok(true)
  }
}
