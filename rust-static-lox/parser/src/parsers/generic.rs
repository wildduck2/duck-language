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
        let diagnostic = self
          .diagnostic(
            DiagnosticError::UnexpectedToken,
            format!("expected type, lifetime, or const parameter, found `{lexeme}`"),
          )
          .with_label(
            token.span,
            Some("expected a type, lifetime, or const parameter here".to_string()),
            LabelStyle::Primary,
          )
          .with_note(format!("unexpected token: `{lexeme}`"))
          .with_help("use `T`, `'a`, or `const N: Type` here".to_string());
        self.emit(diagnostic);
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
          let diagnostic = self
            .diagnostic(
              DiagnosticError::InvalidTraitBoundModifier,
              format!("expected `const` after `~`, found `{found}`"),
            )
            .with_label(
              self.current_token().span,
              Some("`~` must be followed by `const` in trait bounds".to_string()),
              LabelStyle::Primary,
            )
            .with_help("use `~const Trait` or remove the modifier".to_string());
          self.emit(diagnostic);
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
            let diagnostic = self
              .diagnostic(
                DiagnosticError::InvalidTraitBoundModifier,
                format!("expected `const` after `?~`, found `{found}`"),
              )
              .with_label(
                self.current_token().span,
                Some("`?~` must be followed by `const` in trait bounds".to_string()),
                LabelStyle::Primary,
              )
              .with_help("use `?~const Trait` or remove the modifier".to_string());
            self.emit(diagnostic);
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
            let diagnostic = self
              .diagnostic(
                DiagnosticError::InvalidTrailingComma,
                "trailing comma not allowed in generic argument list",
              )
              .with_label(
                token.span,
                Some("remove this trailing comma".to_string()),
                LabelStyle::Primary,
              )
              .with_note("trailing commas are not permitted in generic argument list".to_string())
              .with_help("remove the trailing comma or add another element".to_string());
            self.emit(diagnostic);
            return Err(());
          }
        }

        if inputs.is_empty() {
          let diagnostic = self
            .diagnostic(
              DiagnosticError::EmptyGenericArgs,
              "empty generic arguments list",
            )
            .with_label(
              token.span,
              Some("generic arguments list cannot be empty".to_string()),
              LabelStyle::Primary,
            )
            .with_help("remove the parentheses or add input types".to_string());
          self.emit(diagnostic);
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
        let found = self.get_token_lexeme(&token);
        let diagnostic = self
          .diagnostic(
            DiagnosticError::UnexpectedToken,
            format!("expected generic argument type, found `{found}`"),
          )
          .with_label(
            *token.span.merge(self.last_token_span()),
            Some("expected a valid generic argument type here".to_string()),
            LabelStyle::Primary,
          )
          .with_help("use a named type, lifetime, or const expression as a generic argument".to_string());
        self.emit(diagnostic);
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
        let diagnostic = self
          .diagnostic(
            DiagnosticError::UnexpectedToken,
            format!("expected generic argument, found `{found}`"),
          )
          .with_label(
            bad.span,
            Some("expected a generic argument here".to_string()),
            LabelStyle::Primary,
          )
          .with_note(format!("unexpected token: `{found}`"))
          .with_help("add a type, lifetime, or const expression".to_string());
        self.emit(diagnostic);
        return Err(());
      }

      args.push(self.parse_generic_arg(context)?);
      let token = self.current_token();

      if !matches!(self.current_token().kind, TokenKind::Comma | TokenKind::Gt) {
        let bad = self.current_token();
        let found = self.get_token_lexeme(&bad);
        let details = format!("expected ',' or '>', found `{found}`");
        let diagnostic = self
          .diagnostic(DiagnosticError::InvalidComma, "unexpected comma".to_string())
          .with_label(bad.span, Some(details), LabelStyle::Primary)
          .with_help("remove the comma or add a valid element after it".to_string());
        self.emit(diagnostic);
        return Err(());
      }

      match_and_consume!(self, TokenKind::Comma)?;

      if matches!(self.current_token().kind, TokenKind::Gt)
        && matches!(token.kind, TokenKind::Comma)
      {
        let diagnostic = self
          .diagnostic(
            DiagnosticError::InvalidTrailingComma,
            "trailing comma not allowed in generic argument list",
          )
          .with_label(
            token.span,
            Some("remove this trailing comma".to_string()),
            LabelStyle::Primary,
          )
          .with_note("trailing commas are not permitted in generic argument list".to_string())
          .with_help("remove the trailing comma or add another element".to_string());
        self.emit(diagnostic);
        return Err(());
      }
    }

    if args.is_empty() {
      let diagnostic = self
        .diagnostic(
          DiagnosticError::EmptyGenericArgs,
          "empty generic arguments list",
        )
        .with_label(
          token.span,
          Some("generic arguments list cannot be empty".to_string()),
          LabelStyle::Primary,
        )
        .with_help("remove the angle brackets `<>` or add type parameters".to_string());
      self.emit(diagnostic);
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
      let diagnostic = self
        .diagnostic(
          DiagnosticError::UnexpectedToken,
          format!("trailing `+` in {context} bounds"),
        )
        .with_label(
          prev.span,
          Some("remove this trailing `+` or add another bound".to_string()),
          LabelStyle::Primary,
        )
        .with_help("write bounds like `Clone + Copy` without a dangling `+`".to_string());
      self.emit(diagnostic);
      return Err(());
    }

    if !is_valid_start(&next.kind) {
      let diagnostic = self
        .diagnostic(
          DiagnosticError::UnexpectedToken,
          format!("expected another bound after `+` in {context}"),
        )
        .with_label(
          self.peek_prev(0).span,
          Some("add a bound after `+`, e.g. `Trait` or `'a`".to_string()),
          LabelStyle::Primary,
        )
        .with_note("bounds are separated by `+`, such as `Clone + 'a`".to_string());
      self.emit(diagnostic);
      return Err(());
    }

    Ok(true)
  }
}
