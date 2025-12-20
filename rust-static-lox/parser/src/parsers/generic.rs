use crate::{
  ast::{generic::*, Type},
  match_and_consume,
  parser_utils::ExprContext,
  Diagnostic, Parser,
};
use diagnostic::{code::DiagnosticCode, diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::{Token, TokenKind};

impl Parser {
  pub fn parse_generic_params(&mut self, token: &mut Token) -> Result<Option<GenericParams>, ()> {
    if !matches!(self.current_token().kind, TokenKind::Lt) {
      return Ok(None);
    }

    let mut params = Vec::<GenericParam>::new();

    self.expect(TokenKind::Lt)?; // consume the "<"

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::Gt) {
      params.push(self.parse_generic_param()?);
      match_and_consume!(self, TokenKind::Comma)?;
    }

    self.expect(TokenKind::Gt)?; // consume the ">"

    token.span.merge(self.current_token().span);
    Ok(Some(GenericParams {
      params,
      span: token.span,
    }))
  }

  /// Parses a single generic parameter (type, lifetime, or const).
  pub fn parse_generic_param(&mut self) -> Result<GenericParam, ()> {
    let attributes = self.parse_outer_attributes()?;
    let token = self.current_token();

    match token.kind {
      // const generic: const N: usize = 3
      TokenKind::KwConst => {
        self.advance(); // consume "const"
        let name = self.parse_name(false)?;

        self.expect(TokenKind::Colon)?; // must have ":"
        let ty = self.parse_type()?;

        let default = if matches!(self.current_token().kind, TokenKind::Eq) {
          self.advance();
          Some(self.parse_expression(vec![], ExprContext::Default)?)
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
          while !self.is_eof()
            && !matches!(
              self.current_token().kind,
              TokenKind::OpenBrace | TokenKind::Comma | TokenKind::Gt
            )
          {
            let name = self.get_token_lexeme(&self.current_token());
            self.advance(); // consume the lifetime
            bounds.push(name);

            if matches!(self.current_token().kind, TokenKind::Plus)
              && !matches!(self.peek(1).kind, TokenKind::Lifetime { .. })
              && !self.peek(1).kind.can_start_path()
            {
              let token = self.current_token();
              let diagnostic = Diagnostic::new(
                DiagnosticCode::Error(DiagnosticError::InvalidLifetime),
                "expected a lifetime after `+`".to_string(),
                self.source_file.path.clone(),
              )
              .with_label(
                token.span,
                Some("lifetime bounds must list lifetimes like `'a` or `'b`".to_string()),
                LabelStyle::Primary,
              )
              .with_note("a lifetime must be a valid identifier, like `'a` or `'b`".to_string());
              self.engine.borrow_mut().add(diagnostic);
              return Err(());
            }

            self.expect(TokenKind::Plus)?;
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
        let bounds = self.parse_trait_bounds()?;

        let default = if matches!(self.current_token().kind, TokenKind::Eq) {
          self.advance();
          Some(self.parse_type()?)
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
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
          format!("unexpected token `{}` in generic parameter list", lexeme),
          self.source_file.path.clone(),
        )
        .with_label(
          token.span,
          Some("expected a type, lifetime, or const parameter".to_string()),
          LabelStyle::Primary,
        );

        self.engine.borrow_mut().add(diagnostic);
        Err(())
      },
    }
  }

  /// Parses either lifetime or trait bounds that follow a colon.
  pub fn parse_trait_bounds(&mut self) -> Result<Vec<TypeBound>, ()> {
    let mut bounds = vec![];
    if !matches!(self.current_token().kind, TokenKind::Colon) {
      return Ok(bounds);
    }

    self.expect(TokenKind::Colon)?;

    while !self.is_eof()
      && !matches!(
        self.current_token().kind,
        TokenKind::OpenBrace
          | TokenKind::Comma
          | TokenKind::Gt
          | TokenKind::Eq
          | TokenKind::CloseParen
          | TokenKind::Semi
      )
    {
      match self.current_token().kind {
        TokenKind::Lifetime { .. } => {
          let name = self.get_token_lexeme(&self.current_token());
          self.advance(); // consume the lifetime
          bounds.push(TypeBound::Lifetime { name });
        },
        _ => {
          let modifier = self.parse_trait_bound_modifier()?;

          let for_lifetimes = self.parse_for_lifetimes()?;
          let path = self.parse_path(false)?;

          bounds.push(TypeBound::Trait {
            modifier,
            path,
            for_lifetimes,
          });
        },
      }

      let consumed_plus = match_and_consume!(self, TokenKind::Plus)?;

      if consumed_plus {
        let next = self.current_token();
        if matches!(
          next.kind,
          TokenKind::Gt
            | TokenKind::Comma
            | TokenKind::OpenBrace
            | TokenKind::CloseBrace
            | TokenKind::CloseParen
            | TokenKind::Eq
            | TokenKind::Semi
            | TokenKind::Eof
        ) {
          let prev = self.peek_prev(0);
          // err_trailing_plus_in_bounds(prev.span, "where-clause"));
          return Err(());
        }

        if !matches!(
          next.kind,
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
        ) {
          // let prev = self.peek_prev(0);
          // .add(diag_factory.err_expected_bound_after_plus(prev.span, "where-clause"));
          return Err(());
        }
      }
    }

    Ok(bounds)
  }

  fn parse_trait_bound_modifier(&mut self) -> Result<TraitBoundModifier, ()> {
    match self.current_token().kind {
      TokenKind::Tilde => {
        self.advance(); // consume the "~"

        if matches!(self.current_token().kind, TokenKind::KwConst) {
          // (e.g., `~const Clone`)
          self.advance(); // consume the "const"
          Ok(TraitBoundModifier::Const)
        } else {
          let diagnostic = Diagnostic::new(
            DiagnosticCode::Error(DiagnosticError::InvalidTraitBoundModifier),
            "expected `const` after `~`".to_string(),
            self.source_file.path.clone(),
          )
          .with_label(
            self.current_token().span,
            Some("use `~const Trait` for const trait bounds".to_string()),
            LabelStyle::Primary,
          )
          .with_note(
            "trait bounds may only be prefixed with `~const`, `?`, or `?~const`".to_string(),
          );
          self.engine.borrow_mut().add(diagnostic);
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
            let diagnostic = Diagnostic::new(
              DiagnosticCode::Error(DiagnosticError::InvalidTraitBoundModifier),
              "expected `const` after `?~`".to_string(),
              self.source_file.path.clone(),
            )
            .with_label(
              self.current_token().span,
              Some("use `?~const Trait` for maybe-const trait bounds".to_string()),
              LabelStyle::Primary,
            )
            .with_note(
              "trait bounds may only be prefixed with `~const`, `?`, or `?~const`".to_string(),
            );
            self.engine.borrow_mut().add(diagnostic);
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

  pub fn parse_generic_args(&mut self) -> Result<Option<GenericArgs>, ()> {
    match self.current_token().kind {
      TokenKind::OpenParen => {
        let token = self.current_token();
        let mut inputs = Vec::<Type>::new();
        self.advance();

        while !self.is_eof() && self.current_token().kind != TokenKind::CloseParen {
          inputs.push(self.parse_type()?);
          let token = self.current_token();
          match_and_consume!(self, TokenKind::Comma)?;

          if !matches!(
            self.current_token().kind,
            TokenKind::Comma | TokenKind::CloseParen
          ) {
            let bad = self.current_token();

            let diagnostic = Diagnostic::new(
              DiagnosticCode::Error(DiagnosticError::InvalidGenericArgs),
              "invalid generic argument".to_string(),
              self.source_file.path.clone(),
            )
            .with_label(
              bad.span,
              Some("expected a ',' or ')' after a generic argument".to_string()),
              LabelStyle::Primary,
            )
            .with_help(
              "Generic arguments must be separated by a ',' and closed with ')'.".to_string(),
            );
            self.engine.borrow_mut().add(diagnostic);
            return Err(());
          }

          if matches!(self.current_token().kind, TokenKind::CloseParen)
            && matches!(token.kind, TokenKind::Comma)
          {
            let diagnostic = Diagnostic::new(
              DiagnosticCode::Error(DiagnosticError::InvalidTrailingComma),
              "trailing comma before ')' is not allowed here".to_string(),
              self.source_file.path.clone(),
            )
            .with_label(
              token.span,
              Some("remove this trailing comma".to_string()),
              LabelStyle::Primary,
            );
            self.engine.borrow_mut().add(diagnostic);
            return Err(());
          }
        }

        if inputs.is_empty() {
          let diagnostic = Diagnostic::new(
            DiagnosticCode::Error(DiagnosticError::EmptyGenericArgs),
            "empty generic argument list".to_string(),
            self.source_file.path.clone(),
          )
          .with_label(
            token.span,
            Some("expected a type, lifetime, or const parameter".to_string()),
            LabelStyle::Primary,
          );
          self.engine.borrow_mut().add(diagnostic);
          return Err(());
        }

        self.expect(TokenKind::CloseParen)?;
        self.expect(TokenKind::ThinArrow)?;
        let output = self.parse_type()?;

        Ok(Some(GenericArgs::Parenthesized {
          inputs,
          output: Some(Box::new(output)),
        }))
      },

      TokenKind::Lt | TokenKind::ColonColon => {
        let args = self.parse_angle_bracketed_generic_args_inner()?;
        Ok(Some(GenericArgs::AngleBracketed { args }))
      },

      _ => Ok(None),
    }
  }

  /// Parses a single generic argument (type, lifetime, const, binding, …).
  pub fn parse_generic_arg(&mut self) -> Result<GenericArg, ()> {
    let token = self.current_token();
    let name = self.get_token_lexeme(&token);

    match token.kind {
      TokenKind::Lifetime { .. } => {
        self.advance(); // consume the lifetime
        Ok(GenericArg::Lifetime(name))
      },
      TokenKind::Literal { .. } | TokenKind::OpenBrace => {
        let expr = self.parse_expression(vec![], ExprContext::Default)?;
        Ok(GenericArg::Const(expr))
      },

      TokenKind::Ident if matches!(self.peek(1).kind, TokenKind::Colon) => {
        self.advance(); // consume the identifier

        let args = self.parse_generic_args()?;

        if self.current_token().kind == TokenKind::Colon {
          let bounds = self.parse_trait_bounds()?;

          return Ok(GenericArg::Constraint { name, args, bounds });
        }

        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;
        Ok(GenericArg::Binding { name, args, ty })
      },
      TokenKind::Ident if matches!(self.peek(1).kind, TokenKind::Eq | TokenKind::Lt) => {
        self.advance(); // consume the identifier

        let args = self.parse_generic_args()?;

        if self.current_token().kind == TokenKind::Colon {
          self.advance(); // consume the ':'

          let bounds = self.parse_trait_bounds()?;
          return Ok(GenericArg::Constraint { args, name, bounds });
        }

        self.expect(TokenKind::Eq)?; // consume the '='
        let ty = self.parse_type()?;

        Ok(GenericArg::Binding { name, args, ty })
      },

      _ => Ok(GenericArg::Type(self.parse_type()?)),
    }
  }

  fn parse_angle_bracketed_generic_args_inner(&mut self) -> Result<Vec<GenericArg>, ()> {
    let token = self.current_token();
    let mut args = Vec::<GenericArg>::new();

    self.expect(TokenKind::Lt)?;

    while !self.is_eof() && self.current_token().kind != TokenKind::Gt {
      args.push(self.parse_generic_arg()?);
      let token = self.current_token();

      if !matches!(self.current_token().kind, TokenKind::Comma | TokenKind::Gt) {
        let bad = self.current_token();

        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidGenericArgs),
          "Invalid generic argument".to_string(),
          self.source_file.path.clone(),
        )
        .with_label(
          bad.span,
          Some("Expected a ',' or '>' after a generic argument".to_string()),
          LabelStyle::Primary,
        )
        .with_help(
          "Generic arguments must be separated by a ',' and may not be followed by a '>'."
            .to_string(),
        );
        self.engine.borrow_mut().add(diagnostic);
        return Err(());
      }

      match_and_consume!(self, TokenKind::Comma)?;

      if matches!(self.current_token().kind, TokenKind::Gt)
        && matches!(token.kind, TokenKind::Comma)
      {
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidTrailingComma),
          "trailing comma before '>' is not allowed here".to_string(),
          self.source_file.path.clone(),
        )
        .with_label(
          token.span,
          Some("remove this trailing comma".to_string()),
          LabelStyle::Primary,
        );
        self.engine.borrow_mut().add(diagnostic);
        return Err(());
      }
    }

    if args.is_empty() {
      let diagnostic = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::EmptyGenericArgs),
        "empty generic argument list".to_string(),
        self.source_file.path.clone(),
      )
      .with_label(
        token.span,
        Some("expected a type, lifetime, or const parameter".to_string()),
        LabelStyle::Primary,
      );
      self.engine.borrow_mut().add(diagnostic);
      return Err(());
    }

    self.expect(TokenKind::Gt)?;
    Ok(args)
  }
}
