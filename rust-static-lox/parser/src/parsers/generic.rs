use crate::{
  ast::{generic::*, Type},
  match_and_consume,
  parser_utils::ExprContext,
  DiagnosticEngine, Parser,
};
use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
};
use lexer::token::{Token, TokenKind};

impl Parser {
  pub(crate) fn parse_generic_params(
    &mut self,
    token: &mut Token,
    engine: &mut DiagnosticEngine,
  ) -> Result<Option<GenericParams>, ()> {
    if !matches!(self.current_token().kind, TokenKind::Lt) {
      return Ok(None);
    }

    let mut params = Vec::<GenericParam>::new();

    self.expect(TokenKind::Lt, engine)?; // consume the "<"

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::Gt) {
      params.push(self.parse_generic_param(engine)?);
      match_and_consume!(self, engine, TokenKind::Comma)?;
    }

    self.expect(TokenKind::Gt, engine)?; // consume the ">"

    token.span.merge(self.current_token().span);
    Ok(Some(GenericParams {
      params,
      span: token.span,
    }))
  }

  /// Parses a single generic parameter (type, lifetime, or const).
  pub(crate) fn parse_generic_param(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<GenericParam, ()> {
    let attributes = self.parse_outer_attributes(engine)?;
    let token = self.current_token();

    match token.kind {
      // const generic: const N: usize = 3
      TokenKind::KwConst => {
        self.advance(engine); // consume "const"
        let name = self.parse_name(false, engine)?;

        self.expect(TokenKind::Colon, engine)?; // must have ":"
        let ty = self.parse_type(engine)?;

        let default = if matches!(self.current_token().kind, TokenKind::Eq) {
          self.advance(engine);
          Some(self.parse_expression(vec![], ExprContext::Default, engine)?)
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
        self.advance(engine); // consume the lifetime

        if matches!(self.current_token().kind, TokenKind::Colon) {
          self.advance(engine); // consume the colon
          while !self.is_eof()
            && !matches!(
              self.current_token().kind,
              TokenKind::OpenBrace | TokenKind::Comma | TokenKind::Gt
            )
          {
            let name = self.get_token_lexeme(&self.current_token());
            self.advance(engine); // consume the lifetime
            bounds.push(name);
            match_and_consume!(self, engine, TokenKind::Plus)?;
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
        let name = self.parse_name(false, engine)?;
        let bounds = self.parse_trait_bounds(engine)?;

        let default = if matches!(self.current_token().kind, TokenKind::Eq) {
          self.advance(engine);
          Some(self.parse_type(engine)?)
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

        engine.add(diagnostic);
        Err(())
      },
    }
  }

  /// Parses either lifetime or trait bounds that follow a colon.
  pub(crate) fn parse_trait_bounds(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Vec<TypeBound>, ()> {
    let mut bounds = vec![];
    if !matches!(self.current_token().kind, TokenKind::Colon) {
      return Ok(bounds);
    }

    self.expect(TokenKind::Colon, engine)?;

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
          self.advance(engine); // consume the lifetime
          bounds.push(TypeBound::Lifetime { name });
        },
        _ => {
          let modifier = self.parse_trait_bound_modifier(engine)?;

          let for_lifetimes = self.parse_for_lifetimes(engine)?;
          let path = self.parse_path(false, engine)?;

          bounds.push(TypeBound::Trait {
            modifier,
            path,
            for_lifetimes,
          });
        },
      }

      match_and_consume!(self, engine, TokenKind::Plus)?;
    }

    Ok(bounds)
  }

  fn parse_trait_bound_modifier(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<TraitBoundModifier, ()> {
    match self.current_token().kind {
      TokenKind::Tilde => {
        self.advance(engine); // consume the "~"

        if matches!(self.current_token().kind, TokenKind::KwConst) {
          // (e.g., `~const Clone`)
          self.advance(engine); // consume the "const"
          Ok(TraitBoundModifier::Const)
        } else {
          let diagnostic = Diagnostic::new(
            DiagnosticCode::Error(DiagnosticError::InvalidTraitBoundModifier),
            "invalid trait bound modifier".to_string(),
            self.source_file.path.clone(),
          )
          .with_label(
            self.current_token().span,
            Some("expected `~` or `?`".to_string()),
            LabelStyle::Primary,
          )
          .with_note("trait bounds may only be prefixed with `~` or `?`".to_string());
          engine.add(diagnostic);
          Err(())
        }
      },
      TokenKind::Question => {
        self.advance(engine); // consume the "?"

        if matches!(self.current_token().kind, TokenKind::Tilde) {
          self.advance(engine); // consume the "~"

          if matches!(self.current_token().kind, TokenKind::KwConst) {
            // (e.g., `~const Clone`)
            self.advance(engine); // consume the "const"
            Ok(TraitBoundModifier::MaybeConst)
          } else {
            let diagnostic = Diagnostic::new(
              DiagnosticCode::Error(DiagnosticError::InvalidTraitBoundModifier),
              "invalid trait bound modifier".to_string(),
              self.source_file.path.clone(),
            )
            .with_label(
              self.current_token().span,
              Some("expected `~` or `?`".to_string()),
              LabelStyle::Primary,
            )
            .with_note("trait bounds may only be prefixed with `~` or `?`".to_string());
            engine.add(diagnostic);
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
    engine: &mut DiagnosticEngine,
  ) -> Result<Option<GenericArgs>, ()> {
    match self.current_token().kind {
      TokenKind::OpenParen => {
        let mut inputs = Vec::<Type>::new();
        self.advance(engine); // consume the "("

        while !self.is_eof() && self.current_token().kind != TokenKind::CloseParen {
          inputs.push(self.parse_type(engine)?);
          match_and_consume!(self, engine, TokenKind::Comma)?;
        }

        self.expect(TokenKind::CloseParen, engine)?; // consume the ")"
        self.expect(TokenKind::ThinArrow, engine)?; // consume the "->"

        let output = self.parse_type(engine)?;

        Ok(Some(GenericArgs::Parenthesized {
          inputs,
          output: Some(Box::new(output)),
        }))
      },
      TokenKind::Lt | TokenKind::ColonColon => {
        let mut args = Vec::<GenericArg>::new();
        self.expect(TokenKind::Lt, engine)?; // consume the "<"

        while !self.is_eof() && self.current_token().kind != TokenKind::Gt {
          args.push(self.parse_generic_arg(engine)?);
          match_and_consume!(self, engine, TokenKind::Comma)?;
        }

        self.expect(TokenKind::Gt, engine)?; // consume the ">"

        Ok(Some(GenericArgs::AngleBracketed { args }))
      },
      _ => Ok(None),
    }
  }

  /// Parses a single generic argument (type, lifetime, const, binding, …).
  pub(crate) fn parse_generic_arg(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<GenericArg, ()> {
    let token = self.current_token();
    let name = self.get_token_lexeme(&token);

    match token.kind {
      TokenKind::Lifetime { .. } => {
        self.advance(engine); // consume the lifetime
        Ok(GenericArg::Lifetime(name))
      },
      TokenKind::Literal { .. } | TokenKind::OpenBrace => {
        let expr = self.parse_expression(vec![], ExprContext::Default, engine)?;
        Ok(GenericArg::Const(expr))
      },

      TokenKind::Ident if matches!(self.peek(1).kind, TokenKind::Colon) => {
        self.advance(engine); // consume the identifier

        let args = self.parse_generic_args(engine)?;

        if self.current_token().kind == TokenKind::Colon {
          let bounds = self.parse_trait_bounds(engine)?;

          return Ok(GenericArg::Constraint { name, args, bounds });
        }

        self.expect(TokenKind::Colon, engine)?;
        let ty = self.parse_type(engine)?;
        Ok(GenericArg::Binding { name, args, ty })
      },
      TokenKind::Ident if matches!(self.peek(1).kind, TokenKind::Eq | TokenKind::Lt) => {
        self.advance(engine); // consume the identifier

        let args = self.parse_generic_args(engine)?;

        if self.current_token().kind == TokenKind::Colon {
          self.advance(engine); // consume the ':'

          let bounds = self.parse_trait_bounds(engine)?;
          return Ok(GenericArg::Constraint { args, name, bounds });
        }

        self.expect(TokenKind::Eq, engine)?; // consume the '='
        let ty = self.parse_type(engine)?;

        Ok(GenericArg::Binding { name, args, ty })
      },

      _ => Ok(GenericArg::Type(self.parse_type(engine)?)),
    }
  }
}
