use crate::{
  ast::{
    expr::{ClosureParam, Expr, ExprKind},
    ty::Type,
  },
  parser_utils::ParserContext,
  Parser,
};
use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_closure(&mut self, context: ParserContext) -> Result<Expr, ()> {
    let mut token = self.current_token();
    let (is_move, is_async) = self.parse_closure_flavors()?;
    let params = self.parse_closure_params(context)?;

    if !matches!(self.current_token().kind, TokenKind::ThinArrow) {
      let body = self.parse_expression(vec![], context)?;
      return Ok(Expr {
        attributes: vec![],
        kind: ExprKind::Closure {
          is_async,
          is_move,
          params,
          return_type: None,
          body: Box::new(body),
        },
        span: *token.span.merge(self.last_token_span()),
      });
    }

    self.expect(TokenKind::ThinArrow)?;
    let return_type = self.parse_type(context)?;
    let temp = self.current_token();
    let body = match self.parse_expression(vec![], context)? {
      body @ Expr {
        kind: ExprKind::Block { .. },
        ..
      } => body,
      _ => {
        let diagnostic = self
          .diagnostic(
            DiagnosticError::ExpectedBlockAfterFlavor,
            "expected block after closure flavors",
          )
          .with_label(
            temp.span,
            Some("expected block after closure flavors".to_string()),
            LabelStyle::Primary,
          )
          .with_help("expected block after closure flavors".to_string());
        self.emit(diagnostic);
        return Err(());
      },
    };

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Closure {
        is_async,
        is_move,
        params,
        return_type: Some(return_type),
        body: Box::new(body),
      },
      span: *token.span.merge(self.last_token_span()),
    })
  }

  fn parse_closure_params(&mut self, context: ParserContext) -> Result<Vec<ClosureParam>, ()> {
    self.expect(TokenKind::Or)?;
    let mut params = vec![];

    if matches!(self.current_token().kind, TokenKind::Comma) {
      let diagnostic = self
        .diagnostic(DiagnosticError::UnexpectedToken, "unexpected token")
        .with_label(
          self.current_token().span,
          Some("unexpected token".to_string()),
          LabelStyle::Primary,
        )
        .with_help("unexpected token".to_string());
      self.emit(diagnostic);
      return Err(());
    }

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::Or) {
      params.push(self.parse_closure_param(context)?);
      if !matches!(self.current_token().kind, TokenKind::Or) {
        self.expect(TokenKind::Comma)?;
      }
    }
    self.expect(TokenKind::Or)?;
    Ok(params)
  }

  fn parse_closure_param(&mut self, context: ParserContext) -> Result<ClosureParam, ()> {
    let mut token = self.current_token();
    let attributes = self.parse_outer_attributes(context)?;
    let pattern = self.parse_pattern(context)?;

    let ty = if matches!(self.current_token().kind, TokenKind::Colon) {
      self.advance(); // consume ':'
      Some(self.parse_type(context)?)
    } else {
      Some(Type::Infer)
    };

    Ok(ClosureParam {
      attributes,
      pattern,
      ty,
      span: *token.span.merge(self.last_token_span()),
    })
  }

  fn parse_closure_flavors(&mut self) -> Result<(bool, bool), ()> {
    let mut is_move = false;
    let mut is_async = false;

    if matches!(self.current_token().kind, TokenKind::KwAsync) {
      is_async = true;
      self.advance();

      if matches!(self.current_token().kind, TokenKind::KwMove) {
        is_move = true;
        self.advance();
      }

      if matches!(
        self.current_token().kind,
        TokenKind::KwAsync | TokenKind::KwMove
      ) {
        let bad = self.current_token();
        let diagnostic = self
          .diagnostic(
            DiagnosticError::InvalidFlavorOrder,
            "invalid closure flavor order",
          )
          .with_label(
            bad.span,
            Some("async closures may only be followed by optional move".to_string()),
            LabelStyle::Primary,
          );
        self.emit(diagnostic);
        return Err(());
      }
    } else if matches!(self.current_token().kind, TokenKind::KwMove) {
      is_move = true;
      self.advance();

      if matches!(self.current_token().kind, TokenKind::KwAsync) {
        let bad = self.current_token();
        let diagnostic = self
          .diagnostic(
            DiagnosticError::InvalidFlavorOrder,
            "invalid closure flavor order",
          )
          .with_label(
            bad.span,
            Some("`async` must come before `move` in async closures".to_string()),
            LabelStyle::Primary,
          );
        self.emit(diagnostic);
        return Err(());
      }

      if matches!(self.current_token().kind, TokenKind::KwMove) {
        let bad = self.current_token();
        let diagnostic = self
          .diagnostic(
            DiagnosticError::InvalidFlavorOrder,
            "invalid closure flavor order",
          )
          .with_label(
            bad.span,
            Some("`move` cannot appear twice in a closure".to_string()),
            LabelStyle::Primary,
          );
        self.emit(diagnostic);
        return Err(());
      }
    }

    Ok((is_move, is_async))
  }

  /// Look ahead to verify a closure head can start at the current token.
  /// Accepts `async` optionally followed by `move`, or `move` alone, followed by `|`.
  pub(crate) fn can_start_closure(&self) -> bool {
    let mut offset = 0;
    let mut saw_async = false;
    let mut saw_move = false;

    loop {
      match self.peek(offset).kind {
        TokenKind::KwAsync => {
          // async can only appear once and must come before move
          if saw_async || saw_move {
            return false;
          }
          saw_async = true;
          offset += 1;
        },
        TokenKind::KwMove => {
          // move can only appear once
          if saw_move {
            return false;
          }
          saw_move = true;
          offset += 1;
        },
        TokenKind::Or => {
          // we reached the closure head delimiter
          return true;
        },
        _ => return false,
      }
    }
  }
}
