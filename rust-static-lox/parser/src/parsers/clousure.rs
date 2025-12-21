use crate::{
  ast::{
    expr::{ClosureParam, Expr, ExprKind},
    ty::Type,
  },
  parser_utils::ExprContext,
  Parser,
};
use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_closure(&mut self, context: ExprContext) -> Result<Expr, ()> {
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
        span: *token.span.merge(self.current_token().span),
      });
    }

    self.expect(TokenKind::ThinArrow)?;
    let return_type = self.parse_type()?;
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
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_closure_params(&mut self, context: ExprContext) -> Result<Vec<ClosureParam>, ()> {
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

  fn parse_closure_param(&mut self, context: ExprContext) -> Result<ClosureParam, ()> {
    let mut token = self.current_token();
    let attributes = self.parse_outer_attributes()?;
    let pattern = self.parse_pattern(context)?;

    let ty = if matches!(self.current_token().kind, TokenKind::Colon) {
      self.advance(); // consume ':'
      Some(self.parse_type()?)
    } else {
      Some(Type::Infer)
    };

    Ok(ClosureParam {
      attributes,
      pattern,
      ty,
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_closure_flavors(&mut self) -> Result<(bool, bool), ()> {
    let mut is_move = false;
    let mut is_async = false;

    // We only allow `move` and `async`, in any order, but at most once each.
    loop {
      match self.current_token().kind {
        TokenKind::KwAsync if !is_async => {
          is_async = true;
          self.advance();
        },
        TokenKind::KwMove if !is_move => {
          is_move = true;
          self.advance();
        },
        _ => break,
      }
    }

    Ok((is_move, is_async))
  }

  /// Look ahead to verify a closure head can start at the current token.
  /// Accepts `move`, `async`, or both (any order, single occurrence each)
  /// followed by `|`. Stops at first unexpected token.
  pub(crate) fn can_start_closure(&self) -> bool {
    let mut offset = 0;
    let mut saw_async = false;
    let mut saw_move = false;

    loop {
      match self.peek(offset).kind {
        TokenKind::KwAsync => {
          // async can only appear once
          if saw_async {
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
