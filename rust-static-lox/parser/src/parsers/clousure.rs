use crate::{
  ast::{CaptureKind, ClosureParam, Expr, Type},
  match_and_consume,
  parser_utils::ExprContext,
  Parser,
};
use diagnostic::DiagnosticEngine;
use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_closure(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();
    let (is_move, is_async) = self.parse_closure_flavors(engine)?;
    let capture = if is_move {
      CaptureKind::Move
    } else {
      CaptureKind::Default
    };

    self.expect(TokenKind::Or, engine)?;
    let params = self.parse_closure_params(context, engine)?;
    self.expect(TokenKind::Or, engine)?;

    if !matches!(self.current_token().kind, TokenKind::ThinArrow) {
      let body = self.parse_expression(vec![], context, engine)?;
      token.span.merge(self.current_token().span);
      return Ok(Expr::Closure {
        capture,
        is_async,
        is_move,
        params,
        return_type: None,
        body: Box::new(body),
        span: token.span,
      });
    }

    self.expect(TokenKind::ThinArrow, engine)?;
    let return_type = self.parse_type(engine)?;
    let body = self.parse_expression(vec![], context, engine)?;

    token.span.merge(self.current_token().span);
    Ok(Expr::Closure {
      capture: CaptureKind::Default,
      is_async,
      is_move,
      params,
      return_type: Some(return_type),
      body: Box::new(body),
      span: token.span,
    })
  }

  fn parse_closure_params(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Vec<ClosureParam>, ()> {
    let mut params = vec![];
    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::Or) {
      params.push(self.parse_closure_param(context, engine)?);
      match_and_consume!(self, engine, TokenKind::Comma)?;
    }
    Ok(params)
  }

  fn parse_closure_param(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<ClosureParam, ()> {
    let attributes = self.parse_outer_attributes(engine)?;
    let pattern = self.parse_pattern(context, engine)?;

    let ty = if matches!(self.current_token().kind, TokenKind::Colon) {
      self.advance(engine); // consume ':'
      Some(self.parse_type(engine)?)
    } else {
      Some(Type::Infer)
    };

    Ok(ClosureParam {
      attributes,
      pattern,
      ty,
    })
  }

  fn parse_closure_flavors(&mut self, engine: &mut DiagnosticEngine) -> Result<(bool, bool), ()> {
    let mut is_move = false;
    let mut is_async = false;

    // We only allow `move` and `async`, in any order, but at most once each.
    loop {
      match self.current_token().kind {
        TokenKind::KwAsync if !is_async => {
          is_async = true;
          self.advance(engine);
        },
        TokenKind::KwMove if !is_move => {
          is_move = true;
          self.advance(engine);
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
