use crate::{ast::*, match_and_consume, parser_utils::ExprContext, Parser};
use diagnostic::DiagnosticEngine;
use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_match_expression(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();
    self.advance(engine); // consume "match"

    let scrutinee = self.parse_expression(vec![], context, engine)?;
    self.expect(TokenKind::OpenBrace, engine)?;
    let mut arms = Vec::new();

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::CloseBrace) {
      arms.push(self.parse_match_arm(context, engine)?);
      match_and_consume!(self, engine, TokenKind::Comma)?;
    }

    self.expect(TokenKind::CloseBrace, engine)?;

    Ok(Expr {
      attributes: Vec::new(),
      kind: ExprKind::Match {
        scrutinee: Box::new(scrutinee),
        arms,
      },
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_match_arm(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<MatchArm, ()> {
    let mut token = self.current_token();

    let attributes = self.parse_outer_attributes(engine)?;
    let pattern = self.parse_pattern_with_or(context, engine)?;

    let guard = if matches!(self.current_token().kind, TokenKind::KwIf) {
      self.advance(engine); // consume "if"
      Some(self.parse_expression(vec![], context, engine)?)
    } else {
      None
    };

    self.expect(TokenKind::FatArrow, engine)?;
    let body = self.parse_expression(vec![], ExprContext::Default, engine)?;

    Ok(MatchArm {
      attributes,
      pattern,
      guard,
      body,
      span: *token.span.merge(self.current_token().span),
    })
  }
}
