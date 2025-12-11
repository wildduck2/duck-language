use crate::{ast::*, parser_utils::ExprContext, Parser};
use diagnostic::DiagnosticEngine;
use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_match_expression(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();
    self.advance(engine); // consume the "match"
    let scrutinee = self.parse_expression(vec![], context, engine)?;

    let mut arms = vec![];
    self.expect(TokenKind::OpenBrace, engine)?;

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::CloseBrace) {
      arms.push(self.parse_match_arm(context, engine)?);

      // Here if the arm is the last one, we don't need to consume the comma
      if !matches!(self.current_token().kind, TokenKind::CloseBrace)
        || matches!(self.current_token().kind, TokenKind::Comma)
      {
        self.advance(engine); // consume the comma
      }
    }
    self.expect(TokenKind::CloseBrace, engine)?;

    token.span.merge(self.current_token().span);
    Ok(Expr::Match {
      scrutinee: Box::new(scrutinee),
      arms,
      span: token.span,
    })
  }

  fn parse_match_arm(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<MatchArm, ()> {
    let mut token = self.current_token();
    let attributes = if matches!(self.current_token().kind, TokenKind::Pound) {
      self.parse_attributes(engine)?
    } else {
      vec![]
    };

    let pattern = self.parse_pattern_with_or(context, engine)?;

    let guard = if matches!(self.current_token().kind, TokenKind::KwIf) {
      Some(self.parse_match_guard(context, engine)?)
    } else {
      None
    };

    self.expect(TokenKind::FatArrow, engine)?;
    let body = self.parse_expression(vec![], ExprContext::Default, engine)?;

    token.span.merge(self.current_token().span);

    Ok(MatchArm {
      attributes,
      pattern,
      guard,
      body,
      span: token.span,
    })
  }

  fn parse_match_guard(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();
    self.advance(engine); // consume the "if"
    let condition = self.parse_expression(vec![], context, engine)?;

    token.span.merge(self.current_token().span);
    Ok(Expr::If {
      condition: Box::new(condition),
      then_branch: Box::new(Expr::Block {
        inner_attributes: vec![],
        outer_attributes: vec![],
        stmts: vec![],
        label: None,
        flavor: BlockFlavor::Normal,
        span: token.span,
      }),
      else_branch: None,
      span: token.span,
    })
  }
}
