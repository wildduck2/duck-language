use crate::{ast::*, match_and_consume, parser_utils::ParserContext, Parser};
use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_match_expression(&mut self, context: ParserContext) -> Result<Expr, ()> {
    let mut token = self.current_token();
    self.advance(); // consume "match"

    let scrutinee = self.parse_expression(vec![], ParserContext::Match)?;
    self.expect(TokenKind::LBrace)?;
    let mut arms = Vec::new();

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::RBrace) {
      arms.push(self.parse_match_arm(context)?);
      match_and_consume!(self, TokenKind::Comma)?;
    }

    self.expect(TokenKind::RBrace)?;

    Ok(Expr {
      attributes: Vec::new(),
      kind: ExprKind::Match {
        scrutinee: Box::new(scrutinee),
        arms,
      },
      span: *token.span.merge(self.last_token_span()),
    })
  }

  fn parse_match_arm(&mut self, context: ParserContext) -> Result<MatchArm, ()> {
    let mut token = self.current_token();

    let attributes = self.parse_outer_attributes(context)?;
    let pattern = self.parse_pattern_with_or(context)?;

    let guard = if matches!(self.current_token().kind, TokenKind::KwIf) {
      self.advance(); // consume "if"
      Some(self.parse_expression(vec![], ParserContext::Match)?)
    } else {
      None
    };

    self.expect(TokenKind::FatArrow)?;
    let body = self.parse_expression(vec![], context)?;

    Ok(MatchArm {
      attributes,
      pattern,
      guard,
      body,
      span: *token.span.merge(self.last_token_span()),
    })
  }
}
