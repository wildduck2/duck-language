use crate::{
  ast::{Attribute, Expr, ExprKind},
  parser_utils::ExprContext,
  Parser,
};
use diagnostic::DiagnosticEngine;
use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_loop_expression(
    &mut self,
    label: Option<String>,
    outer_attributes: Vec<Attribute>,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();
    if !outer_attributes.is_empty() {
      token.span.merge(outer_attributes[0].span);
    }

    self.advance(engine); // consume the "loop"
    let body = self.parse_block(None, ExprContext::LoopCondition, outer_attributes, engine)?;

    token.span.merge(self.current_token().span);
    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Loop {
        label,
        body: Box::new(body),
      },
      span: token.span,
    })
  }

  pub(crate) fn parse_while_expression(
    &mut self,
    label: Option<String>,
    outer_attributes: Vec<Attribute>,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();
    if !outer_attributes.is_empty() {
      token.span.merge(outer_attributes[0].span);
    }

    self.advance(engine); // consume the "while"

    let condition = self.parse_expression(vec![], ExprContext::Default, engine)?;

    let body = self.parse_block(None, ExprContext::Default, outer_attributes, engine)?;

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::While {
        condition: Box::new(condition),
        body: Box::new(body),
        label,
      },
      span: *token.span.merge(self.current_token().span),
    })
  }

  pub(crate) fn parse_for_expression(
    &mut self,
    label: Option<String>,
    outer_attributes: Vec<Attribute>,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();
    if !outer_attributes.is_empty() {
      token.span.merge(outer_attributes[0].span);
    }

    self.advance(engine); // consume the "for"

    let pattern = self.parse_pattern(ExprContext::Default, engine)?;
    self.expect(TokenKind::KwIn, engine)?;
    let iterator = self.parse_expression(vec![], ExprContext::Default, engine)?;
    let body = self.parse_block(None, ExprContext::Default, outer_attributes, engine)?;

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::For {
        pattern,
        iterator: Box::new(iterator),
        body: Box::new(body),
        label,
      },
      span: token.span,
    })
  }
}
