use crate::{
  ast::{Attribute, Expr, ExprKind},
  parser_utils::ParserContext,
  Parser,
};
use lexer::token::{Token, TokenKind};

impl Parser {
  pub(crate) fn parse_loop_expression(
    &mut self,
    label: Option<String>,
    outer_attributes: Vec<Attribute>,
    context: ParserContext,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();
    if !outer_attributes.is_empty() {
      token.span.merge(outer_attributes[0].span);
    }

    self.advance(); // consume the "loop"
    let body = self.parse_block(None, context, outer_attributes)?;

    token.span.merge(self.last_token_span());
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
    context: ParserContext,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();
    if !outer_attributes.is_empty() {
      token.span.merge(outer_attributes[0].span);
    }

    self.advance(); // consume the "while"

    if matches!(self.current_token().kind, TokenKind::KwLet) {
      return self.parse_while_let_expression(label, outer_attributes, &mut token, context);
    }

    let condition = self.parse_expression(vec![], context)?;
    let body = self.parse_block(None, ParserContext::LoopCondition, outer_attributes)?;

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::While {
        condition: Box::new(condition),
        body: Box::new(body),
        label,
      },
      span: *token.span.merge(self.last_token_span()),
    })
  }

  fn parse_while_let_expression(
    &mut self,
    label: Option<String>,
    outer_attributes: Vec<Attribute>,
    token: &mut Token,
    context: ParserContext,
  ) -> Result<Expr, ()> {
    self.advance(); // consume "let"
    let pattern = self.parse_pattern(context)?;
    self.expect(TokenKind::Eq)?;
    let scrutinee = self.parse_expression(vec![], context)?;
    let body = self.parse_block(None, ParserContext::LoopCondition, outer_attributes)?;

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::WhileLet {
        label,
        pattern,
        scrutinee: Box::new(scrutinee),
        body: Box::new(body),
      },
      span: *token.span.merge(self.last_token_span()),
    })
  }

  pub(crate) fn parse_for_expression(
    &mut self,
    label: Option<String>,
    outer_attributes: Vec<Attribute>,
    context: ParserContext,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();
    if !outer_attributes.is_empty() {
      token.span.merge(outer_attributes[0].span);
    }

    self.advance(); // consume the "for"

    let pattern = self.parse_pattern(ParserContext::Default)?;
    self.expect(TokenKind::KwIn)?;
    let iterator = self.parse_expression(vec![], context)?;
    let body = self.parse_block(None, ParserContext::LoopCondition, outer_attributes)?;

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::For {
        pattern,
        iterator: Box::new(iterator),
        body: Box::new(body),
        label,
      },
      span: *token.span.merge(self.last_token_span()),
    })
  }
}
