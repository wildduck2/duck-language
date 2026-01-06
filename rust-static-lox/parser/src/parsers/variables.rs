use lexer::token::TokenKind;

use crate::{
  ast::{Attribute, Expr, ExprKind, LetStmt, Stmt},
  match_and_consume,
  parser_utils::ParserContext,
  Parser,
};

impl Parser {
  pub(crate) fn parse_let_statement(
    &mut self,
    attributes: Vec<Attribute>,
    context: ParserContext,
  ) -> Result<Stmt, ()> {
    let token = self.current_token();
    self.advance(); // consume `let`

    let pattern = self.parse_pattern_with_or(context)?;

    let ty = if match_and_consume!(self, TokenKind::Colon)? {
      Some(self.parse_type(context)?)
    } else {
      None
    };

    let init = if match_and_consume!(self, TokenKind::Eq)? {
      Some(self.parse_expression(vec![], ParserContext::Default)?)
    } else {
      None
    };

    let context = if matches!(context, ParserContext::LoopCondition) {
      context
    } else {
      ParserContext::LetElse
    };

    let else_block = if match_and_consume!(self, TokenKind::KwElse)? {
      Some(self.parse_block(None, context, vec![])?)
    } else {
      None
    };

    Ok(Stmt::Let(LetStmt {
      attributes,
      pattern,
      ty,
      init: init.map(Box::new),
      else_block: else_block.map(Box::new),
      span: token.span,
    }))
  }

  pub(crate) fn parse_assignment_expr(&mut self, context: ParserContext) -> Result<Expr, ()> {
    let mut lhs = self.parse_range_expr(context)?;
    let token = self.current_token();

    if matches!(
      token.kind,
      TokenKind::Eq
        | TokenKind::PlusEq
        | TokenKind::MinusEq
        | TokenKind::StarEq
        | TokenKind::SlashEq
        | TokenKind::PercentEq
        | TokenKind::AndEq
        | TokenKind::OrEq
        | TokenKind::CaretEq
        | TokenKind::ShiftLeftEq
        | TokenKind::ShiftRightEq
    ) {
      self.advance(); // consume assignment operator

      let rhs = self.parse_range_expr(context)?;

      lhs = Expr {
        attributes: vec![],
        kind: ExprKind::Assign {
          target: Box::new(lhs),
          value: Box::new(rhs),
        },
        span: token.span,
      };
    }

    Ok(lhs)
  }
}
