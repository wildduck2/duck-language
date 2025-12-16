use diagnostic::DiagnosticEngine;
use lexer::token::TokenKind;

use crate::{
  ast::{Attribute, Expr, ExprKind, LetStmt, Stmt},
  match_and_consume,
  parser_utils::ExprContext,
  Parser,
};

impl Parser {
  pub(crate) fn parse_let_statement(
    &mut self,
    context: ExprContext,
    attributes: Vec<Attribute>,
    engine: &mut DiagnosticEngine,
  ) -> Result<Stmt, ()> {
    let token = self.current_token();
    self.advance(engine); // consume `let`

    let pattern = self.parse_pattern_with_or(context, engine)?;

    let ty = if match_and_consume!(self, engine, TokenKind::Colon)? {
      Some(self.parse_type(engine)?)
    } else {
      None
    };

    let init = if match_and_consume!(self, engine, TokenKind::Eq)? {
      Some(self.parse_expression(vec![], ExprContext::Default, engine)?)
    } else {
      None
    };

    let context = if matches!(context, ExprContext::LoopCondition) {
      context
    } else {
      ExprContext::LetElse
    };

    let else_block = if match_and_consume!(self, engine, TokenKind::KwElse)? {
      Some(self.parse_block(None, context, vec![], engine)?)
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

  pub(crate) fn parse_assignment_expr(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut lhs = self.parse_range_expr(context, engine)?;

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
      self.advance(engine); // consume assignment operator

      let rhs = self.parse_range_expr(context, engine)?;

      // TODO: split Assign vs AssignOp when you lower compound operators
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
