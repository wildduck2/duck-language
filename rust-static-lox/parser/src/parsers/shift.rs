use lexer::token::TokenKind;

use crate::ast::{BinaryOp, Expr, ExprKind};
use crate::parser_utils::ExprContext;
use crate::Parser;

impl Parser {
  pub(crate) fn parse_shift(&mut self, context: ExprContext) -> Result<Expr, ()> {
    let mut lhs = self.parse_term(context)?;

    while !self.is_eof() {
      let mut token = self.current_token();
      let next = self.peek(1);

      // Detect shift operator pairs
      let op = match (token.kind, next.kind) {
        (TokenKind::Lt, TokenKind::Lt) => Some(BinaryOp::Shl),
        (TokenKind::Gt, TokenKind::Gt) => Some(BinaryOp::Shr),
        _ => None,
      };

      // Stop if not a shift operator
      if op.is_none() {
        break;
      }

      // Consume both characters (`<<` or `>>`)
      self.advance();
      self.advance();

      let rhs = self.parse_term(context)?;

      lhs = Expr {
        attributes: vec![],
        kind: ExprKind::Binary {
          op: op.unwrap(),
          left: Box::new(lhs),
          right: Box::new(rhs),
        },
        span: *token.span.merge(self.current_token().span),
      };
    }

    Ok(lhs)
  }
}
