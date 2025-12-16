use diagnostic::DiagnosticEngine;
use lexer::token::TokenKind;

use crate::{
  ast::expr::{Expr, ExprKind},
  parser_utils::ExprContext,
  Parser,
};

impl Parser {
  pub(crate) fn parse_cast(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    // First parse the next higher precedence level: unary
    let mut lhs = self.parse_unary(context, engine)?;

    loop {
      match self.current_token().kind {
        TokenKind::KwAs => {
          let mut token = self.current_token();
          self.advance(engine); // consume `as`
          let ty = self.parse_type(engine)?;
          token.span.merge(self.current_token().span);
          lhs = Expr {
            attributes: vec![], // TODO: implement attributes
            kind: ExprKind::Cast {
              expr: Box::new(lhs),
              ty,
            },
            span: *token.span.merge(self.current_token().span),
          };
        },
        TokenKind::OpenParen | TokenKind::Dot | TokenKind::OpenBracket | TokenKind::Question => {
          lhs = self.parse_postfix_chain(lhs, context, engine)?;
        },
        _ => break,
      }
    }

    Ok(lhs)
  }
}
