use lexer::token::TokenKind;

use crate::{
  ast::expr::{Expr, ExprKind},
  parser_utils::ExprContext,
  Parser,
};

impl Parser {
  pub(crate) fn parse_cast(&mut self, context: ExprContext) -> Result<Expr, ()> {
    // First parse the next higher precedence level: unary
    let mut lhs = self.parse_unary(context)?;

    loop {
      match self.current_token().kind {
        TokenKind::KwAs => {
          let mut token = self.current_token();
          self.advance(); // consume `as`
          let ty = self.parse_type(context)?;
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
        _ => break,
      }
    }

    Ok(lhs)
  }
}
