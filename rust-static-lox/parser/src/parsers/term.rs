use lexer::token::TokenKind;

use crate::ast::{BinaryOp, ExprKind};
use crate::parser_utils::ParserContext;
use crate::{ast::Expr, Parser};

impl Parser {
  pub(crate) fn parse_term(&mut self, context: ParserContext) -> Result<Expr, ()> {
    let mut lhs = self.parse_factor(context)?;

    'term_find: while !self.is_eof() {
      let mut token = self.current_token();
      match token.kind {
        TokenKind::Plus | TokenKind::Minus => {
          self.advance(); // consume the operator

          let op = match token.kind {
            TokenKind::Plus => BinaryOp::Add,
            TokenKind::Minus => BinaryOp::Sub,
            _ => unreachable!(),
          };

          let rhs = self.parse_factor(context)?;

          lhs = Expr {
            attributes: vec![],
            kind: ExprKind::Binary {
              op,
              left: Box::new(lhs),
              right: Box::new(rhs),
            },
            span: *token.span.merge(self.last_token_span()),
          };
        },
        _ => break 'term_find,
      }
    }

    Ok(lhs)
  }
}
