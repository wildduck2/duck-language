use lexer::token::TokenKind;

use crate::ast::{ExprKind, UnaryOp};
use crate::match_and_consume;
use crate::parser_utils::ParserContext;
use crate::{ast::Expr, Parser};

impl Parser {
  pub(crate) fn parse_unary(&mut self, context: ParserContext) -> Result<Expr, ()> {
    let mut token = self.current_token();

    match token.kind {
      TokenKind::Minus | TokenKind::Bang | TokenKind::Star => {
        self.advance(); // consume operator
        let op = match token.kind {
          TokenKind::Minus => UnaryOp::Neg,
          TokenKind::Bang => UnaryOp::Not,
          TokenKind::Star => UnaryOp::Deref,
          _ => unreachable!(),
        };
        let rhs = self.parse_unary(context)?;
        Ok(Expr {
          attributes: vec![],
          kind: ExprKind::Unary {
            expr: Box::new(rhs),
            op,
          },
          span: *token.span.merge(self.last_token_span()),
        })
      },
      TokenKind::Amp => {
        self.advance(); // consume the first '&'

        let mut depth = 1;
        while !self.is_eof() && match_and_consume!(self, TokenKind::Amp)? {
          depth += 1;
        }
        let mutability = self.parse_mutability()?;
        let rhs = self.parse_unary(context)?;
        Ok(Expr {
          attributes: vec![],
          kind: ExprKind::Unary {
            expr: Box::new(rhs),
            op: UnaryOp::Ref { mutability, depth },
          },
          span: *token.span.merge(self.last_token_span()),
        })
      },
      _ => self.parse_postfix(context),
    }
  }
}
