use diagnostic::DiagnosticEngine;
use lexer::token::TokenKind;

use crate::ast::{ExprKind, UnaryOp};
use crate::match_and_consume;
use crate::parser_utils::ExprContext;
use crate::{ast::Expr, Parser};

impl Parser {
  pub(crate) fn parse_unary(&mut self, context: ExprContext) -> Result<Expr, ()> {
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
        token.span.merge(self.current_token().span);

        Ok(Expr {
          attributes: vec![],
          kind: ExprKind::Unary {
            expr: Box::new(rhs),
            op,
          },
          span: *token.span.merge(self.current_token().span),
        })
      },
      TokenKind::And => {
        self.advance(); // consume the first '&'

        let mut depth = 1;
        while !self.is_eof() && match_and_consume!(self, TokenKind::And)? {
          depth += 1;
        }

        let mutability = self.parse_mutability()?;

        let rhs = self.parse_unary(context)?;
        token.span.merge(self.current_token().span);

        Ok(Expr {
          attributes: vec![],
          kind: ExprKind::Unary {
            expr: Box::new(rhs),
            op: UnaryOp::Ref { mutability, depth },
          },
          span: *token.span.merge(self.current_token().span),
        })

        // Ok(Expr::Unary {
        //   expr: Box::new(rhs),
        //   op: UnaryOp::Ref { mutable, depth },
        //   span: token.span,
        // })
      },
      _ => self.parse_postfix(context),
    }
  }
}
