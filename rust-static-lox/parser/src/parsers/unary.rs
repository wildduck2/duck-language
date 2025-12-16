use diagnostic::DiagnosticEngine;
use lexer::token::TokenKind;

use crate::ast::{ExprKind, UnaryOp};
use crate::match_and_consume;
use crate::parser_utils::ExprContext;
use crate::{ast::Expr, Parser};

impl Parser {
  pub(crate) fn parse_unary(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();

    match token.kind {
      TokenKind::Minus | TokenKind::Bang | TokenKind::Star => {
        self.advance(engine); // consume operator

        let op = match token.kind {
          TokenKind::Minus => UnaryOp::Neg,
          TokenKind::Bang => UnaryOp::Not,
          TokenKind::Star => UnaryOp::Deref,
          _ => unreachable!(),
        };

        let rhs = self.parse_unary(context, engine)?;
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
        self.advance(engine); // consume the first '&'

        let mut depth = 1;
        while !self.is_eof() && match_and_consume!(self, engine, TokenKind::And)? {
          depth += 1;
        }

        let mutability = self.parse_mutability(engine)?;

        let rhs = self.parse_unary(context, engine)?;
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
      _ => self.parse_postfix(context, engine),
    }
  }
}
