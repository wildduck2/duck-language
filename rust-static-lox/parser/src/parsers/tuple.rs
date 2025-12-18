use diagnostic::DiagnosticEngine;
use lexer::token::TokenKind;

use crate::{
  ast::{Expr, ExprKind},
  parser_utils::ExprContext,
  Parser,
};

impl Parser {
  pub(crate) fn parse_grouped_and_tuple_expr(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();
    // consume "("
    self.advance(engine);

    // Handle the unit expression "()"
    if matches!(self.current_token().kind, TokenKind::CloseParen) {
      self.advance(engine); // consume ")"

      return Ok(Expr {
        attributes: vec![], // TODO: implement attributes
        kind: ExprKind::Tuple { elements: vec![] },
        span: token.span,
      });
    }

    let mut elements = vec![];
    let mut trailing_comma = false;
    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::CloseParen) {
      // Skip stray leading commas
      if matches!(self.current_token().kind, TokenKind::Comma) {
        trailing_comma = true;
        self.advance(engine);
        continue;
      }

      elements.push(self.parse_expression(vec![], ExprContext::Default, engine)?);

      if matches!(self.current_token().kind, TokenKind::Comma) {
        trailing_comma = true;
        self.advance(engine); // consume ","
      } else {
        trailing_comma = false;
        break;
      }
    }

    // consume ")"
    self.expect(TokenKind::CloseParen, engine)?;
    token.span.merge(self.current_token().span);

    // Distinguish grouped vs tuple vs unit
    match elements.len() {
      1 => {
        if trailing_comma {
          // (x,) is a tuple

          Ok(Expr {
            attributes: vec![],
            kind: ExprKind::Tuple { elements },
            span: token.span,
          })
        } else {
          // (x) is a grouping expression

          Ok(Expr {
            attributes: vec![],
            kind: ExprKind::Group {
              expr: Box::new(elements[0].clone()),
            },
            span: token.span,
          })
        }
      },

      _ => {
        // Multiple elements always form a tuple

        Ok(Expr {
          attributes: vec![],
          kind: ExprKind::Tuple { elements },
          span: token.span,
        })
      },
    }
  }
}
