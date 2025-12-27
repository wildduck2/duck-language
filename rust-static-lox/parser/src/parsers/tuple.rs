use lexer::token::TokenKind;

use crate::{
  ast::{Expr, ExprKind},
  parser_utils::ParserContext,
  Parser,
};

impl Parser {
  pub(crate) fn parse_grouped_and_tuple_expr(&mut self) -> Result<Expr, ()> {
    let mut token = self.current_token();
    self.advance(); // consume "("

    // Handle the unit expression "()"
    if matches!(self.current_token().kind, TokenKind::RParen) {
      self.advance(); // consume ")"

      return Ok(Expr {
        attributes: vec![], // TODO: implement attributes
        kind: ExprKind::Tuple { elements: vec![] },
        span: *token.span.merge(self.last_token_span()),
      });
    }

    let mut elements = vec![];
    let mut trailing_comma = false;
    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::RParen) {
      elements.push(self.parse_expression(vec![], ParserContext::Default)?);

      match self.check_comma_with_trailing(true)? {
        v @ true => {
          trailing_comma = v;
          continue;
        },
        false => break,
      }
    }

    // consume ")"
    self.expect(TokenKind::RParen)?;
    token.span.merge(self.last_token_span());

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
