use lexer::token::TokenKind;

use crate::{
  ast::expr::{Expr, ExprKind},
  parser_utils::ParserContext,
  Parser,
};

impl Parser {
  pub(crate) fn parse_array_expr(&mut self) -> Result<Expr, ()> {
    let mut token = self.current_token();
    self.advance(); // consume '['

    let (elements, repeat) = self.parse_array_elements()?;
    token.span.merge(self.last_token_span());

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Array {
        elements,
        repeat: repeat.map(Box::new),
      },
      span: *token.span.merge(self.last_token_span()),
    })
  }

  fn parse_array_elements(&mut self) -> Result<(Vec<Expr>, Option<Expr>), ()> {
    let mut elements = vec![];

    // Case 1: Empty array: `[]`
    if self.current_token().kind == TokenKind::RBracket {
      self.advance();
      return Ok((elements, None));
    }

    // First element
    elements.push(self.parse_expression(vec![], ParserContext::Default)?);

    // Case 2: Repeat array form `[value; count]`
    if matches!(self.current_token().kind, TokenKind::Semi) {
      self.advance(); // consume `;`
      let repeat_count = self.parse_expression(vec![], ParserContext::Default)?;
      self.expect(TokenKind::RBracket)?;
      return Ok((elements, Some(repeat_count)));
    }

    // Case 3: Standard comma-separated array
    loop {
      match self.current_token().kind {
        TokenKind::Comma => {
          self.advance();
          if matches!(self.current_token().kind, TokenKind::RBracket) {
            self.advance();
            return Ok((elements, None));
          }
          elements.push(self.parse_expression(vec![], ParserContext::Default)?);
        },
        TokenKind::RBracket => {
          self.advance();
          return Ok((elements, None));
        },
        _ => {
          let bad = self.current_token();
          let lexeme = self.get_token_lexeme(&bad);
          self.emit(self.err_unexpected_token(bad.span, "`,` or `]`", &lexeme));
          return Err(());
        },
      }
    }
  }
}
