use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
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

      // Reject `[value;]` - missing count expression
      if !self.current_token().kind.is_literal() {
        let bad = self.current_token();
        let lexeme = self.get_token_lexeme(&bad);

        let diagnostic = self
          .diagnostic(
            DiagnosticError::UnexpectedToken,
            "invalid repeat expression",
          )
          .with_label(
            bad.span,
            Some(format!(
              "expected an integer literal here, found `{lexeme}`"
            )),
            LabelStyle::Primary,
          )
          .with_note(
            "repeat array syntax is: `[value; N]` where `N` is a compile-time constant integer."
              .into(),
          )
          .with_help("example: `[x; 4]`".into());

        self.emit(diagnostic);
        return Err(());
      }

      let repeat_count = self.parse_expression(vec![], ParserContext::Default)?;
      self.expect(TokenKind::RBracket)?;
      return Ok((elements, Some(repeat_count)));
    }

    // Case 3: Standard comma-separated array                                 */
    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::RBracket) {
      if matches!(self.current_token().kind, TokenKind::Comma) {
        self.advance();
      }

      if self.current_token().kind == TokenKind::RBracket {
        break;
      }

      elements.push(self.parse_expression(vec![], ParserContext::Default)?);
    }

    self.expect(TokenKind::RBracket)?;
    Ok((elements, None))
  }
}
