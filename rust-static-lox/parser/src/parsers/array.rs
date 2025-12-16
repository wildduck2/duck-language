use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
};
use lexer::token::TokenKind;

use crate::{
  ast::expr::{Expr, ExprKind},
  parser_utils::ExprContext,
  DiagnosticEngine, Parser,
};

impl Parser {
  pub(crate) fn parse_array_expr(&mut self, engine: &mut DiagnosticEngine) -> Result<Expr, ()> {
    let mut token = self.current_token();
    self.advance(engine); // consume '['

    let (elements, repeat) = self.parse_array_elements(engine)?;
    token.span.merge(self.current_token().span);

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Array {
        elements,
        repeat: repeat.map(Box::new),
      },
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_array_elements(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<(Vec<Expr>, Option<Expr>), ()> {
    let mut elements = vec![];

    // Case 1: Empty array: `[]`
    if self.current_token().kind == TokenKind::CloseBracket {
      self.advance(engine);
      return Ok((elements, None));
    }

    // First element
    elements.push(self.parse_expression(vec![], ExprContext::Default, engine)?);

    // Case 2: Repeat array form `[value; count]`
    if matches!(self.current_token().kind, TokenKind::Semi) {
      self.advance(engine); // consume `;`

      // Reject `[value;]` - missing count expression
      if !self.current_token().kind.is_literal() {
        let bad = self.current_token();
        let lexeme = self.get_token_lexeme(&bad);

        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
          "invalid repeat expression".into(),
          self.source_file.path.clone(),
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

        engine.add(diagnostic);
        return Err(());
      }

      let repeat_count = self.parse_expression(vec![], ExprContext::Default, engine)?;
      self.expect(TokenKind::CloseBracket, engine)?;
      return Ok((elements, Some(repeat_count)));
    }

    // Case 3: Standard comma-separated array                                 */
    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::CloseBracket) {
      if matches!(self.current_token().kind, TokenKind::Comma) {
        self.advance(engine);
      }

      if self.current_token().kind == TokenKind::CloseBracket {
        break;
      }

      elements.push(self.parse_expression(vec![], ExprContext::Default, engine)?);
    }

    self.expect(TokenKind::CloseBracket, engine)?;
    Ok((elements, None))
  }
}
