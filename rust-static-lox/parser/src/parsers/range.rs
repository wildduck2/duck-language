use crate::{
  ast::{Expr, ExprKind, RangeExprKind},
  parser_utils::ExprContext,
  Parser,
};
use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_range_expr(&mut self, context: ExprContext) -> Result<Expr, ()> {
    // Case 1: a range begins directly with ".." or "..="
    if matches!(
      self.current_token().kind,
      TokenKind::DotDot | TokenKind::DotDotEq
    ) {
      return self.parse_range(context, None);
    }

    // Case 2: range may follow a left-hand side expression
    let lhs = self.parse_logical_or(context)?;

    // At most one range operator is allowed
    if matches!(
      self.current_token().kind,
      TokenKind::DotDot | TokenKind::DotDotEq
    ) {
      self.parse_range(context, Some(lhs))
    } else {
      Ok(lhs)
    }
  }

  fn parse_range(&mut self, context: ExprContext, start: Option<Expr>) -> Result<Expr, ()> {
    let mut token = self.current_token();
    self.advance(); // consume ".." or "..="

    // Determine whether an expression follows the operator
    let has_end = !matches!(
      self.current_token().kind,
      TokenKind::CloseBracket
        | TokenKind::CloseParen
        | TokenKind::CloseBrace
        | TokenKind::Semi
        | TokenKind::Eof
    );

    let end = if has_end {
      Some(Box::new(self.parse_logical_or(context)?))
    } else {
      None
    };

    // Classify the kind of range being constructed
    let kind = match (token.kind, start.is_some(), end.is_some()) {
      (TokenKind::DotDot, false, false) => RangeExprKind::Full,
      (TokenKind::DotDot, true, false) => RangeExprKind::From,
      (TokenKind::DotDot, _, true) => RangeExprKind::To,
      (TokenKind::DotDotEq, _, true) => RangeExprKind::ToInclusive,
      (TokenKind::DotDotEq, true, false) => RangeExprKind::FromInclusive,
      _ => RangeExprKind::Exclusive,
    };

    // Reject chained range operators (e.g., `a..b..c`)
    if matches!(
      self.current_token().kind,
      TokenKind::DotDot | TokenKind::DotDotEq
    ) {
      let bad = self.current_token();
      let lexeme = self.get_token_lexeme(&bad);

      let diagnostic = self
        .diagnostic(
          DiagnosticError::UnexpectedToken,
          "chained range expressions are not allowed",
        )
      .with_label(
        bad.span,
        Some(format!("found `{lexeme}` after a range expression")),
        LabelStyle::Primary,
      )
      .with_help("only one `..` or `..=` may appear in a range expression".to_string())
      .with_note("`a..b..c` is invalid; use `(a..b)` or `(b..c)` instead".to_string());

      self.emit(diagnostic);
      return Err(());
    }

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Range {
        start: start.map(Box::new),
        end,
        kind,
      },
      span: *token.span.merge(self.current_token().span),
    })
  }
}
