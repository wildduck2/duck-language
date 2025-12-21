use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::TokenKind;

use crate::ast::{BinaryOp, Expr, ExprKind};
use crate::parser_utils::ExprContext;
use crate::Parser;

impl Parser {
  pub(crate) fn parse_shift(&mut self, context: ExprContext) -> Result<Expr, ()> {
    let mut lhs = self.parse_term(context)?;

    while !self.is_eof() {
      let mut token = self.current_token();
      let next = self.peek(1);

      // Detect shift operator pairs
      let op = match (token.kind, next.kind) {
        (TokenKind::Lt, TokenKind::Lt) => Some(BinaryOp::Shl),
        (TokenKind::Gt, TokenKind::Gt) => Some(BinaryOp::Shr),
        _ => None,
      };

      // Stop if not a shift operator
      if op.is_none() {
        break;
      }

      // Consume both characters (`<<` or `>>`)
      self.advance();
      self.advance();

      let rhs = self.parse_term(context)?;

      // Reject invalid chaining with range operators after shifting
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

      lhs = Expr {
        attributes: vec![],
        kind: ExprKind::Binary {
          op: op.unwrap(),
          left: Box::new(lhs),
          right: Box::new(rhs),
        },
        span: *token.span.merge(self.current_token().span),
      };
    }

    Ok(lhs)
  }
}
