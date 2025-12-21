use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::TokenKind;

use crate::ast::expr::{BinaryOp, Expr, ExprKind};
use crate::parser_utils::ExprContext;
use crate::Parser;

impl Parser {
  pub(crate) fn parse_comparison(&mut self, context: ExprContext) -> Result<Expr, ()> {
    // first parse the next higher precedence level
    let mut lhs = self.parse_bitwise_or(context)?;
    loop {
      if matches!(self.peek(1).kind, TokenKind::Colon) {
        break;
      }

      let mut token = self.current_token();
      let op = match token.kind {
        TokenKind::EqEq => BinaryOp::Eq,
        TokenKind::Ne => BinaryOp::NotEq,
        TokenKind::Lt => BinaryOp::Less,
        TokenKind::Le => BinaryOp::LessEq,
        TokenKind::Gt => BinaryOp::Greater,
        TokenKind::Ge => BinaryOp::GreaterEq,
        _ => break, // no more comparison operators
      };
      self.advance(); // consume operator

      // parse the right side with the same precedence level beneath comparison
      let rhs = self.parse_bitwise_or(context)?;

      let _token = self.current_token().kind;
      if !_token.can_start_expression()
        && !_token.can_continue_expression_or(TokenKind::Semi)
        && !self.is_eof()
      {
        let bad = self.current_token();
        let lexeme = self.get_token_lexeme(&bad);

        let diagnostic = self
          .diagnostic(
            DiagnosticError::UnexpectedToken,
            "invalid right-hand side of comparison expression",
          )
        .with_label(
          bad.span,
          Some(format!(
            "expected an expression after the comparison operator, found `{lexeme}`"
          )),
          LabelStyle::Primary,
        )
        .with_help("comparison operators must be followed by a valid expression".to_string())
        .with_note("examples: `x == y`, `x != y`, `x < y`, `x <= y`, `x > y`, `x >= y`".to_string())
        .with_note(
          "Rust parses `a < b < c` as `(a < b) < c`, which is almost always incorrect".to_string(),
        );

        self.emit(diagnostic);
        return Err(());
      }

      lhs = Expr {
        attributes: vec![],
        kind: ExprKind::Binary {
          left: Box::new(lhs),
          op,
          right: Box::new(rhs),
        },
        span: *token.span.merge(self.current_token().span),
      };
    }

    Ok(lhs)
  }
}
