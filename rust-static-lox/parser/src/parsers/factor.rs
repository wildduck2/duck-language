use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::TokenKind;

use crate::ast::{BinaryOp, ExprKind};
use crate::parser_utils::ParserContext;
use crate::{ast::Expr, Parser};

impl Parser {
  pub(crate) fn parse_factor(&mut self, context: ParserContext) -> Result<Expr, ()> {
    // start with the next higher-precedence expression
    let mut lhs = self.parse_cast(context)?;

    loop {
      let mut token = self.current_token();

      let op = match token.kind {
        TokenKind::Star => BinaryOp::Mul,
        TokenKind::Slash => BinaryOp::Div,
        TokenKind::Percent => BinaryOp::Mod,
        _ => break, // not a factor operator
      };

      self.advance(); // consume operator

      if matches!(
        self.current_token().kind,
        TokenKind::Star | TokenKind::Slash | TokenKind::Percent
      ) {
        let bad = self.current_token();
        let lexeme = self.get_token_lexeme(&bad);

        let diagnostic = self
          .diagnostic(
            DiagnosticError::UnexpectedToken,
            "invalid right-hand side of factor expression",
          )
          .with_label(
            bad.span,
            Some(format!(
              "expected an expression after the factor operator, found `{lexeme}`"
            )),
            LabelStyle::Primary,
          )
          .with_help("factor operators must be followed by a valid expression".to_string())
          .with_note("examples: `x * y`, `x / y`, `x % y`".to_string())
          .with_note(
            "Rust parses `a * b * c` as `(a * b) * c`, which is almost always incorrect"
              .to_string(),
          );

        self.emit(diagnostic);
        return Err(());
      }

      if !self.current_token().kind.can_start_expression() {
        let bad = self.current_token();
        let lexeme = self.get_token_lexeme(&bad);

        let diagnostic = self
          .diagnostic(
            DiagnosticError::UnexpectedToken,
            "invalid right-hand side of factor expression",
          )
          .with_label(
            bad.span,
            Some(format!(
              "expected an expression after the factor operator, found `{lexeme}`"
            )),
            LabelStyle::Primary,
          )
          .with_help("factor operators must be followed by a valid expression".to_string())
          .with_note("examples: `x * y`, `x / y`, `x % y`".to_string())
          .with_note(
            "Rust parses `a * b * c` as `(a * b) * c`, which is almost always incorrect"
              .to_string(),
          );

        self.emit(diagnostic);
        return Err(());
      }

      // parse the next cast-level expression (not parse_factor)
      let rhs = self.parse_cast(context)?;

      lhs = Expr {
        attributes: vec![], // TODO: implement attributes
        kind: ExprKind::Binary {
          left: Box::new(lhs),
          op,
          right: Box::new(rhs),
        },
        span: *token.span.merge(self.last_token_span()),
      };
    }

    Ok(lhs)
  }
}
