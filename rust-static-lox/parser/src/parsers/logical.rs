use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::TokenKind;

use crate::ast::{BinaryOp, ExprKind};
use crate::parser_utils::ExprContext;
use crate::{ast::Expr, Parser};

impl Parser {
  pub(crate) fn parse_logical_or(&mut self, context: ExprContext) -> Result<Expr, ()> {
    let mut lhs = self.parse_logical_and(context)?;

    loop {
      let token = self.current_token();
      match token.kind {
        TokenKind::Or if self.peek(1).kind == TokenKind::Or => {
          self.advance(); //
          self.advance(); // consume the "||"

          if !self.current_token().kind.can_start_expression() {
            let bad = self.current_token();
            let lexeme = self.get_token_lexeme(&bad);

            let diagnostic = self
              .diagnostic(
                DiagnosticError::UnexpectedToken,
                "invalid right-hand side of logical OR expression",
              )
              .with_label(
                bad.span,
                Some(format!(
                  "expected an expression after `||`, found `{lexeme}`"
                )),
                LabelStyle::Primary,
              )
              .with_help(
                "logical OR requires a left and a right expression, for example: a || b"
                  .to_string(),
              )
              .with_note("examples: x || y, flags || MASK, (a && b) || c".to_string());

            self.emit(diagnostic);
            return Err(());
          }

          let rhs = self.parse_logical_and(context)?;
          lhs = Expr {
            attributes: vec![],
            kind: ExprKind::Binary {
              op: BinaryOp::Or,
              left: Box::new(lhs),
              right: Box::new(rhs),
            },
            span: token.span,
          };
        },
        _ => break,
      }
    }

    Ok(lhs)
  }

  pub(crate) fn parse_logical_and(&mut self, context: ExprContext) -> Result<Expr, ()> {
    let mut lhs = self.parse_comparison(context)?;

    loop {
      let token = self.current_token();
      match token.kind {
        TokenKind::Amp if self.peek(1).kind == TokenKind::Amp => {
          self.advance();
          self.advance();

          if !self.current_token().kind.can_start_expression() {
            let bad = self.current_token();
            let lexeme = self.get_token_lexeme(&bad);

            let diagnostic = self
              .diagnostic(
                DiagnosticError::UnexpectedToken,
                "invalid right-hand side of logical AND expression",
              )
              .with_label(
                bad.span,
                Some(format!(
                  "expected an expression after `&&`, found `{lexeme}`"
                )),
                LabelStyle::Primary,
              )
              .with_help(
                "logical AND requires a left and a right expression, for example: a && b"
                  .to_string(),
              )
              .with_note("examples: x && y, flags && MASK, (a || b) && c".to_string());

            self.emit(diagnostic);
            return Err(());
          }

          let rhs = self.parse_comparison(context)?;

          lhs = Expr {
            attributes: vec![],
            kind: ExprKind::Binary {
              op: BinaryOp::And,
              left: Box::new(lhs),
              right: Box::new(rhs),
            },
            span: token.span,
          };
        },
        _ => break,
      }
    }

    Ok(lhs)
  }
}
