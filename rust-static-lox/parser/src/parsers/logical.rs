use diagnostic::code::DiagnosticCode;
use diagnostic::diagnostic::{Diagnostic, LabelStyle};
use diagnostic::types::error::DiagnosticError;
use diagnostic::DiagnosticEngine;
use lexer::token::TokenKind;

use crate::ast::BinaryOp;
use crate::parser_utils::ExprContext;
use crate::{ast::Expr, Parser};

impl Parser {
  /// Parses logical OR expressions.
  ///
  /// Grammar:
  ///
  ///   logicalOr ::= logicalAnd ( "||" logicalAnd )*
  ///
  /// Properties:
  /// - Left associative.
  /// - Short-circuiting semantics.
  /// - Left operand is parsed using `parse_logical_and()`.
  ///
  /// Example:
  ///   a || b || c
  pub(crate) fn parse_logical_or(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut lhs = self.parse_logical_and(context, engine)?;

    loop {
      let token = self.current_token();
      match token.kind {
        TokenKind::Or if self.peek(1).kind == TokenKind::Or => {
          self.advance(engine); //
          self.advance(engine); // consume the "||"

          if !self.current_token().kind.can_start_expression() {
            let bad = self.current_token();
            let lexeme = self.get_token_lexeme(&bad);

            let diagnostic = Diagnostic::new(
              DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
              "invalid right-hand side of logical OR expression".to_string(),
              self.source_file.path.clone(),
            )
            .with_label(
              bad.span,
              Some(format!(
                "expected an expression after `||`, found `{lexeme}`"
              )),
              LabelStyle::Primary,
            )
            .with_help(
              "logical OR requires a left and a right expression, for example: a || b".to_string(),
            )
            .with_note("examples: x || y, flags || MASK, (a && b) || c".to_string());

            engine.add(diagnostic);
            return Err(());
          }

          let rhs = self.parse_logical_and(context, engine)?;

          lhs = Expr::Binary {
            op: BinaryOp::Or,
            left: Box::new(lhs),
            right: Box::new(rhs),
            span: token.span,
          };
        },
        _ => break,
      }
    }

    Ok(lhs)
  }

  /// Parses logical AND expressions.
  ///
  /// Grammar:
  ///
  ///   logicalAnd ::= comparison ( "&&" comparison )*
  ///
  /// Properties:
  /// - Left associative.
  /// - Short-circuiting semantics.
  /// - Left operand is parsed using `parse_comparison()`.
  ///
  /// Example:
  ///   a && b && c
  pub(crate) fn parse_logical_and(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut lhs = self.parse_comparison(context, engine)?;

    loop {
      let token = self.current_token();
      match token.kind {
        TokenKind::And if self.peek(1).kind == TokenKind::And => {
          self.advance(engine);
          self.advance(engine);

          if !self.current_token().kind.can_start_expression() {
            let bad = self.current_token();
            let lexeme = self.get_token_lexeme(&bad);

            let diagnostic = Diagnostic::new(
              DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
              "invalid right-hand side of logical AND expression".to_string(),
              self.source_file.path.clone(),
            )
            .with_label(
              bad.span,
              Some(format!(
                "expected an expression after `&&`, found `{lexeme}`"
              )),
              LabelStyle::Primary,
            )
            .with_help(
              "logical AND requires a left and a right expression, for example: a && b".to_string(),
            )
            .with_note("examples: x && y, flags && MASK, (a || b) && c".to_string());

            engine.add(diagnostic);
            return Err(());
          }

          let rhs = self.parse_comparison(context, engine)?;

          lhs = Expr::Binary {
            op: BinaryOp::And,
            left: Box::new(lhs),
            right: Box::new(rhs),
            span: token.span,
          };
        },
        _ => break,
      }
    }

    Ok(lhs)
  }
}
