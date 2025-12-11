use crate::{ast::Expr, parser_utils::ExprContext, Parser};
use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
  DiagnosticEngine,
};

use crate::ast::BinaryOp;
use lexer::token::TokenKind;
impl Parser {
  /// Parses bitwise OR expressions.
  ///
  /// Grammar:
  ///
  ///   bitwise_or ::= bitwise_xor ( "|" bitwise_xor )*
  ///
  /// This level implements the lowest-precedence bitwise operator.
  ///
  /// Behavior:
  /// - Left associative
  /// - Accepts chained operations: a | b | c
  /// - Correctly interacts with higher-precedence operators: a | b ^ c & d
  ///
  /// Notes:
  /// - This parses only the expression-level "|" operator.
  /// - Pattern and closure "|" are not handled here.
  pub(crate) fn parse_bitwise_or(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut lhs = self.parse_bitwise_xor(context, engine)?;

    if matches!(context, ExprContext::Match | ExprContext::IfCondition) {
      return Ok(lhs);
    }

    while !self.is_eof()
      && !matches!(context, ExprContext::Match)
      && !matches!(self.peek(1).kind, TokenKind::Or)
    {
      let token = self.current_token();

      match token.kind {
        TokenKind::Or => {
          self.advance(engine); // consume the "|"

          if !self
            .current_token()
            .kind
            .can_start_expression_and_not(TokenKind::Or)
          {
            let bad = self.current_token();
            let lexeme = self.get_token_lexeme(&bad);

            let diagnostic = Diagnostic::new(
              DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
              "invalid right-hand side of bitwise OR expression".to_string(),
              self.source_file.path.clone(),
            )
            .with_label(
              bad.span,
              Some(format!(
                "expected an expression after `|`, found `{}`",
                lexeme
              )),
              LabelStyle::Primary,
            )
            .with_help(
              "bitwise OR requires a left and a right expression, for example: a | b".to_string(),
            )
            .with_note("examples: x | y, flags | MASK, (a & b) | c".to_string());

            engine.add(diagnostic);
            return Err(());
          }

          let rhs = self.parse_bitwise_xor(context, engine)?;

          lhs = Expr::Binary {
            op: BinaryOp::BitOr,
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

  /// Parses bitwise XOR expressions.
  ///
  /// Grammar:
  ///
  ///   bitwise_xor ::= bitwise_and ( "^" bitwise_and )*
  ///
  /// This is the middle-precedence bitwise operator.
  ///
  /// Behavior:
  /// - Left associative
  /// - Allows chains like: a ^ b ^ c
  /// - Interacts correctly with bitwise AND, which has higher precedence
  ///
  /// Notes:
  /// - Only the expression-level "^" operator is handled here.
  pub(crate) fn parse_bitwise_xor(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut lhs = self.parse_bitwise_and(context, engine)?;

    while !self.is_eof() && !matches!(self.peek(1).kind, TokenKind::Caret) {
      let token = self.current_token();

      match token.kind {
        TokenKind::Caret => {
          self.advance(engine); // consume the "^"

          if !self
            .current_token()
            .kind
            .can_start_expression_and_not(TokenKind::Caret)
          {
            let bad = self.current_token();
            let lexeme = self.get_token_lexeme(&bad);

            let diagnostic = Diagnostic::new(
              DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
              "invalid right-hand side of bitwise XOR expression".to_string(),
              self.source_file.path.clone(),
            )
            .with_label(
              bad.span,
              Some(format!(
                "expected an expression after `^`, found `{}`",
                lexeme
              )),
              LabelStyle::Primary,
            )
            .with_help(
              "bitwise XOR requires a left and a right expression, for example: a ^ b".to_string(),
            )
            .with_note("examples: x ^ y, flags ^ MASK, (a & b) ^ c".to_string());

            engine.add(diagnostic);
            return Err(());
          }

          let rhs = self.parse_bitwise_and(context, engine)?;

          lhs = Expr::Binary {
            op: BinaryOp::BitXor,
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

  /// Parses bitwise AND expressions.
  ///
  /// Grammar:
  ///
  ///   bitwise_and ::= shift ( "&" shift )*
  ///
  /// This is the highest-precedence bitwise operator.
  ///
  /// Behavior:
  /// - Left associative
  /// - Supports chains like: a & b & c
  ///
  /// Notes:
  /// - This function handles only the binary "&" operator.
  /// - Unary "&" (reference) is parsed in the unary expression level.
  pub(crate) fn parse_bitwise_and(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut lhs = self.parse_shift(context, engine)?;

    while !self.is_eof() && !matches!(self.peek(1).kind, TokenKind::And) {
      let token = self.current_token();

      match token.kind {
        TokenKind::And => {
          self.advance(engine);

          if !self
            .current_token()
            .kind
            .can_start_expression_and_not(TokenKind::And)
          {
            let bad = self.current_token();
            let lexeme = self.get_token_lexeme(&bad);

            let diagnostic = Diagnostic::new(
              DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
              "invalid right-hand side of bitwise AND expression".to_string(),
              self.source_file.path.clone(),
            )
            .with_label(
              bad.span,
              Some(format!(
                "expected an expression after `&`, found `{}`",
                lexeme
              )),
              LabelStyle::Primary,
            )
            .with_help(
              "bitwise AND requires a left and a right expression, for example: a & b".to_string(),
            )
            .with_note("examples: x & y, flags & MASK, (a | b) & c".to_string());

            engine.add(diagnostic);
            return Err(());
          }

          let rhs = self.parse_shift(context, engine)?;

          lhs = Expr::Binary {
            op: BinaryOp::BitAnd,
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
