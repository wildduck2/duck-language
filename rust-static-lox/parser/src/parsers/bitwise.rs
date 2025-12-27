use crate::{
  ast::expr::{BinaryOp, Expr, ExprKind},
  parser_utils::ParserContext,
  Parser,
};
use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};

use lexer::token::TokenKind;
impl Parser {
  pub(crate) fn parse_bitwise_or(&mut self, context: ParserContext) -> Result<Expr, ()> {
    let mut lhs = self.parse_bitwise_xor(context)?;

    if matches!(context, ParserContext::Match | ParserContext::IfCondition) {
      return Ok(lhs);
    }

    while !self.is_eof()
      && !matches!(context, ParserContext::Match)
      && !matches!(self.peek(1).kind, TokenKind::Or)
    {
      let mut token = self.current_token();

      match token.kind {
        TokenKind::Or => {
          self.advance(); // consume the "|"

          if !self
            .current_token()
            .kind
            .can_start_expression_and_not(TokenKind::Or)
          {
            let bad = self.current_token();
            let lexeme = self.get_token_lexeme(&bad);

            let diagnostic = self
              .diagnostic(
                DiagnosticError::UnexpectedToken,
                "invalid right-hand side of bitwise OR expression",
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

            self.emit(diagnostic);
            return Err(());
          }

          let rhs = self.parse_bitwise_xor(context)?;

          lhs = Expr {
            attributes: vec![],
            kind: ExprKind::Binary {
              left: Box::new(lhs),
              op: BinaryOp::BitOr,
              right: Box::new(rhs),
            },
            span: *token.span.merge(self.last_token_span()),
          };
        },

        _ => break,
      }
    }

    Ok(lhs)
  }

  pub(crate) fn parse_bitwise_xor(&mut self, context: ParserContext) -> Result<Expr, ()> {
    let mut lhs = self.parse_bitwise_and(context)?;

    while !self.is_eof() && !matches!(self.peek(1).kind, TokenKind::Caret) {
      let mut token = self.current_token();

      match token.kind {
        TokenKind::Caret => {
          self.advance(); // consume the "^"

          if !self
            .current_token()
            .kind
            .can_start_expression_and_not(TokenKind::Caret)
          {
            let bad = self.current_token();
            let lexeme = self.get_token_lexeme(&bad);

            let diagnostic = self
              .diagnostic(
                DiagnosticError::UnexpectedToken,
                "invalid right-hand side of bitwise XOR expression",
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
                "bitwise XOR requires a left and a right expression, for example: a ^ b"
                  .to_string(),
              )
              .with_note("examples: x ^ y, flags ^ MASK, (a & b) ^ c".to_string());

            self.emit(diagnostic);
            return Err(());
          }

          let rhs = self.parse_bitwise_and(context)?;

          lhs = Expr {
            attributes: vec![],
            kind: ExprKind::Binary {
              left: Box::new(lhs),
              op: BinaryOp::BitXor,
              right: Box::new(rhs),
            },
            span: *token.span.merge(self.last_token_span()),
          };
        },

        _ => break,
      }
    }

    Ok(lhs)
  }

  pub(crate) fn parse_bitwise_and(&mut self, context: ParserContext) -> Result<Expr, ()> {
    let mut lhs = self.parse_shift(context)?;

    while !self.is_eof() && !matches!(self.peek(1).kind, TokenKind::Amp) {
      let mut token = self.current_token();

      match token.kind {
        TokenKind::Amp => {
          self.advance();

          if !self
            .current_token()
            .kind
            .can_start_expression_and_not(TokenKind::Amp)
          {
            let bad = self.current_token();
            let lexeme = self.get_token_lexeme(&bad);

            let diagnostic = self
              .diagnostic(
                DiagnosticError::UnexpectedToken,
                "invalid right-hand side of bitwise AND expression",
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
                "bitwise AND requires a left and a right expression, for example: a & b"
                  .to_string(),
              )
              .with_note("examples: x & y, flags & MASK, (a | b) & c".to_string());

            self.emit(diagnostic);
            return Err(());
          }

          let rhs = self.parse_shift(context)?;

          lhs = Expr {
            attributes: vec![],
            kind: ExprKind::Binary {
              left: Box::new(lhs),
              op: BinaryOp::BitAnd,
              right: Box::new(rhs),
            },
            span: *token.span.merge(self.last_token_span()),
          };
        },

        _ => break,
      }
    }

    Ok(lhs)
  }
}
