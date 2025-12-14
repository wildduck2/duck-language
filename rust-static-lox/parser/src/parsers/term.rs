use diagnostic::DiagnosticEngine;
use lexer::token::TokenKind;

use crate::ast::BinaryOp;
use crate::parser_utils::ExprContext;
use crate::{ast::Expr, Parser};

impl Parser {
  //! TODO: term parser missing full Rust behavior
  //! - Overloaded operator resolution is done in type checking, not parsing.
  //! - Unary plus and unary minus are handled in unary parser, not term.
  //! - Parsing is correct for all valid Rust additive expression forms.

  /// Parses additive expressions.
  ///
  /// Rust grammar reference (simplified):
  ///
  /// ```
  /// term
  ///   -> factor
  ///      ( "+" factor
  ///      | "-" factor )*
  /// ```
  ///
  /// Meaning:
  /// - Operators plus and minus have left associativity.
  /// - This parser produces a left associative tree of binary expressions.
  ///
  /// Examples:
  ///   a + b
  ///   a - b - c
  ///   x + y * z   (multiplication handled in factor)
  ///
  /// Returns:
  ///   Expr representing a chain of addition or subtraction operations.
  pub(crate) fn parse_term(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut lhs = self.parse_factor(context, engine)?;

    'term_find: while !self.is_eof() {
      let token = self.current_token();
      match token.kind {
        TokenKind::Plus | TokenKind::Minus => {
          self.advance(engine); // consume the operator

          let op = match token.kind {
            TokenKind::Plus => BinaryOp::Add,
            TokenKind::Minus => BinaryOp::Sub,
            _ => unreachable!(),
          };

          let rhs = self.parse_factor(context, engine)?;

          lhs = Expr::Binary {
            op,
            left: Box::new(lhs),
            right: Box::new(rhs),
            span: token.span,
          };
        },
        _ => break 'term_find,
      }
    }

    Ok(lhs)
  }
}
