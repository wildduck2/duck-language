use diagnostic::code::DiagnosticCode;
use diagnostic::diagnostic::{Diagnostic, LabelStyle};
use diagnostic::types::error::DiagnosticError;
use diagnostic::DiagnosticEngine;
use lexer::token::TokenKind;

use crate::ast::BinaryOp;
use crate::parser_utils::ExprContext;
use crate::{ast::Expr, Parser};

impl Parser {
  /// Parses comparison expressions using the standard comparison operators.
  ///
  /// Grammar:
  ///
  ///   comparison ::= bitwiseOr ( compOp bitwiseOr )*
  ///
  ///   compOp ::= "==" | "!=" | "<" | "<=" | ">" | ">="
  ///
  /// Description:
  /// - A comparison expression consists of a left operand and zero or more
  ///   comparison operator + operand pairs.
  /// - Each operand is parsed using `parse_bitwise_or`, which has higher
  ///   precedence than comparison operators.
  ///
  /// Associativity:
  /// - Comparison operators associate from left to right.
  ///   Example: a < b < c parses as (a < b) < c.
  ///
  /// Error Handling:
  /// - If the right side of a comparison operator does not begin a valid
  ///   expression, an UnexpectedToken diagnostic is emitted.
  ///
  /// Examples:
  ///   x == y
  ///   a != b
  ///   count < limit
  ///   low <= mid <= high   (parsed as left associative)
  ///
  pub(crate) fn parse_comparison(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    // first parse the next higher precedence level
    let mut lhs = self.parse_bitwise_or(context, engine)?;

    loop {
      let token = self.current_token();
      let op = match token.kind {
        TokenKind::EqEq => BinaryOp::Eq,
        TokenKind::Ne => BinaryOp::NotEq,
        TokenKind::Lt => BinaryOp::Less,
        TokenKind::Le => BinaryOp::LessEq,
        TokenKind::Gt => BinaryOp::Greater,
        TokenKind::Ge => BinaryOp::GreaterEq,
        _ => break, // no more comparison operators
      };
      self.advance(engine); // consume operator

      // parse the right side with the same precedence level beneath comparison
      let rhs = self.parse_bitwise_or(context, engine)?;

      let _token = self.current_token().kind;
      if !_token.can_start_expression()
        && !_token.can_continue_expression_or(TokenKind::Semi)
        && !self.is_eof()
      {
        let bad = self.current_token();
        let lexeme = self.get_token_lexeme(&bad);

        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
          "invalid right-hand side of comparison expression".to_string(),
          self.source_file.path.clone(),
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

        engine.add(diagnostic);
        return Err(());
      }

      lhs = Expr::Binary {
        op,
        left: Box::new(lhs),
        right: Box::new(rhs),
        span: token.span,
      };
    }

    Ok(lhs)
  }
}
