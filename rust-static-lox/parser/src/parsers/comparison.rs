use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::TokenKind;

use crate::ast::expr::{BinaryOp, Expr, ExprKind};
use crate::parser_utils::ParserContext;
use crate::Parser;

impl Parser {
  pub(crate) fn parse_comparison(&mut self, context: ParserContext) -> Result<Expr, ()> {
    let lhs = self.parse_bitwise_or(context)?;

    // This solves the problem like this, where we have '>' and it's not
    // the place we should have a comparison in the AST
    // fn foo< const N: usize = 3>() {}
    if matches!(self.peek(1).kind, TokenKind::Colon) || matches!(context, ParserContext::Function) {
      return Ok(lhs);
    }

    let mut token = self.current_token();
    let op = match token.kind {
      TokenKind::EqEq => BinaryOp::Eq,
      TokenKind::Ne => BinaryOp::NotEq,
      TokenKind::Lt => BinaryOp::Less,
      TokenKind::Le => BinaryOp::LessEqual,
      TokenKind::Gt => BinaryOp::Greater,
      TokenKind::Ge => BinaryOp::GreaterEq,
      _ => return Ok(lhs), // no comparison operator
    };
    self.advance(); // consume operator

    let rhs = self.parse_bitwise_or(context)?;

    let _token = self.current_token().kind;
    if !_token.can_start_expression()
      && !_token.can_continue_expression_or(TokenKind::Semi)
      && !matches!(
        _token,
        TokenKind::RParen
          | TokenKind::RBracket
          | TokenKind::RBrace
          | TokenKind::Comma
          | TokenKind::FatArrow
      )
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

    if matches!(
      self.current_token().kind,
      TokenKind::EqEq
        | TokenKind::Ne
        | TokenKind::Lt
        | TokenKind::Le
        | TokenKind::Gt
        | TokenKind::Ge
    ) && !matches!(context, ParserContext::Type)
    {
      let bad = self.current_token();
      let lexeme = self.get_token_lexeme(&bad);

      let diagnostic = self
        .diagnostic(
          DiagnosticError::UnexpectedToken,
          "chained comparison operators are not allowed",
        )
        .with_label(
          bad.span,
          Some(format!("found `{lexeme}` after a comparison")),
          LabelStyle::Primary,
        )
        .with_help("use parentheses to make the comparison order explicit".to_string())
        .with_note("examples: `(a < b) < c` or `a < (b < c)`".to_string());

      self.emit(diagnostic);
      return Err(());
    }

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Binary {
        left: Box::new(lhs),
        op,
        right: Box::new(rhs),
      },
      span: *token.span.merge(self.last_token_span()),
    })
  }
}
