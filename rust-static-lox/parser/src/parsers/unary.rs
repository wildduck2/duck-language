//! TODO:
//! - Unary operators in Rust can also include address-of raw pointer operators
//!   like "addr_of!" but these are macros, not actual unary grammar.
//! - Rust allows multiple "&mut" sequences only through macro expansion,
//!   but your grammar choice for depth is fine.
//! - Overflow checking for unary minus is done in type checking, not parsing.
//! - Parsing of unary plus ("+") is intentionally omitted because Rust does
//!   not define it as a meaningful unary operator.
//! - Need to ensure that "&mut" on non-lvalues is validated in semantic phase.

use diagnostic::DiagnosticEngine;
use lexer::token::TokenKind;

use crate::ast::UnaryOp;
use crate::match_and_consume;
use crate::parser_utils::ExprContext;
use crate::{ast::Expr, Parser};

impl Parser {
  /// Parses prefix unary operators.
  ///
  /// Rust grammar reference (simplified):
  ///
  /// ```
  /// unary
  ///   -> ( "-"
  ///      | "!"
  ///      | "*"
  ///      | "&" "mut"?
  ///      | "&&" "mut"? ) unary
  ///   | postfix
  /// ```
  ///
  /// Notes:
  /// - This grammar is left-recursive in the spec, but implemented in
  ///   right-associative style here because unary operators bind right.
  /// - "&" and "&&" produce reference operators. The parser records the
  ///   reference depth and optional "mut".
  /// - "*" is the dereference operator.
  /// - "-" is arithmetic negation.
  /// - "!" is logical negation.
  ///
  /// Examples:
  ///   -x
  ///   !flag
  ///   *ptr
  ///   &x
  ///   &mut x
  ///   &&y
  ///
  /// The return value is an Expr::Unary node unless no unary operator is found.
  pub(crate) fn parse_unary(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();

    match token.kind {
      TokenKind::Minus | TokenKind::Bang | TokenKind::Star => {
        self.advance(engine); // consume operator

        let op = match token.kind {
          TokenKind::Minus => UnaryOp::Neg,
          TokenKind::Bang => UnaryOp::Not,
          TokenKind::Star => UnaryOp::Deref,
          _ => unreachable!(),
        };

        let rhs = self.parse_unary(context, engine)?;
        token.span.merge(self.current_token().span);

        Ok(Expr::Unary {
          expr: Box::new(rhs),
          op,
          span: token.span,
        })
      }
      TokenKind::And => {
        self.advance(engine); // consume the first '&'

        let mut depth = 1;
        while match_and_consume!(self, engine, TokenKind::And)? {
          depth += 1;
        }

        let mutable = match_and_consume!(self, engine, TokenKind::KwMut)?;

        let rhs = self.parse_unary(context, engine)?;
        token.span.merge(self.current_token().span);

        Ok(Expr::Unary {
          expr: Box::new(rhs),
          op: UnaryOp::Ref { mutable, depth },
          span: token.span,
        })
      }
      _ => self.parse_postfix(context, engine),
    }
  }
}
