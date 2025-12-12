//! TODO:
//! - Rust allows outer attributes on visibility such as #[cfg], but those
//!   belong to the item parser, not here.
//! - The simple path parser used by pub(in ...) must support full Rust paths
//!   including leading colons, self, super, and $crate if macros are added.
//! - Validate that pub(in ...) uses a valid module path in the semantic pass.
//! - Rust allows whitespace and comments everywhere; ensure lexer already
//!   provides that behavior.
//! - Future: support macro fragments that may expand into visibility.
//! - Future: handle token trees in proc macro contexts where visibility is
//!   not constrained by normal grammar.

use diagnostic::code::DiagnosticCode;
use diagnostic::diagnostic::{Diagnostic, LabelStyle};
use diagnostic::types::error::DiagnosticError;
use diagnostic::DiagnosticEngine;
use lexer::token::TokenKind;

use crate::{ast::Visibility, Parser};

impl Parser {
  /// Parses a Rust visibility modifier.
  ///
  /// Rust grammar (simplified):
  ///
  /// visibility :
  ///     "pub"
  ///   | "pub" "(" "crate" ")"
  ///   | "pub" "(" "self" ")"
  ///   | "pub" "(" "super" ")"
  ///   | "pub" "(" "in" simple_path ")"
  ///
  /// Notes:
  /// - If no "pub" keyword is present, the visibility is private.
  /// - The "in" form accepts any crate-visible path.
  /// - "pub(in crate)" is equivalent to "pub(crate)".
  ///
  /// Returns:
  /// - A Visibility enum describing the parsed visibility.
  ///
  /// Examples:
  /// pub struct Foo
  /// pub(crate) mod util
  /// pub(self) fn f
  /// pub(super) fn g
  /// pub(in path::to::module) type X
  pub(crate) fn parse_visibility(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Visibility, ()> {
    let token = self.current_token();

    // If not "pub" then visibility is private.
    if !matches!(token.kind, TokenKind::KwPub) {
      return Ok(Visibility::Private);
    }

    self.advance(engine); // consume "pub"

    // Check for restricted form: pub( ... )
    if matches!(self.current_token().kind, TokenKind::OpenParen) {
      self.advance(engine); // consume "("

      let restriction = self.current_token();
      self.advance(engine); // consume identifier inside parens

      let visibility = match restriction.kind {
        TokenKind::KwCrate => Ok(Visibility::LicCrate),
        TokenKind::KwSelf => Ok(Visibility::LicSelf),
        TokenKind::KwSuper => Ok(Visibility::LicSuper),
        TokenKind::KwIn => Ok(Visibility::LicIn(self.parse_path(false, engine)?)),

        _ => {
          let lexeme = self.get_token_lexeme(&restriction);
          let diagnostic = Diagnostic::new(
            DiagnosticCode::Error(DiagnosticError::InvalidVisibilityRestriction),
            format!("invalid visibility restriction '{}'", lexeme),
            self.source_file.path.clone(),
          )
          .with_label(
            restriction.span,
            Some(format!(
              "expected crate, self, super, or in <path>, found {}",
              lexeme
            )),
            LabelStyle::Primary,
          )
          .with_help(
            "valid forms are: pub(crate), pub(self), pub(super), pub(in path::to::module)"
              .to_string(),
          );

          engine.add(diagnostic);
          Err(())
        },
      };

      self.expect(TokenKind::CloseParen, engine)?; // consume ")"
      visibility
    } else {
      // Simple "pub"
      Ok(Visibility::Lic)
    }
  }
}
