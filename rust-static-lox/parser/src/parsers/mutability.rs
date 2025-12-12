//! TODO: Support `_` (wildcard) as a valid pattern start (e.g. `mut _` is legal in Rust).
//!
//! TODO: Accept patterns starting with `ref` and `ref mut`.
//!       Example valid Rust patterns:
//!         `ref x`
//!         `ref mut x`
//!       Currently `ref` is rejected because it is not in the allowed token set.
//!
//! TODO: Support `box` patterns (`box x`) if your language intends to mimic Rust pre-2021 syntax.
//!
//! TODO: Allow pattern starts beginning with `|` inside or-patterns.
//!       Example: `let (mut |x| y) = ...`
//!       (Only if your language implements Rust or-patterns.)
//!
//! TODO: Ensure this mutability parser interoperates with full pattern grammar.
//!       Rust allows many pattern starts that are not yet included:
//!         - literal patterns (numbers, strings)
//!         - path patterns (`A::B(x)`)
//!         - struct patterns (`Point { x, y }`)
//!         - tuple patterns (`(a, b)`)
//!         - slice patterns (`[a, b, ..rest]`)
//!
//! TODO: Mutability must only apply to *binding identifiers*, not to all pattern forms.
//!       Example: `mut 3` or `mut A { .. }` must be rejected.
//!
//! TODO: Add support for `mut` in `for` loops, e.g. `for mut x in iter {}`.
//!
//! TODO: Reject `mut` followed by invalid keywords (e.g., `mut let`, `mut pub`)
//!       with a diagnostic matching rustc behavior.
//!
//! TODO: If you plan to support match ergonomics, ensure that parser distinguishes
//!       pattern mutability from reference binding mutability automatically.
//!
//! TODO: Treat `mut` as a pattern-only modifier — it has no meaning in type contexts.
//!       (This is correct but should be enforced consistently across parser entry points.)

use crate::ast::Mutability;
use crate::{DiagnosticEngine, Parser};
use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
};
use lexer::token::TokenKind;

impl Parser {
  /// Parses an optional mut keyword used in patterns or bindings.
  ///
  /// Rust grammar simplified:
  /// patternBinding -> "mut"? IDENTIFIER
  ///
  /// Notes:
  /// - Only the keyword mut indicates mutability.
  /// - The keyword const does not represent mutability. It begins a constant item.
  /// - The pattern &mut x is handled by the type parser, not this function.
  ///
  /// Returns:
  /// - Mutability::Mutable if the mut keyword is present.
  /// - Mutability::Immutable otherwise.
  pub(crate) fn parse_mutability(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Mutability, ()> {
    let token = self.current_token();

    // Only the mut keyword is considered a mutability specifier.
    if matches!(token.kind, TokenKind::KwMut) {
      self.advance(engine); // consume mut
      return Ok(Mutability::Mutable);
    } else if matches!(token.kind, TokenKind::KwConst) {
      self.advance(engine); // consume const
      return Ok(Mutability::Immutable);
    }

    Ok(Mutability::Immutable)
    /*
    // These tokens can legally start a pattern or binding where mutability is optional.
    // Examples:
    //   x          -> Ident
    //   [a, b]     -> OpenBracket
    //   (a, b)     -> OpenParen
    //   &x         -> And
    //   'a x       -> Lifetime
    //   ..         -> DotDot
    //
    // The keyword const is allowed to appear here but does not indicate mutability.
    if matches!(
      token.kind,
      TokenKind::Ident
        | TokenKind::OpenBracket
        | TokenKind::OpenParen
        | TokenKind::Lifetime { .. }
        | TokenKind::And
        | TokenKind::KwConst
        | TokenKind::DotDot
        | TokenKind::Lt
    ) {
      return Ok(Mutability::Immutable);
    }

    // Invalid token in a place where mutability or a pattern should begin.
    let lexeme = self.get_token_lexeme(&token);

    let diagnostic = Diagnostic::new(
      DiagnosticCode::Error(DiagnosticError::InvalidMutability),
      format!("unexpected token `{}` while parsing mutability", lexeme),
      self.source_file.path.clone(),
    )
    .with_label(
      token.span,
      Some(format!(
        "expected `mut` or a valid pattern start, found `{}`",
        lexeme
      )),
      LabelStyle::Primary,
    )
    .with_help("mutability is written as: mut x".to_string())
    .with_note("const begins a constant item and is not part of pattern mutability".to_string());

    engine.add(diagnostic);
    Err(())
    */
  }
}
