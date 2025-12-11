//! TODO: Support turbofish after `::` and after path segments:
//!       Example: `foo::bar::<T, U>`.
//!       Current parser only reads `<` when immediately after IDENT,
//!       but Rust allows `IDENT::<T>` and `(path):: <T>` patterns.

//! TODO: Properly handle `>>` in generic arguments:
//!       Rust requires token-splitting so that `Vec<Vec<T>>` parses
//!       as `>` `>` not as shift operator.
//!       Your lexer likely already splits tokens, but parser MUST
//!       not treat `>>` as shift when inside generics.

//! TODO: Implement full generic argument grammar:
//!       genericArgs -> "<" (genericArg ("," genericArg)*)? ">".
//!       genericArg  -> type | lifetime | constExpr.
//!       You currently parse only simple type lists, but Rust allows
//!       lifetimes `'a`, const args `3`, and complex types.

//! TODO: Enforce that `$crate` MUST appear only as the *first* segment
//!       and ONLY when no leading `::` was used.
//!       Rust forbids: `foo::$crate::bar` and `::$crate::foo`.

//! TODO: Implement support for keyword paths like `Self::Item`
//!       and `<T as Trait>::Assoc`.  
//!       Your parser currently rejects `<` because it stops path parsing
//!       when encountering `<`, but Rust allows type-qualified paths.

//! TODO: Support type-qualified path syntax:
//!       `<Type as Trait>::Assoc`
//!       Grammar: qpath -> "<" type "as" path ">" "::" IDENT (genericArgs?)
//!       Rust uses this for trait-associated items.

//! TODO: Handle associated type bindings inside generic args:
//!       `Iterator<Item = T>`
//!       This is valid Rust generic syntax.

//! TODO: Permit macros in paths when parsing expressions:
//!       `foo::bar!()`
//!       Rust allows macro invocation as a postfix of a path.

//! TODO: Reject empty path segments like `:::`
//!       This currently would cause your parser to try parsing a segment
//!       after each `::`, but Rust rejects `foo:::bar`.

//! TODO: Distinguish expression-paths vs type-paths:
//!       Rust allows `crate::foo()` (expr) and `crate::Foo` (type).
//!       Some grammar differences exist:
//!       type paths allow `Trait::method`.
//!       expression paths allow tuple indices `.0``.
//!       This may need separate parsing modes.

use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
};
use lexer::token::TokenKind;

use crate::{ast::path::*, DiagnosticEngine, Parser};

impl Parser {
  /// Parses a **path expression or type path**, e.g. `std::io::Read` or `crate::foo::<T>`.
  ///
  /// Grammar (simplified):
  /// ```
  /// path          -> ( "::" )? pathSegment ( "::" pathSegment )*
  /// pathSegment   -> IDENTIFIER genericArgs? | "self" | "super" | "crate" | "$crate"
  /// ```
  ///
  /// - `with_args`: whether generic arguments (`::<T>`) should be parsed.
  /// - Returns a [`Path`] struct containing all segments.
  ///
  /// Examples:
  /// ```rust
  /// self.parse_path(true, engine)?;   // Parses `foo::<T>`
  /// self.parse_path(false, engine)?;  // Parses `foo`
  /// ```
  pub(crate) fn parse_path(
    &mut self,
    with_args: bool,
    engine: &mut DiagnosticEngine,
  ) -> Result<Path, ()> {
    // Handle leading '::' (absolute paths)
    let mut leading_colon = false;
    if matches!(self.current_token().kind, TokenKind::ColonColon) {
      leading_colon = true;
      self.advance(engine); // consume '::'
    }

    // Parse the first segment
    let (first_segment, has_dollar_crate) = self.parse_path_segment(with_args, engine)?;
    let mut segments = vec![first_segment];

    // Parse additional `::`-separated segments
    while !self.is_eof()
      && !matches!(
        self.current_token().kind,
        TokenKind::CloseBracket
          | TokenKind::Lt
          | TokenKind::Eq
          | TokenKind::OpenParen
          | TokenKind::OpenBrace
          | TokenKind::CloseParen
          | TokenKind::Comma
          | TokenKind::Gt
          | TokenKind::Plus
          | TokenKind::Colon
          | TokenKind::KwAs
          | TokenKind::FatArrow
          | TokenKind::Bang
          | TokenKind::Dot
      )
    {
      self.expect(TokenKind::ColonColon, engine)?; // require '::' separator

      let (segment, is_dollar_crate) = self.parse_path_segment(with_args, engine)?;
      if is_dollar_crate {
        // `$crate` can only appear as the first path segment
        let offending = self.peek_prev(0);
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
          "Unexpected `$crate` segment in path".to_string(),
          self.source_file.path.clone(),
        )
        .with_label(
          offending.span,
          Some("`$crate` cannot appear in this position".to_string()),
          LabelStyle::Primary,
        )
        .with_help("`$crate` is only valid as the first path segment.".to_string());
        engine.add(diagnostic);
        return Err(());
      }

      segments.push(segment);
    }

    Ok(Path {
      leading_colon: leading_colon || has_dollar_crate,
      segments,
    })
  }

  /// Parses a **single path segment**, optionally with generic arguments.
  ///
  /// Grammar:
  /// ```
  /// pathSegment  -> IDENTIFIER genericArgs? | "self" | "super" | "crate" | "$crate"
  /// ```
  ///
  /// Returns:
  /// - `(PathSegment, bool)` → where `bool` indicates if `$crate` appeared.
  ///
  /// Examples:
  /// ```rust
  /// let (seg, is_dollar_crate) = self.parse_path_segment(true, engine)?;
  /// ```
  pub(crate) fn parse_path_segment(
    &mut self,
    with_args: bool,
    engine: &mut DiagnosticEngine,
  ) -> Result<(PathSegment, bool), ()> {
    let token = self.current_token();
    self.advance(engine); // consume the segment identifier or keyword

    // Optional generic arguments
    let args = if with_args && matches!(self.current_token().kind, TokenKind::Lt) {
      self.parse_path_generic_args(engine)?
    } else if matches!(self.current_token().kind, TokenKind::ColonColon)
      && matches!(self.peek(1).kind, TokenKind::Lt)
    {
      self.advance(engine);
      self.parse_path_generic_args(engine)?
    } else {
      None
    };

    match token.kind {
      TokenKind::KwSelf => Ok((PathSegment::new(PathSegmentKind::Self_, args), false)),
      TokenKind::KwSuper => Ok((PathSegment::new(PathSegmentKind::Super, args), false)),
      TokenKind::KwCrate => Ok((PathSegment::new(PathSegmentKind::Crate, args), false)),
      TokenKind::Ident => Ok((
        PathSegment::new(PathSegmentKind::Ident(self.get_token_lexeme(&token)), args),
        false,
      )),
      TokenKind::Dollar if self.peek(0).kind == TokenKind::KwCrate => {
        self.advance(engine); // consume `$crate`
        Ok((PathSegment::new(PathSegmentKind::DollarCrate, args), true))
      },
      _ => {
        // Invalid path segment (e.g. `123::foo`)
        let lexeme = self.get_token_lexeme(&token);
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
          "Unexpected token in path segment".to_string(),
          self.source_file.path.clone(),
        )
        .with_label(
          token.span,
          Some(format!("Expected a path segment, found `{lexeme}`")),
          LabelStyle::Primary,
        )
        .with_help("Valid path segments are identifiers or keywords like `self`, `super`, `crate`, or `$crate`.".to_string());
        engine.add(diagnostic);
        Err(())
      },
    }
  }
}
