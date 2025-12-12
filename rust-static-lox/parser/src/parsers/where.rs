//! TODO:
//! add support for predicates of the form (T, U): Trait
//! add support for qualified type paths inside HRTBs
//! add support for equality predicates inside trait bounds such as T: Trait<Item = U>
//! add support for dyn Trait plus separated bounds after equality
//! add support for underscore type predicate heads like _: Trait
//! add support for parenthesized types inside predicates
//! add recovery for malformed predicates such as missing bound after colon
//! ensure type parser supports generic arguments and QSelf forms used in where clauses
//! ensure parse_where_clause stops on all valid Rust terminators beyond braces parens semicolon

use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
  DiagnosticEngine,
};
use lexer::token::TokenKind;

use crate::{ast::generic::*, Parser};

impl Parser {
  /// Parse a `where` clause attached to a type, function, or struct.
  ///
  /// Grammar:
  /// ```
  /// whereClause -> "where" (whereClauseItem ("," whereClauseItem)* ","?)?
  /// ```
  ///
  /// Each `where` predicate is parsed using [`parse_type_predicate`].
  /// Parsing stops when encountering one of `{`, `(`, or `;`, matching
  /// struct, tuple, or function item contexts.
  ///
  /// Returns `Ok(Some(WhereClause))` if a `where` clause is present,
  /// or `Ok(None)` if not found.
  ///
  /// Example:
  /// ```rust
  /// struct Container<T, U> where T: Clone, U: Copy;
  /// ```
  pub(crate) fn parse_where_clause(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Option<WhereClause>, ()> {
    if matches!(self.current_token().kind, TokenKind::KwWhere) {
      self.advance(engine); // consume 'where'

      let mut predicates = vec![];

      while !self.is_eof()
        && !matches!(
          self.current_token().kind,
          TokenKind::OpenBrace | TokenKind::OpenParen | TokenKind::Semi
        )
      {
        predicates.push(self.parse_type_predicate(engine)?);

        if matches!(self.current_token().kind, TokenKind::Comma) {
          self.advance(engine); // consume ','
        }
      }

      return Ok(Some(WhereClause { predicates }));
    }

    Ok(None)
  }

  /// Parse a single `where`-clause predicate.
  ///
  /// Supported predicate forms:
  /// - **Type bound**         -> `T: Clone + Send`
  /// - **Lifetime bound**     ->`'a: 'b + 'c`
  /// - **Higher-ranked trait bound (HRTB)** -> `for<'a> F: Fn(&'a str)`
  /// - **Equality predicate** ->`T::Item = i32`
  ///
  /// Grammar:
  /// ```
  /// whereClauseItem
  ///   -> lifetimeWhereClauseItem
  ///    | typeBoundWhereClauseItem
  ///
  /// typeBoundWhereClauseItem
  ///   -> forLifetimes? type ":" typeParamBounds?
  ///    | forLifetimes? type "=" type
  ///
  /// lifetimeWhereClauseItem
  ///   → LIFETIME ":" lifetimeBounds
  /// ```
  ///
  /// Example:
  /// ```rust
  /// where
  ///   'a: 'b + 'c,
  ///   for<'x> F: Fn(&'x T),
  ///   T::Item = U
  /// ```
  pub(crate) fn parse_type_predicate(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<WherePredicate, ()> {
    let token = self.current_token();

    // Lifetime predicate: `'a: 'b + 'c`
    if matches!(token.kind, TokenKind::Lifetime { .. }) {
      let lifetime = self.get_token_lexeme(&token);
      self.advance(engine); // consume lifetime

      let lifetime_bounds = if matches!(self.current_token().kind, TokenKind::Colon) {
        self.advance(engine); // consume ':'
        self.parse_lifetime_bounds(engine)?
      } else {
        vec![]
      };

      return Ok(WherePredicate::Lifetime {
        lifetime,
        bounds: lifetime_bounds,
      });
    }

    // Optional for<'a, 'b> prefix (applies only to type predicates)
    let for_lifetimes = if matches!(self.current_token().kind, TokenKind::KwFor) {
      Some(self.parse_for_lifetimes(engine)?)
    } else {
      None
    };

    // Parse the left-hand type (e.g. T, <T as Trait>::Item, etc.)
    let ty = self.parse_type(engine)?;

    // Handle either `:` (bounds) or `=` (equality)
    match self.current_token().kind {
      TokenKind::Colon => {
        self.advance(engine); // consume ':'
        let bounds = Some(self.parse_type_bounds(engine)?);
        Ok(WherePredicate::Type {
          for_lifetimes,
          ty,
          bounds,
        })
      },

      TokenKind::Eq => {
        self.advance(engine); // consume '='
        Ok(WherePredicate::Equality {
          ty,
          equals: self.parse_type(engine)?,
        })
      },

      _ => {
        let lexeme = self.get_token_lexeme(&self.current_token());
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidWherePredicate),
          format!("unexpected token `{}` in where-clause predicate", lexeme),
          self.source_file.path.clone(),
        )
        .with_label(
          token.span,
          Some("expected `:`, `=`, or a valid bound here".to_string()),
          LabelStyle::Primary,
        )
        .with_note(
          "a predicate must be one of: `T: Bound`, `'a: 'b`, or `T::Assoc = Type`".to_string(),
        );
        engine.add(diagnostic);
        Err(())
      },
    }
  }
}
