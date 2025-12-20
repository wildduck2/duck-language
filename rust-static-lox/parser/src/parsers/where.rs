use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
};
use lexer::token::TokenKind;

use crate::{ast::generic::*, match_and_consume, Parser};

impl Parser {
  pub(crate) fn parse_where_clause(&mut self) -> Result<Option<WhereClause>, ()> {
    if !matches!(self.current_token().kind, TokenKind::KwWhere) {
      return Ok(None);
    }

    self.advance(); // consume 'where'

    let mut predicates = vec![];

    while !self.is_eof()
      && !matches!(
        self.current_token().kind,
        TokenKind::OpenBrace | TokenKind::Semi | TokenKind::Eq
      )
    {
      predicates.push(self.parse_type_predicate()?);
      match_and_consume!(self, TokenKind::Comma)?;
    }

    if predicates.is_empty() {
      let token = self.current_token();
      let lexeme = self.get_token_lexeme(&token);
      let diagnostic = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::InvalidWhereClause),
        format!("unexpected token `{}` in where-clause", lexeme),
        self.source_file.path.clone(),
      )
      .with_label(
        token.span,
        Some("expected a valid where-clause here".to_string()),
        LabelStyle::Primary,
      )
      .with_note("a where-clause must be a valid predicate, like `T: Clone`".to_string());
      self.engine.borrow_mut().add(diagnostic);
      return Err(());
    }

    Ok(Some(WhereClause { predicates }))
  }

  pub(crate) fn parse_type_predicate(&mut self) -> Result<WherePredicate, ()> {
    let token = self.current_token();

    // Lifetime predicate: `'a: 'b + 'c`
    if matches!(token.kind, TokenKind::Lifetime { .. }) {
      return self.parse_lifetime_predicate();
    }

    // Optional for<'a, 'b> prefix (applies only to type predicates)
    let for_lifetimes = self.parse_for_lifetimes()?;

    // Parse the left-hand type (e.g. T, <T as Trait>::Item, etc.)
    let ty = self.parse_type()?;

    // Handle either `:` (bounds) or `=` (equality)
    match self.current_token().kind {
      TokenKind::Colon => {
        let bounds = self.parse_trait_bounds()?;

        if bounds.is_empty() {
          let token = self.current_token();
          let lexeme = self.get_token_lexeme(&token);
          let diagnostic = Diagnostic::new(
            DiagnosticCode::Error(DiagnosticError::InvalidTraitBound),
            format!("unexpected token `{}` in where-clause predicate", lexeme),
            self.source_file.path.clone(),
          )
          .with_label(
            token.span,
            Some("expected a valid trait bound here".to_string()),
            LabelStyle::Primary,
          )
          .with_note("a trait bound must be a valid type, like `T: Clone`".to_string());
          self.engine.borrow_mut().add(diagnostic);
          return Err(());
        }

        Ok(WherePredicate::Type {
          for_lifetimes,
          ty,
          bounds,
        })
      },

      TokenKind::Eq => {
        self.advance(); // consume '='
        Ok(WherePredicate::Equality {
          ty,
          equals: self.parse_type()?,
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
        self.engine.borrow_mut().add(diagnostic);
        Err(())
      },
    }
  }

  pub(crate) fn parse_lifetime_predicate(&mut self) -> Result<WherePredicate, ()> {
    let token = self.current_token();
    let lifetime = self.get_token_lexeme(&token);
    self.advance(); // consume lifetime

    let lifetime_bounds = if matches!(self.current_token().kind, TokenKind::Colon) {
      self.advance(); // consume ':'
      self.parse_lifetime_bounds()?
    } else {
      vec![]
    };

    return Ok(WherePredicate::Lifetime {
      lifetime,
      bounds: lifetime_bounds,
    });
  }
}
