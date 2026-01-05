use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::TokenKind;

use crate::{ast::generic::*, match_and_consume, parser_utils::ParserContext, Parser};

impl Parser {
  pub(crate) fn parse_where_clause(
    &mut self,
    context: ParserContext,
  ) -> Result<Option<WhereClause>, ()> {
    if !matches!(self.current_token().kind, TokenKind::KwWhere) {
      return Ok(None);
    }

    self.advance(); // consume 'where'

    let mut predicates = vec![];

    while !self.is_eof()
      && !matches!(
        self.current_token().kind,
        TokenKind::LBrace | TokenKind::Semi | TokenKind::Eq
      )
    {
      predicates.push(self.parse_type_predicate(context)?);
      match_and_consume!(self, TokenKind::Comma)?;
    }

    if predicates.is_empty() {
      let token = self.current_token();
      let lexeme = self.get_token_lexeme(&token);
      let diagnostic = self
        .diagnostic(
          DiagnosticError::InvalidWhereClause,
          format!("expected where-clause predicate, found `{lexeme}`"),
        )
        .with_label(
          token.span,
          Some("expected a valid where-clause predicate here".to_string()),
          LabelStyle::Primary,
        )
        .with_note(
          "where-clauses must contain at least one predicate, like `T: Clone` or `'a: 'b`"
            .to_string(),
        );
      self.emit(diagnostic);
      return Err(());
    }

    Ok(Some(WhereClause { predicates }))
  }

  pub(crate) fn parse_type_predicate(
    &mut self,
    context: ParserContext,
  ) -> Result<WherePredicate, ()> {
    let token = self.current_token();

    // Lifetime predicate: `'a: 'b + 'c`
    if matches!(token.kind, TokenKind::Lifetime { .. }) {
      return self.parse_lifetime_predicate();
    }

    // Optional for<'a, 'b> prefix (applies only to type predicates)
    let for_lifetimes = self.parse_for_lifetimes()?;

    // Parse the left-hand type (e.g. T, <T as Trait>::Item, etc.)
    let ty = self.parse_type(context)?;

    // Handle either `:` (bounds) or `=` (equality)
    match self.current_token().kind {
      TokenKind::Colon => {
        let bounds = self.parse_trait_bounds("where-clause", context)?;

        if bounds.is_empty() {
          let token = self.current_token();
          let lexeme = self.get_token_lexeme(&token);
          let diagnostic = self
            .diagnostic(
              DiagnosticError::InvalidTraitBound,
              format!("expected trait bound, found `{lexeme}`"),
            )
            .with_label(
              token.span,
              Some("expected a trait bound here".to_string()),
              LabelStyle::Primary,
            )
            .with_note("trait bounds must be trait names, like `Clone` or `Copy`".to_string());
          self.emit(diagnostic);
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
          equals: self.parse_type(context)?,
        })
      },

      _ => {
        let lexeme = self.get_token_lexeme(&self.current_token());
        let diagnostic = self
          .diagnostic(
            DiagnosticError::InvalidWherePredicate,
            format!("expected where-clause predicate, found `{lexeme}`"),
          )
          .with_label(
            token.span,
            Some(format!(
              "expected a predicate like `T: Trait` or `'a: 'b`, found `{lexeme}`"
            )),
            LabelStyle::Primary,
          )
          .with_note(
            "where-clause predicates must be of the form `Type: Bound`, `'lifetime: 'bound`, or `Type::Assoc = Type`"
              .to_string(),
          );
        self.emit(diagnostic);
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

    Ok(WherePredicate::Lifetime {
      lifetime,
      bounds: lifetime_bounds,
    })
  }
}
