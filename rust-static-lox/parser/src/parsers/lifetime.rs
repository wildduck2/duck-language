use crate::{DiagnosticEngine, Parser};
use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
};
use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_lifetimes(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Vec<String>, ()> {
    let mut lifetimes = vec![];
    while !self.is_eof() && matches!(self.current_token().kind, TokenKind::Lifetime { .. }) {
      lifetimes.push(self.parse_lifetime(engine)?);

      if matches!(self.current_token().kind, TokenKind::Comma) {
        self.advance(engine); // consume ','
      } else {
        break;
      }
    }
    Ok(lifetimes)
  }

  pub(crate) fn parse_lifetime(&mut self, engine: &mut DiagnosticEngine) -> Result<String, ()> {
    let token = self.current_token();

    if matches!(token.kind, TokenKind::Lifetime { .. }) {
      let lifetime = self.get_token_lexeme(&token);
      self.advance(engine); // consume the lifetime
      return Ok(lifetime);
    }

    let lexeme = self.get_token_lexeme(&self.current_token());
    let diagnostic = Diagnostic::new(
      DiagnosticCode::Error(DiagnosticError::InvalidLifetime),
      format!("unexpected token `{}` in lifetime", lexeme),
      self.source_file.path.clone(),
    )
    .with_label(
      token.span,
      Some("expected a valid lifetime here".to_string()),
      LabelStyle::Primary,
    )
    .with_note("a lifetime must be a valid identifier, like `'a` or `'b`".to_string());

    engine.add(diagnostic);
    Err(())
  }

  pub(crate) fn parse_for_lifetimes(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Option<Vec<String>>, ()> {
    if matches!(self.current_token().kind, TokenKind::KwFor) {
      self.expect(TokenKind::KwFor, engine)?; // consume 'for'
      self.expect(TokenKind::Lt, engine)?; // consume '<'

      let lifetimes = self.parse_lifetimes(engine)?;
      self.expect(TokenKind::Gt, engine)?; // consume '>'
      Ok(Some(lifetimes))
    } else {
      Ok(None)
    }
  }

  pub(crate) fn parse_lifetime_bounds(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Vec<String>, ()> {
    let mut bounds = vec![];
    while !self.is_eof()
      && !matches!(
        self.current_token().kind,
        TokenKind::OpenBrace | TokenKind::Comma | TokenKind::Gt
      )
    {
      let lifetime = self.parse_lifetime(engine)?;

      if matches!(self.current_token().kind, TokenKind::Plus) {
        self.advance(engine); // consume '+'
      }

      bounds.push(lifetime);
    }

    Ok(bounds)
  }
}
