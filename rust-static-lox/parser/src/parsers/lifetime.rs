use crate::Parser;
use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_lifetimes(&mut self) -> Result<Vec<String>, ()> {
    let mut lifetimes = vec![];
    while !self.is_eof() && matches!(self.current_token().kind, TokenKind::Lifetime { .. }) {
      lifetimes.push(self.parse_lifetime()?);

      if matches!(self.current_token().kind, TokenKind::Comma) {
        self.advance(); // consume ','
      } else {
        break;
      }
    }
    Ok(lifetimes)
  }

  pub(crate) fn parse_lifetime(&mut self) -> Result<String, ()> {
    let token = self.current_token();

    if matches!(token.kind, TokenKind::Lifetime { .. }) {
      let lifetime = self.get_token_lexeme(&token);
      self.advance(); // consume the lifetime
      return Ok(lifetime);
    }

    let lexeme = self.get_token_lexeme(&self.current_token());
    self.emit(self.err_expected_lifetime(token.span, &lexeme));
    Err(())
  }

  pub(crate) fn parse_for_lifetimes(&mut self) -> Result<Option<Vec<String>>, ()> {
    if matches!(self.current_token().kind, TokenKind::KwFor) {
      self.expect(TokenKind::KwFor)?; // consume 'for'
      self.expect(TokenKind::Lt)?; // consume '<'

      if matches!(self.current_token().kind, TokenKind::Gt) {
        let token = self.current_token();
        let lexeme = self.get_token_lexeme(&token);
        self.emit(self.err_expected_lifetime(token.span, &lexeme));
        return Err(());
      }

      let lifetimes = self.parse_lifetimes()?;
      self.expect(TokenKind::Gt)?; // consume '>'

      Ok(Some(lifetimes))
    } else {
      Ok(None)
    }
  }

  pub(crate) fn parse_lifetime_bounds(&mut self) -> Result<Vec<String>, ()> {
    let mut bounds = vec![];
    while !self.is_eof() && !Self::is_bound_terminator(&self.current_token().kind) {
      let lifetime = self.parse_lifetime()?;
      bounds.push(lifetime);

      if !self.consume_plus_and_require_bound("lifetime bounds", Self::is_lifetime_start)? {
        break;
      }
    }

    Ok(bounds)
  }
}
