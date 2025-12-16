use crate::ast::Mutability;
use crate::{DiagnosticEngine, Parser};
use lexer::token::TokenKind;

impl Parser {
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
  }
}
