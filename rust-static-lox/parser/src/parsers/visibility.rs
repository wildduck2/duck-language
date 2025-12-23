use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::TokenKind;

use crate::{ast::Visibility, parser_utils::ParserContext, Parser};

impl Parser {
  pub(crate) fn parse_visibility(&mut self, context: ParserContext) -> Result<Visibility, ()> {
    let token = self.current_token();

    // not `pub` means private visibility
    if !matches!(token.kind, TokenKind::Kwpub) {
      return Ok(Visibility::Private);
    }

    self.advance(); // consume `pub`

    // restricted form: pub(...)
    if matches!(self.current_token().kind, TokenKind::LParen) {
      self.advance(); // consume '('

      let restriction = self.current_token();
      self.advance(); // consume keyword or `in`

      let visibility = match restriction.kind {
        TokenKind::KwCrate => Ok(Visibility::PublicCrate),
        TokenKind::KwSelf => Ok(Visibility::PublicSelf),
        TokenKind::KwSuper => Ok(Visibility::PublicSuper),

        TokenKind::KwIn => {
          let path = self.parse_path(false, context)?;
          Ok(Visibility::PublicIn(path))
        },

        _ => {
          let lexeme = self.get_token_lexeme(&restriction);
          let diagnostic = self
            .diagnostic(
              DiagnosticError::InvalidVisibilityRestriction,
              format!("invalid visibility restriction '{lexeme}'"),
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
              "valid forms are: pub, pub(self), pub(super), pub(in path::to::module)".to_string(),
            );

          self.emit(diagnostic);
          return Err(());
        },
      };

      self.expect(TokenKind::RParen)?;
      visibility
    } else {
      // plain `pub`
      Ok(Visibility::Public)
    }
  }
}
