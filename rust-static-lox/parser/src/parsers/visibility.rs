use diagnostic::code::DiagnosticCode;
use diagnostic::diagnostic::{Diagnostic, LabelStyle};
use diagnostic::types::error::DiagnosticError;
use diagnostic::DiagnosticEngine;
use lexer::token::TokenKind;

use crate::{ast::Visibility, Parser};

impl Parser {
  pub(crate) fn parse_visibility(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Visibility, ()> {
    let token = self.current_token();

    // not `pub` means private visibility
    if !matches!(token.kind, TokenKind::KwPub) {
      return Ok(Visibility::Private);
    }

    self.advance(engine); // consume `pub`

    // restricted form: pub(...)
    if matches!(self.current_token().kind, TokenKind::OpenParen) {
      self.advance(engine); // consume '('

      let restriction = self.current_token();
      self.advance(engine); // consume keyword or `in`

      let visibility = match restriction.kind {
        TokenKind::KwCrate => Ok(Visibility::PublicCrate),
        TokenKind::KwSelf => Ok(Visibility::PublicSelf),
        TokenKind::KwSuper => Ok(Visibility::PublicSuper),

        TokenKind::KwIn => {
          let path = self.parse_path(false, engine)?;
          Ok(Visibility::PublicIn(path))
        },

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
          return Err(());
        },
      };

      self.expect(TokenKind::CloseParen, engine)?;
      visibility
    } else {
      // plain `pub`
      Ok(Visibility::Public)
    }
  }
}
