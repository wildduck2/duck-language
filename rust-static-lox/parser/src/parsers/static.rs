use crate::{
  ast::{attrs::Attribute, items::Item, visibility::Visibility, StaticDecl, VisItem, VisItemKind},
  parser_utils::ParserContext,
  Parser,
};
use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_static_decl(
    &mut self,
    attributes: Vec<Attribute>,
    visibility: Visibility,
    context: ParserContext,
  ) -> Result<Item, ()> {
    let mut token = self.current_token();
    self.advance(); // consume the "static"

    if matches!(self.current_token().kind, TokenKind::KwConst) {
      let diagnostic = self
        .diagnostic(
          DiagnosticError::InvalidConstKeyword,
          "invalid const keyword",
        )
        .with_label(
          self.current_token().span,
          Some("const keyword is only valid on static items".to_string()),
          LabelStyle::Primary,
        )
        .with_help("use `static mut` instead".to_string());
      self.emit(diagnostic);
      return Err(());
    }

    let mutability = self.parse_mutability()?;
    let name = self.parse_name(false)?;
    self.expect(TokenKind::Colon)?;
    let ty = self.parse_type(context)?;
    let value = if matches!(self.current_token().kind, TokenKind::Eq) {
      self.advance(); // consume '='
      Some(self.parse_expression(vec![], ParserContext::Default)?)
    } else {
      None
    };
    self.expect(TokenKind::Semi)?;

    Ok(Item::Vis(VisItem {
      attributes,
      visibility,
      kind: VisItemKind::Static(StaticDecl {
        name,
        ty,
        mutability,
        value,
      }),
      span: *token.span.merge(self.last_token_span()),
    }))
  }
}
