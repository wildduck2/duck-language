use lexer::token::TokenKind;

use crate::{
  ast::{Attribute, ExternTypeDecl, Item, VisItem, VisItemKind, Visibility},
  Parser,
};

impl Parser {
  pub(crate) fn parse_extern_type_decl(
    &mut self,
    attributes: Vec<Attribute>,
    visibility: Visibility,
  ) -> Result<Item, ()> {
    let mut token = self.current_token();
    if !attributes.is_empty() {
      token.span.merge(self.current_token().span);
    }

    self.expect(TokenKind::KwExtern)?;
    self.expect(TokenKind::KwType)?;

    let name = self.parse_name(false)?;
    self.expect(TokenKind::Semi)?;

    Ok(Item::Vis(VisItem {
      attributes,
      visibility,
      kind: VisItemKind::ExternType(ExternTypeDecl { name }),
      span: *token.span.merge(self.last_token_span()),
    }))
  }
}
