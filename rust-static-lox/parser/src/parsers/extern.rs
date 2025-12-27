use crate::{
  ast::{Attribute, ExternCrateDecl, Ident, Item, VisItem, VisItemKind, Visibility},
  parser_utils::ParserContext,
  Parser,
};

use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_extern_crate_decl(
    &mut self,
    attributes: Vec<Attribute>,
    visibility: Visibility,
    context: ParserContext,
  ) -> Result<Item, ()> {
    let mut token = self.current_token();
    if !attributes.is_empty() {
      token.span.merge(self.current_token().span);
    }

    self.expect(TokenKind::KwExtern)?;
    self.expect(TokenKind::KwCrate)?;

    let name = Ident::Name(self.parse_name(true)?);
    let alias = if self.current_token().kind == TokenKind::KwAs {
      self.advance();
      if self.get_token_lexeme(&self.current_token()) == "_" {
        self.advance();
        Some(Ident::Underscore)
      } else {
        Some(Ident::Name(self.parse_name(true)?))
      }
    } else {
      None
    };
    let semi = self.expect(TokenKind::Semi)?;

    Ok(Item::Vis(VisItem {
      attributes,
      visibility,
      kind: VisItemKind::ExternCrate(ExternCrateDecl { name, alias }),
      span: *token.span.merge(semi.span),
    }))
  }
}
