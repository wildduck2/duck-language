use crate::Parser;
use crate::{
  ast::{Attribute, ConstDecl, Ident, Item, VisItem, VisItemKind, Visibility},
  parser_utils::ParserContext,
};
use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_const_decl(
    &mut self,
    attributes: Vec<Attribute>,
    visibility: Visibility,
    context: ParserContext,
  ) -> Result<Item, ()> {
    let mut token = self.current_token();
    if !attributes.is_empty() {
      token.span.merge(self.current_token().span);
    }

    self.expect(TokenKind::KwConst)?;
    let name = Ident::Name(self.parse_name(true)?);
    self.expect(TokenKind::Colon)?;
    let ty = self.parse_type(context)?;
    self.expect(TokenKind::Eq)?;
    let value = self.parse_expression(vec![], context)?;
    self.expect(TokenKind::Semi)?;
    token.span.merge(self.current_token().span);

    Ok(Item::Vis(VisItem {
      attributes,
      visibility,
      kind: VisItemKind::Const(ConstDecl { ty, name, value }),
      span: token.span,
    }))
  }
}
