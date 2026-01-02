use crate::Parser;
use crate::{
  ast::{Attribute, ConstDecl, Item, VisItem, VisItemKind, Visibility},
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
    let name = self.parse_name(true)?;
    self.expect(TokenKind::Colon)?;
    let ty = self.parse_type(context)?;
    self.expect(TokenKind::Eq)?;
    let value = self.parse_expression(vec![], context)?;
    let semi = self.expect(TokenKind::Semi)?;
    token.span.merge(semi.span);

    Ok(Item::Vis(VisItem {
      attributes,
      visibility,
      kind: VisItemKind::Const(ConstDecl { ty, name, value }),
      span: token.span,
    }))
  }

  pub(crate) fn can_start_const_item(&self) -> bool {
    matches!(self.current_token().kind, TokenKind::KwConst)
      && matches!(self.peek(1).kind, TokenKind::Ident)
  }
}
