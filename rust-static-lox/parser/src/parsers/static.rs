use crate::{
  ast::{attrs::Attribute, items::Item, visibility::Visibility},
  parser_utils::ParserContext,
  Parser,
};
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

    let mutability = self.parse_mutability()?;
    let name = self.parse_name(false)?;
    let ty = self.parse_type(context)?;
    let value = if matches!(self.current_token().kind, TokenKind::Eq) {
      self.advance(); // consume '='
      Some(self.parse_expression(vec![], ParserContext::Default)?)
    } else {
      None
    };

    Err(())

    // Ok(Item::Vis(VisItem {
    //   attributes,
    //   visibility,
    //   kind: VisItemKind::Static(StaticDecl {
    //     name,
    //     ty,
    //     mutability,
    //     value,
    //   }),
    //   span: *token.span.merge(self.current_token().span),
    // }))
  }
}
