use crate::{
  ast::{Attribute, Ident, Item, ModuleBody, ModuleDecl, VisItem, VisItemKind, Visibility},
  parser_utils::ParserContext,
  Parser,
};

use lexer::token::{Token, TokenKind};

impl Parser {
  pub(crate) fn parse_module_decl(
    &mut self,
    attributes: Vec<Attribute>,
    visibility: Visibility,
    context: ParserContext,
  ) -> Result<Item, ()> {
    let mut token = self.current_token();
    if !attributes.is_empty() {
      token.span.merge(self.current_token().span);
    }

    self.expect(TokenKind::KwMod)?;

    let name = self.parse_name(true)?;
    let body = self.parse_module_body(context)?;

    Ok(Item::Vis(VisItem {
      attributes,
      visibility,
      kind: VisItemKind::Module(ModuleDecl { name, body }),
      span: *token.span.merge(self.last_token_span()),
    }))
  }

  fn parse_module_body(&mut self, context: ParserContext) -> Result<Option<ModuleBody>, ()> {
    match self.current_token().kind {
      TokenKind::Semi => {
        self.advance();
        Ok(None)
      },
      TokenKind::LBrace => {
        self.advance();
        let mut inner_token = self.current_token();
        let mut items = vec![];
        while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::RBrace) {
          let outer_attributes = self.parse_outer_attributes(context)?;
          let visibility = self.parse_visibility(context)?;
          items.push(self.parse_item(outer_attributes, visibility, context)?);
        }
        let close = self.expect(TokenKind::RBrace)?;
        inner_token.span.merge(close.span);
        Ok(Some(ModuleBody {
          inner_attributes: vec![],
          items,
          span: inner_token.span,
        }))
      },
      _ => {
        let found = self.get_token_lexeme(&self.current_token());
        self.emit(self.err_unexpected_token(self.current_token().span, "module body", &found));
        Err(())
      },
    }
  }
}
