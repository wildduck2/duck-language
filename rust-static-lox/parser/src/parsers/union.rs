use crate::{
  ast::{
    attrs::Attribute, items::Item, visibility::Visibility, FieldDecl, UnionDecl, VisItem,
    VisItemKind,
  },
  match_and_consume,
  parser_utils::ParserContext,
  Parser,
};
use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_union_decl(
    &mut self,
    attributes: Vec<Attribute>,
    visibility: Visibility,
    context: ParserContext,
  ) -> Result<Item, ()> {
    let mut token = self.current_token();
    if !attributes.is_empty() {
      token.span.merge(self.current_token().span);
    }

    self.expect(TokenKind::KwUnion)?;
    let name = self.parse_name(false)?;
    let generics = self.parse_generic_params(&mut token, context)?;
    let where_clause = self.parse_where_clause(context)?;
    let fields = self.parse_union_fields(context)?;

    Ok(Item::Vis(VisItem {
      attributes,
      visibility,
      kind: VisItemKind::Union(UnionDecl {
        name,
        generics,
        where_clause,
        fields,
      }),
      span: *token.span.merge(self.last_token_span()),
    }))
  }

  fn parse_union_fields(&mut self, context: ParserContext) -> Result<Vec<FieldDecl>, ()> {
    let mut fields = vec![];
    self.expect(TokenKind::LBrace)?;
    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::RBrace) {
      fields.push(self.parse_union_field(context)?);
      match_and_consume!(self, TokenKind::Comma)?;
    }
    self.expect(TokenKind::RBrace)?;
    Ok(fields)
  }

  fn parse_union_field(&mut self, context: ParserContext) -> Result<FieldDecl, ()> {
    let mut token = self.current_token();
    let attributes = self.parse_outer_attributes(context)?;
    let visibility = self.parse_visibility(context)?;
    let name = self.parse_name(false)?;
    self.expect(TokenKind::Colon)?;
    let ty = self.parse_type(context)?;

    Ok(FieldDecl {
      attributes,
      visibility,
      name,
      ty,
      span: *token.span.merge(self.last_token_span()),
    })
  }
}
