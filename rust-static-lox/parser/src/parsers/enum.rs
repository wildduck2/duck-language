use lexer::token::TokenKind;

use crate::{
  ast::{
    Attribute, EnumDecl, EnumVariant, EnumVariantKind, Item, VisItem, VisItemKind, Visibility,
  },
  match_and_consume,
  parser_utils::ExprContext,
  Parser,
};

impl Parser {
  pub(crate) fn parse_enum_decl(
    &mut self,
    attributes: Vec<Attribute>,
    visibility: Visibility,
  ) -> Result<Item, ()> {
    println!("{:#?}", visibility);

    let mut token = self.current_token();
    self.advance(); // consume the "enum"

    let name = self.parse_name(false)?;
    let generics = self.parse_generic_params(&mut token)?;
    let where_clause = self.parse_where_clause()?;

    let variants = self.parse_enum_variants()?;

    Ok(Item::Vis(VisItem {
      attributes,
      visibility,
      kind: VisItemKind::Enum(EnumDecl {
        name,
        generics,
        variants,
        where_clause,
      }),
      span: *token.span.merge(token.span),
    }))
  }

  fn parse_enum_variants(&mut self) -> Result<Vec<EnumVariant>, ()> {
    let mut variants = vec![];
    self.expect(TokenKind::OpenBrace)?; // consume '{'

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::CloseBrace) {
      variants.push(self.parse_enum_variant()?);
      match_and_consume!(self, TokenKind::Comma)?;
    }

    self.expect(TokenKind::CloseBrace)?; // consume '}'
    Ok(variants)
  }

  fn parse_enum_variant(&mut self) -> Result<EnumVariant, ()> {
    let mut token = self.current_token();
    let attributes = self.parse_outer_attributes()?;
    let visibility = self.parse_visibility()?;
    let name = self.parse_name(false)?;

    let kind = if matches!(self.current_token().kind, TokenKind::OpenBrace) {
      EnumVariantKind::Struct {
        fields: self.parse_record_fields()?,
      }
    } else if matches!(self.current_token().kind, TokenKind::OpenParen) {
      EnumVariantKind::Tuple {
        fields: self.parse_tuple_fields()?,
      }
    } else {
      EnumVariantKind::Unit
    };

    let discriminant = if matches!(self.current_token().kind, TokenKind::Eq) {
      self.advance(); // consume '='
      Some(self.parse_expression(vec![], ExprContext::Default)?)
    } else {
      None
    };

    Ok(EnumVariant {
      attributes,
      visibility,
      name,
      kind,
      discriminant,
      span: *token.span.merge(token.span),
    })
  }
}
