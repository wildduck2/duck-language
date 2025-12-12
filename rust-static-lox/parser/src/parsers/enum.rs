use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
  DiagnosticEngine,
};
use lexer::token::TokenKind;

use crate::{
  ast::{
    r#struct::{FieldDecl, TupleField},
    Attribute, EnumDecl, EnumVariant, EnumVariantKind, Item, Visibility,
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
    engine: &mut DiagnosticEngine,
  ) -> Result<Item, ()> {
    println!("{:#?}", visibility);

    let mut token = self.current_token();
    self.advance(engine); // consume the "enum"

    let name = self.parse_name_identifier(engine)?;
    let generics = self.parse_generic_params(&mut token, engine)?;
    let where_clause = self.parse_where_clause(engine)?;

    let variants = self.parse_enum_variants(engine)?;

    Ok(Item::Enum(EnumDecl {
      attributes,
      visibility,
      name,
      generics,
      variants,
      where_clause,
      span: *token.span.merge(token.span),
    }))
  }

  fn parse_enum_variants(&mut self, engine: &mut DiagnosticEngine) -> Result<Vec<EnumVariant>, ()> {
    let mut variants = vec![];
    self.expect(TokenKind::OpenBrace, engine)?; // consume '{'

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::CloseBrace) {
      variants.push(self.parse_enum_variant(engine)?);
      match_and_consume!(self, engine, TokenKind::Comma)?;
    }

    self.expect(TokenKind::CloseBrace, engine)?; // consume '}'
    Ok(variants)
  }

  fn parse_enum_variant(&mut self, engine: &mut DiagnosticEngine) -> Result<EnumVariant, ()> {
    let mut token = self.current_token();
    let attributes = self.parse_outer_attributes(engine)?;
    let visibility = self.parse_visibility(engine)?;
    let name = self.parse_name_identifier(engine)?;

    let kind = if matches!(self.current_token().kind, TokenKind::OpenBrace) {
      EnumVariantKind::Struct(self.parse_enum_record_fields(engine)?)
    } else if matches!(self.current_token().kind, TokenKind::OpenParen) {
      EnumVariantKind::Tuple(self.parse_enum_tuple_fields(engine)?)
    } else {
      EnumVariantKind::Unit
    };

    let discriminant = if matches!(self.current_token().kind, TokenKind::Eq) {
      self.advance(engine); // consume '='
      Some(self.parse_expression(vec![], ExprContext::Default, engine)?)
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

  fn parse_enum_tuple_fields(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Vec<TupleField>, ()> {
    let mut fields = vec![];
    self.expect(TokenKind::OpenParen, engine)?; // consume '('

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::CloseParen) {
      fields.push(self.parse_enum_tuple_field(engine)?);
      match_and_consume!(self, engine, TokenKind::Comma)?;
    }

    self.expect(TokenKind::CloseParen, engine)?; // consume ')'
    Ok(fields)
  }

  fn parse_enum_tuple_field(&mut self, engine: &mut DiagnosticEngine) -> Result<TupleField, ()> {
    let mut token = self.current_token();
    let attributes = self.parse_attributes(engine)?;
    let visibility = self.parse_visibility(engine)?;
    let ty = self.parse_type(engine)?;

    Ok(TupleField {
      visibility,
      attributes,
      ty,
      span: *token.span.merge(token.span),
    })
  }

  fn parse_enum_record_fields(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Vec<FieldDecl>, ()> {
    let mut fields = vec![];
    self.expect(TokenKind::OpenBrace, engine)?; // consume '{'

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::CloseBrace) {
      fields.push(self.parse_struct_record_field(engine)?);
    }

    self.expect(TokenKind::CloseBrace, engine)?; // consume '}'
    Ok(fields)
  }
}
