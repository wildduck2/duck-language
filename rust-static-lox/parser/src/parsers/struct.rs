use crate::ast::{r#struct::*, *};
use crate::{match_and_consume, Parser};
use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
  DiagnosticEngine,
};
use lexer::token::TokenKind;

impl Parser {
  //! TODO: missing parts for full Rust struct parsing
  //! - support for generics with where clause before tuple fields: struct A<T> where T: Copy (...)
  //! - support for trailing commas in tuple structs: struct A(u8,);
  //! - support for visibility on tuple and record fields exactly as Rust does
  //! - support for attributes on struct, tuple, and unit variants in all valid positions
  //! - support for parsing arbitrary inner attributes on struct bodies
  //! - support for parsing doc comments as attributes
  //! - ensure recovery for malformed field lists matches Rust behavior
  //! - support for parsing attributes before generics and before where clause
  //! - handle macro invocation forms: struct A { x: i32 } macro_rules!
  //! - add better error recovery when encountering unexpected tokens

  pub(crate) fn parse_struct_decl(
    &mut self,
    attributes: Vec<Attribute>,
    visibility: Visibility,
    engine: &mut DiagnosticEngine,
  ) -> Result<Item, ()> {
    let mut token = self.current_token();
    self.advance(engine); // consume 'struct'

    let name = self.parse_name_identifier(engine)?;
    let generics = self.parse_generic_params(&mut token, engine)?;
    let where_clause = self.parse_where_clause(engine)?;

    if matches!(self.current_token().kind, TokenKind::OpenBrace) {
      // record: optional where BEFORE '{'
      // struct Name<T> where ... { fields }   (no trailing ';')
      let fields = self.parse_record_fields(engine)?;
      token.span.merge(self.current_token().span);
      return Ok(Item::Struct(StructDecl {
        attributes,
        visibility,
        name,
        generics,
        kind: StructKind::Named { fields },
        where_clause,
        span: token.span,
      }));
    } else if matches!(self.current_token().kind, TokenKind::OpenParen) {
      // tuple: fields first, then where, then ';'
      // struct Name<T>(...) where ... ;
      let fields = self.parse_tuple_fields(engine)?;
      let where_clause = self.parse_where_clause(engine)?;
      self.expect(TokenKind::Semi, engine)?; // required
      token.span.merge(self.current_token().span);
      return Ok(Item::Struct(StructDecl {
        attributes,
        visibility,
        name,
        generics,
        kind: StructKind::Tuple(fields),
        where_clause,
        span: token.span,
      }));
    }

    // unit: optional where, then ';'
    // struct Name<T> where ... ;
    self.expect(TokenKind::Semi, engine)?; // required
    token.span.merge(self.current_token().span);
    Ok(Item::Struct(StructDecl {
      attributes,
      visibility,
      name,
      generics,
      kind: StructKind::Unit,
      where_clause,
      span: token.span,
    }))
  }

  pub(crate) fn parse_record_fields(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Vec<FieldDecl>, ()> {
    self.expect(TokenKind::OpenBrace, engine)?; // consume '{'

    let mut fields = vec![];

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::CloseBrace) {
      fields.push(self.parse_record_field(engine)?);
      match_and_consume!(self, engine, TokenKind::Comma)?;
    }

    self.expect(TokenKind::CloseBrace, engine)?; // consume '}'
    Ok(fields)
  }

  pub(crate) fn parse_record_field(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<FieldDecl, ()> {
    let mut token = self.current_token();

    let attributes = if matches!(self.current_token().kind, TokenKind::Pound) {
      self.parse_attributes(engine)?
    } else {
      vec![]
    };

    let visibility = self.parse_visibility(engine)?;
    let name = self.parse_name_identifier(engine)?;

    self.expect(TokenKind::Colon, engine)?; // consume ':'
    let ty = self.parse_type(engine)?;

    Ok(FieldDecl {
      attributes,
      name,
      ty,
      visibility,
      span: *token.span.merge(self.current_token().span),
    })
  }

  /// Parse tuple-style struct fields `( ... )`.
  ///
  /// Grammar:
  ///   tupleStructFields → "(" tupleFields? ")"
  ///   tupleField        → outerAttr* visibility? type
  ///
  /// This function consumes the opening `(` and closing `)` and parses each
  /// field separated by commas. Diagnostics are produced for missing commas or
  /// unexpected tokens.
  ///
  /// Example:
  /// ```rust
  /// struct Color(u8, u8, u8);
  /// ```
  pub(crate) fn parse_tuple_fields(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Vec<TupleField>, ()> {
    let mut fields = vec![];
    self.expect(TokenKind::OpenParen, engine)?; // consume '('

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::CloseParen) {
      fields.push(self.parse_tuple_field(engine)?);
      match_and_consume!(self, engine, TokenKind::Comma)?;
    }

    self.expect(TokenKind::CloseParen, engine)?; // consume ')'
    Ok(fields)
  }

  fn parse_tuple_field(&mut self, engine: &mut DiagnosticEngine) -> Result<TupleField, ()> {
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

  /// Parse a name identifier and return its string value.
  ///
  /// Expects the current token to be an identifier (`TokenKind::Ident`). Emits
  /// a diagnostic and returns `Err(())` if the next token is not a valid name.
  ///
  /// Used for struct names, field names, function names, and similar cases.
  ///
  /// Example:
  /// ```rust
  /// let name = self.parse_name_identifier(engine)?;
  /// ```
  pub(crate) fn parse_name_identifier(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<String, ()> {
    if matches!(self.current_token().kind, TokenKind::Ident) {
      let name = self.get_token_lexeme(&self.current_token());
      self.advance(engine); // consume the identifier
      return Ok(name);
    }

    let lexeme = self.get_token_lexeme(&self.current_token());
    let diagnostic = Diagnostic::new(
      DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
      "Unexpected name identifier".to_string(),
      self.source_file.path.clone(),
    )
    .with_label(
      self.current_token().span,
      Some(format!(
        "Expected a primary expression, found \"{}\"",
        lexeme
      )),
      LabelStyle::Primary,
    )
    .with_help("Expected a valid name identifier".to_string());

    engine.add(diagnostic);

    Err(())
  }
}
