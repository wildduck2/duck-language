use crate::ast::expr::FieldInit;
use crate::ast::path::Path;
use crate::ast::{
  r#struct::*, Attribute, Expr, ExprKind, FieldName, Item, VisItem, VisItemKind, Visibility,
};
use crate::parser_utils::ExprContext;
use crate::{match_and_consume, Parser};
use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
  DiagnosticEngine,
};
use lexer::token::{LiteralKind, TokenKind};

impl Parser {
  pub(crate) fn parse_struct_decl(
    &mut self,
    attributes: Vec<Attribute>,
    visibility: Visibility,
    engine: &mut DiagnosticEngine,
  ) -> Result<Item, ()> {
    let mut token = self.current_token();
    self.advance(engine); // consume 'struct'

    let name = self.parse_name(false, engine)?;
    let generics = self.parse_generic_params(&mut token, engine)?;
    let where_clause = self.parse_where_clause(engine)?;

    if matches!(self.current_token().kind, TokenKind::OpenBrace) {
      // record: optional where BEFORE '{'
      // struct Name<T> where ... { fields }   (no trailing ';')
      let fields = self.parse_record_fields(engine)?;
      token.span.merge(self.current_token().span);
      return Ok(Item::Vis(VisItem {
        attributes,
        visibility,
        kind: VisItemKind::Struct(StructDecl {
          name,
          generics,
          kind: StructKind::Named { fields },
          where_clause,
        }),
        span: token.span,
      }));
    } else if matches!(self.current_token().kind, TokenKind::OpenParen) {
      // tuple: fields first, then where, then ';'
      // struct Name<T>(...) where ... ;
      let fields = self.parse_tuple_fields(engine)?;
      let where_clause = self.parse_where_clause(engine)?;
      self.expect(TokenKind::Semi, engine)?; // required
      token.span.merge(self.current_token().span);

      return Ok(Item::Vis(VisItem {
        attributes,
        visibility,
        kind: VisItemKind::Struct(StructDecl {
          name,
          generics,
          kind: StructKind::Tuple { fields },
          where_clause,
        }),
        span: token.span,
      }));
    }

    // unit: optional where, then ';'
    // struct Name<T> where ... ;
    self.expect(TokenKind::Semi, engine)?; // required
    token.span.merge(self.current_token().span);
    Ok(Item::Vis(VisItem {
      attributes,
      visibility,
      kind: VisItemKind::Struct(StructDecl {
        name,
        generics,
        kind: StructKind::Unit,
        where_clause,
      }),
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
      self.parse_outer_attributes(engine)?
    } else {
      vec![]
    };

    let visibility = self.parse_visibility(engine)?;
    let name = self.parse_name(false, engine)?;

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
    let attributes = self.parse_outer_attributes(engine)?;
    let visibility = self.parse_visibility(engine)?;
    let ty = self.parse_type(engine)?;

    Ok(TupleField {
      visibility,
      attributes,
      ty,
      span: *token.span.merge(token.span),
    })
  }

  pub(crate) fn parse_name(
    &mut self,
    accept_digit: bool,
    engine: &mut DiagnosticEngine,
  ) -> Result<String, ()> {
    if matches!(self.current_token().kind, TokenKind::Ident)
      || (accept_digit && matches!(self.current_token().kind, TokenKind::Literal { .. }))
    {
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

  pub(crate) fn parse_struct_expr(
    &mut self,
    path: Path,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();

    if matches!(self.current_token().kind, TokenKind::OpenBrace) {
      let (fields, base) = self.parse_struct_record_init_fields(engine)?;
      Ok(Expr {
        attributes: vec![],
        kind: ExprKind::Struct { path, fields, base },
        span: *token.span.merge(self.current_token().span),
      })
    } else {
      let elements = self.parse_struct_tuple_init_fields(engine)?;
      Ok(Expr {
        attributes: vec![],
        kind: ExprKind::TupleStruct { path, elements },
        span: *token.span.merge(self.current_token().span),
      })
    }
  }

  fn parse_struct_record_init_fields(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<(Vec<FieldInit>, Option<Box<Expr>>), ()> {
    self.expect(TokenKind::OpenBrace, engine)?; // consume '{'
    let mut fields = vec![];

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::CloseBrace) {
      if matches!(self.current_token().kind, TokenKind::DotDot) {
        self.advance(engine);
        let base = self.parse_expression(vec![], ExprContext::Struct, engine)?;
        self.expect(TokenKind::CloseBrace, engine)?;
        return Ok((fields, Some(Box::new(base))));
      }

      fields.push(self.parse_struct_record_init_field(engine)?);
      match_and_consume!(self, engine, TokenKind::Comma)?;
    }
    self.expect(TokenKind::CloseBrace, engine)?; // consume '}'
    Ok((fields, None))
  }

  fn parse_struct_tuple_init_fields(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Vec<Expr>, ()> {
    let mut elements = vec![];
    self.expect(TokenKind::OpenParen, engine)?; // consume '('

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::CloseParen) {
      elements.push(self.parse_struct_tuple_init_field(engine)?);
      match_and_consume!(self, engine, TokenKind::Comma)?;
    }
    self.expect(TokenKind::CloseParen, engine)?; // consume ')'
    Ok(elements)
  }

  fn parse_struct_tuple_init_field(&mut self, engine: &mut DiagnosticEngine) -> Result<Expr, ()> {
    self.parse_expression(vec![], ExprContext::Struct, engine)
  }

  fn parse_struct_record_init_field(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<FieldInit, ()> {
    let mut token = self.current_token();

    let attributes = if matches!(self.current_token().kind, TokenKind::Pound) {
      self.parse_outer_attributes(engine)?
    } else {
      vec![]
    };

    // parse field name
    let name = match self.current_token().kind {
      TokenKind::Ident => {
        let ident = self.get_token_lexeme(&self.current_token());
        self.advance(engine);
        FieldName::Ident(ident)
      },

      TokenKind::Literal {
        kind: LiteralKind::Integer { .. },
      } => {
        let index = self
          .get_token_lexeme(&self.current_token())
          .parse::<usize>()
          .unwrap();
        self.advance(engine);
        FieldName::TupleIndex(index)
      },

      _ => {
        // TODO: emit diagnostic for invalid struct field name
        return Err(());
      },
    };

    let value = if matches!(self.current_token().kind, TokenKind::Colon) {
      self.advance(engine); // consume ':'
      Some(self.parse_expression(vec![], ExprContext::Struct, engine)?)
    } else {
      // shorthand field init
      Some(Expr {
        attributes: vec![],
        kind: ExprKind::Path {
          qself: None,
          path: Path::from_ident(match &name {
            FieldName::Ident(s) => s.clone(),
            FieldName::TupleIndex(_) => unreachable!(),
          }),
        },
        span: token.span,
      })
    };

    Ok(FieldInit {
      attributes,
      name,
      value,
      span: *token.span.merge(self.current_token().span),
    })
  }
}
