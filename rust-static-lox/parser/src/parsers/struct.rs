use crate::ast::expr::FieldInit;
use crate::ast::path::Path;
use crate::ast::{
  r#struct::*, Attribute, Expr, ExprKind, FieldName, Item, VisItem, VisItemKind, Visibility,
};
use crate::parser_utils::ExprContext;
use crate::Parser;
use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::{LiteralKind, TokenKind};

impl Parser {
  pub(crate) fn parse_struct_decl(
    &mut self,
    attributes: Vec<Attribute>,
    visibility: Visibility,
  ) -> Result<Item, ()> {
    let mut token = self.current_token();
    self.advance(); // consume 'struct'

    let name = self.parse_name(false)?;
    let generics = self.parse_generic_params(&mut token)?;
    let where_clause = self.parse_where_clause()?;

    if matches!(self.current_token().kind, TokenKind::LBrace) {
      // record: optional where BEFORE '{'
      // struct Name<T> where ... { fields }   (no trailing ';')
      let fields = self.parse_record_fields()?;
      return Ok(Item::Vis(VisItem {
        attributes,
        visibility,
        kind: VisItemKind::Struct(StructDecl {
          name,
          generics,
          kind: StructKind::Named { fields },
          where_clause,
        }),
        span: *token.span.merge(self.current_token().span),
      }));
    } else if matches!(self.current_token().kind, TokenKind::LParen) {
      // tuple: fields first, then where, then ';'
      // struct Name<T>(...) where ... ;
      let fields = self.parse_tuple_fields()?;
      let where_clause = self.parse_where_clause()?;
      self.expect(TokenKind::Semi)?; // required
      return Ok(Item::Vis(VisItem {
        attributes,
        visibility,
        kind: VisItemKind::Struct(StructDecl {
          name,
          generics,
          kind: StructKind::Tuple { fields },
          where_clause,
        }),
        span: *token.span.merge(self.current_token().span),
      }));
    }

    // unit: optional where, then ';'
    // struct Name<T> where ... ;
    self.expect(TokenKind::Semi)?; // required
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

  pub(crate) fn parse_record_fields(&mut self) -> Result<Vec<FieldDecl>, ()> {
    self.expect(TokenKind::LBrace)?; // consume '{'
    let mut fields = vec![];

    loop {
      match self.current_token().kind {
        TokenKind::RBrace => {
          self.advance(); // consume '}'
          break;
        },
        _ if self.is_eof() => {
          self.expect(TokenKind::RBrace)?;
          break;
        },
        _ => {
          fields.push(self.parse_record_field()?);
          if matches!(self.current_token().kind, TokenKind::RBrace) {
            self.advance();
            break;
          }
          self.expect(TokenKind::Comma)?;
        },
      }
    }
    Ok(fields)
  }

  pub(crate) fn parse_record_field(&mut self) -> Result<FieldDecl, ()> {
    let mut token = self.current_token();

    let attributes = if matches!(self.current_token().kind, TokenKind::Pound) {
      self.parse_outer_attributes()?
    } else {
      vec![]
    };

    let visibility = self.parse_visibility()?;
    let name = self.parse_name(false)?;

    self.expect(TokenKind::Colon)?; // consume ':'
    let ty = self.parse_type()?;

    Ok(FieldDecl {
      attributes,
      name,
      ty,
      visibility,
      span: *token.span.merge(self.current_token().span),
    })
  }

  pub(crate) fn parse_tuple_fields(&mut self) -> Result<Vec<TupleField>, ()> {
    let mut fields = vec![];
    self.expect(TokenKind::LParen)?; // consume '('

    loop {
      match self.current_token().kind {
        TokenKind::RParen => {
          self.advance();
          break;
        },
        _ if self.is_eof() => {
          self.expect(TokenKind::RParen)?;
          break;
        },
        _ => {
          fields.push(self.parse_tuple_field()?);
          if matches!(self.current_token().kind, TokenKind::RParen) {
            self.advance();
            break;
          }
          self.expect(TokenKind::Comma)?;
        },
      }
    }
    Ok(fields)
  }

  fn parse_tuple_field(&mut self) -> Result<TupleField, ()> {
    let mut token = self.current_token();
    let attributes = self.parse_outer_attributes()?;
    let visibility = self.parse_visibility()?;
    let ty = self.parse_type()?;

    Ok(TupleField {
      visibility,
      attributes,
      ty,
      span: *token.span.merge(token.span),
    })
  }

  pub(crate) fn parse_name(&mut self, accept_digit: bool) -> Result<String, ()> {
    if matches!(self.current_token().kind, TokenKind::Ident)
      || (accept_digit && matches!(self.current_token().kind, TokenKind::Literal { .. }))
    {
      let name = self.get_token_lexeme(&self.current_token());
      self.advance(); // consume the identifier
      return Ok(name);
    }

    let lexeme = self.get_token_lexeme(&self.current_token());
    let diagnostic = self
      .diagnostic(
        DiagnosticError::UnexpectedToken,
        "unexpected name identifier",
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

    self.emit(diagnostic);

    Err(())
  }

  pub(crate) fn parse_struct_expr(&mut self, path: Path) -> Result<Expr, ()> {
    let mut token = self.current_token();
    let (fields, base) = self.parse_struct_record_init_fields()?;
    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Struct { path, fields, base },
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_struct_record_init_fields(&mut self) -> Result<(Vec<FieldInit>, Option<Box<Expr>>), ()> {
    self.expect(TokenKind::LBrace)?; // consume '{'
    let mut fields = vec![];
    let mut base = None;

    loop {
      match self.current_token().kind {
        TokenKind::RBrace => {
          self.advance();
          break;
        },
        TokenKind::DotDot => {
          self.advance();
          let base_expr = self.parse_expression(vec![], ExprContext::Struct)?;
          self.expect(TokenKind::RBrace)?;
          base = Some(Box::new(base_expr));
          break;
        },
        _ if self.is_eof() => {
          self.expect(TokenKind::RBrace)?;
          break;
        },
        _ => {
          fields.push(self.parse_struct_record_init_field()?);
          if matches!(self.current_token().kind, TokenKind::RBrace) {
            self.advance();
            break;
          }
          self.expect(TokenKind::Comma)?;
        },
      }
    }
    Ok((fields, base))
  }

  fn parse_struct_record_init_field(&mut self) -> Result<FieldInit, ()> {
    let mut token = self.current_token();

    let attributes = if matches!(self.current_token().kind, TokenKind::Pound) {
      self.parse_outer_attributes()?
    } else {
      vec![]
    };

    // parse field name
    let name = match self.current_token().kind {
      TokenKind::Ident => {
        let ident = self.get_token_lexeme(&self.current_token());
        self.advance();
        FieldName::Ident(ident)
      },

      TokenKind::Literal {
        kind: LiteralKind::Integer { .. },
      } => {
        let index = self
          .get_token_lexeme(&self.current_token())
          .parse::<usize>()
          .unwrap();
        self.advance();
        FieldName::TupleIndex(index)
      },

      _ => {
        let found = self.get_token_lexeme(&self.current_token());
        let diagnostic = self
          .diagnostic(
            DiagnosticError::UnexpectedToken,
            "invalid struct field name",
          )
          .with_label(
            self.current_token().span,
            Some(format!(
              "expected an identifier or tuple index, found `{found}`"
            )),
            LabelStyle::Primary,
          )
          .with_help("struct fields must be identifiers or numeric tuple indexes".to_string());
        self.emit(diagnostic);
        return Err(());
      },
    };

    let value = match self.current_token().kind {
      TokenKind::Colon => {
        self.advance(); // consume ':'
        Some(self.parse_expression(vec![], ExprContext::Struct)?)
      },
      TokenKind::Comma | TokenKind::RBrace => Some(Expr {
        attributes: vec![],
        kind: ExprKind::Path {
          qself: None,
          path: Path::from_ident(match &name {
            FieldName::Ident(s) => s.clone(),
            FieldName::TupleIndex(_) => {
              let lexeme = self.get_token_lexeme(&token);
              let diagnostic = self
                .diagnostic(
                  DiagnosticError::UnexpectedToken,
                  "invalid struct field name",
                )
                .with_label(
                  token.span,
                  Some(format!(
                    "expected an identifier or tuple index, found `{lexeme}`"
                  )),
                  LabelStyle::Primary,
                )
                .with_help(
                  "struct fields must be identifiers or numeric tuple indexes".to_string(),
                );
              self.emit(diagnostic);
              return Err(());
            },
          }),
        },
        span: token.span,
      }),
      _ => None,
    };

    Ok(FieldInit {
      attributes,
      name,
      value,
      span: *token.span.merge(self.current_token().span),
    })
  }
}
