use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::TokenKind;

use crate::{
  ast::{Attribute, Item, TraitDecl, TraitItem, VisItem, VisItemKind},
  match_and_consume,
  parser_utils::ParserContext,
  Parser,
};

impl Parser {
  pub(crate) fn parse_trait_decl(
    &mut self,
    attributes: Vec<crate::ast::Attribute>,
    visibility: crate::ast::Visibility,
    context: ParserContext,
  ) -> Result<Item, ()> {
    let mut token = self.current_token();

    let is_unsafe = match_and_consume!(self, TokenKind::KwUnsafe)?;
    let is_auto = match_and_consume!(self, TokenKind::KwAuto)?;
    self.expect(TokenKind::KwTrait)?;

    let name = self.parse_name(false)?;
    let generics = self.parse_generic_params(&mut token, context)?;
    let supertraits = self.parse_trait_bounds("trait declaration", context)?;
    let where_clause = self.parse_where_clause(context)?;

    let (items, inner_attributes) = self.parse_trait_body(context)?;

    Ok(Item::Vis(VisItem {
      attributes,
      visibility,
      kind: VisItemKind::Trait(TraitDecl {
        is_auto,
        is_unsafe,
        name,
        generics,
        supertraits,
        where_clause,
        inner_attributes,
        items,
      }),
      span: *token.span.merge(self.last_token_span()),
    }))
  }

  fn parse_trait_body(
    &mut self,
    context: ParserContext,
  ) -> Result<(Vec<TraitItem>, Vec<Attribute>), ()> {
    let mut items = vec![];
    self.expect(TokenKind::LBrace)?;
    let inner_attributes = self.parse_inner_attributes(context)?;

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::RBrace) {
      items.push(self.parse_trait_body_item(context)?);
    }

    self.expect(TokenKind::RBrace)?;

    Ok((items, inner_attributes))
  }

  fn parse_trait_body_item(&mut self, context: ParserContext) -> Result<TraitItem, ()> {
    let mut token = self.current_token();
    let outer_attributes = self.parse_outer_attributes(context)?;
    let visibility = self.parse_visibility(context)?;

    match self.current_token().kind {
      // associatedTypeItem
      TokenKind::KwType => {
        self.advance(); // consume "type"
        let name = self.parse_name(false)?;
        let generics = self.parse_generic_params(&mut self.current_token(), context)?;

        let bounds = if matches!(self.current_token().kind, TokenKind::Colon) {
          self.parse_trait_bounds("trait item", context)?
        } else {
          vec![]
        };

        let where_clause = self.parse_where_clause(context)?;
        let default = if match_and_consume!(self, TokenKind::Eq)? {
          Some(self.parse_type(context)?)
        } else {
          None
        };

        self.expect(TokenKind::Semi)?;

        Ok(TraitItem::Type {
          attributes: outer_attributes,
          name,
          generics,
          where_clause,
          bounds,
          default,
          span: *token.span.merge(self.last_token_span()),
        })
      },

      // associatedConstItem or const fn
      TokenKind::KwConst => {
        if self.can_start_fun() {
          let item = self.parse_fn_decl(outer_attributes, visibility, context)?;

          if let Item::Vis(VisItem {
            kind: VisItemKind::Function(func),
            ..
          }) = item
          {
            return Ok(TraitItem::Method(func));
          }

          let found = self.get_token_lexeme(&self.current_token());
          let diagnostic = self
            .diagnostic(
              DiagnosticError::UnexpectedToken,
              format!("expected `trait item`, found `{found}`"),
            )
            .with_label(
              self.current_token().span,
              Some("expected `trait item` here".to_string()),
              LabelStyle::Primary,
            )
            .with_note(format!("unexpected token: `{found}`"))
            .with_help("add a valid trait item like `type`, `const`, or `fn`".to_string());
          self.emit(diagnostic);
          return Err(());
        }

        self.advance(); // consume "const"
        let name = self.parse_name(false)?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type(context)?;

        let default = if match_and_consume!(self, TokenKind::Eq)? {
          Some(self.parse_expression(vec![], context)?)
        } else {
          None
        };
        self.expect(TokenKind::Semi)?;

        Ok(TraitItem::Const {
          attributes: outer_attributes,
          name,
          ty,
          default,
          span: *token.span.merge(self.last_token_span()),
        })
      },

      // associatedFunctionItem
      TokenKind::KwFn => {
        let item = self.parse_fn_decl(outer_attributes, visibility, context)?;

        if let Item::Vis(VisItem {
          kind: VisItemKind::Function(func),
          ..
        }) = item
        {
          Ok(TraitItem::Method(func))
        } else {
          let found = self.get_token_lexeme(&self.current_token());
          let diagnostic = self
            .diagnostic(
              DiagnosticError::UnexpectedToken,
              format!("expected `trait item`, found `{found}`"),
            )
            .with_label(
              self.current_token().span,
              Some("expected `trait item` here".to_string()),
              LabelStyle::Primary,
            )
            .with_note(format!("unexpected token: `{found}`"))
            .with_help("add a valid trait item like `type`, `const`, or `fn`".to_string());
          self.emit(diagnostic);
          Err(())
        }
      },

      // macroInvocationSemi
      TokenKind::Ident => {
        let path = self.parse_path(false, context)?;
        self.expect(TokenKind::Bang)?;
        let mac = self.parse_macro_invocation(path)?;
        self.expect(TokenKind::Semi)?;
        Ok(TraitItem::Macro { mac })
      },

      _ => {
        let found = self.get_token_lexeme(&self.current_token());
        let diagnostic = self
          .diagnostic(
            DiagnosticError::UnexpectedToken,
            format!("expected `trait item`, found `{found}`"),
          )
          .with_label(
            self.current_token().span,
            Some("expected `trait item` here".to_string()),
            LabelStyle::Primary,
          )
          .with_note(format!("unexpected token: `{found}`"))
          .with_help("add a valid trait item like `type`, `const`, or `fn`".to_string());
        self.emit(diagnostic);
        Err(())
      },
    }
  }
  pub(crate) fn can_start_trait(&self) -> bool {
    let mut offset = 0;

    // optional: unsafe
    if matches!(self.peek(offset).kind, TokenKind::KwUnsafe) {
      offset += 1;
    }

    // optional: auto
    if matches!(self.peek(offset).kind, TokenKind::KwAuto) {
      offset += 1;
    }

    // required: trait
    matches!(self.peek(offset).kind, TokenKind::KwTrait)
  }
}
