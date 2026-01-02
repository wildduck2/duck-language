use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::TokenKind;

use crate::{
  ast::{
    Attribute, Ident, ImplBlock, ImplItem, ImplPolarity, Item, VisItem, VisItemKind, Visibility,
  },
  match_and_consume,
  parser_utils::ParserContext,
  Parser,
};

impl Parser {
  pub(crate) fn parse_impl_decl(
    &mut self,
    attributes: Vec<Attribute>,
    visibility: Visibility,
    context: ParserContext,
  ) -> Result<Item, ()> {
    let mut token = self.current_token();
    if !attributes.is_empty() {
      token.span.merge(self.current_token().span);
    }

    let is_unsafe = match_and_consume!(self, TokenKind::KwUnsafe).unwrap_or_else(|_: ()| false);
    self.expect(TokenKind::KwImpl)?;

    let generics = self.parse_generic_params(&mut token, context)?;

    // Try parsing trait-impl prefix: `const? !? traitPath for ...`
    let saved_pos = self.current;
    let is_const = match_and_consume!(self, TokenKind::KwConst).unwrap_or_else(|_: ()| false);
    let polarity = self.parse_impl_polarity();

    let parsed_path = self.parse_path(false, context);

    let (trait_ref, is_inherit, self_ty, is_const, polarity) = match parsed_path {
      Ok(path) if matches!(self.current_token().kind, TokenKind::KwFor) => {
        self.advance(); // consume "for"
        let self_ty = self.parse_type(context)?;
        (Some(path), false, self_ty, is_const, polarity)
      },

      _ => {
        // Inherent impl: rollback and parse `type ...`
        self.current = saved_pos;
        let self_ty = self.parse_type(context)?;
        (None, true, self_ty, false, ImplPolarity::Positive)
      },
    };

    let where_clause = self.parse_where_clause(context)?;

    let (items, inner_attributes) = if is_inherit {
      self.parse_impl_inherit_body(context)?
    } else {
      self.parse_impl_body(context)?
    };

    Ok(Item::Vis(VisItem {
      attributes,
      visibility,
      kind: VisItemKind::Impl(ImplBlock {
        is_unsafe,
        is_const,
        generics,
        polarity,
        trait_ref,
        self_ty,
        where_clause,
        inner_attributes,
        items,
      }),
      span: *token.span.merge(self.last_token_span()),
    }))
  }

  // Shared "{ innerAttr* items* }" wrapper. Only the item parser differs.
  pub(crate) fn parse_impl_body_common<F>(
    &mut self,
    context: ParserContext,
    mut parse_item: F,
  ) -> Result<(Vec<ImplItem>, Vec<Attribute>), ()>
  where
    F: FnMut(&mut Self, ParserContext) -> Result<ImplItem, ()>,
  {
    self.expect(TokenKind::LBrace)?;
    let inner_attributes = self.parse_inner_attributes(context)?;

    let mut items = vec![];
    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::RBrace) {
      items.push(parse_item(self, context)?);
    }

    self.expect(TokenKind::RBrace)?;
    Ok((items, inner_attributes))
  }

  fn parse_impl_inherit_body(
    &mut self,
    context: ParserContext,
  ) -> Result<(Vec<ImplItem>, Vec<Attribute>), ()> {
    self.parse_impl_body_common(context, |p, ctx| p.parse_impl_body_item(false, ctx))
  }

  pub(crate) fn parse_impl_body(
    &mut self,
    context: ParserContext,
  ) -> Result<(Vec<ImplItem>, Vec<Attribute>), ()> {
    self.parse_impl_body_common(context, |p, ctx| p.parse_impl_body_item(true, ctx))
  }

  fn parse_impl_body_item(
    &mut self,
    is_inherit: bool,
    context: ParserContext,
  ) -> Result<ImplItem, ()> {
    let mut token = self.current_token();
    let outer_attributes = self.parse_outer_attributes(context)?;
    let visibility = self.parse_visibility(context)?;

    match self.current_token().kind {
      // associatedTypeItem
      TokenKind::KwType if !is_inherit => {
        self.advance(); // consume "type"

        let name = self.parse_name(false)?;

        let mut dummy = self.current_token();
        let generics = self.parse_generic_params(&mut dummy, context)?;

        // Optional bounds `: ...` (parse + discard for now; AST doesn't store them).
        let has_bounds = matches!(self.current_token().kind, TokenKind::Colon);
        let bounds = self.parse_trait_bounds("impl associated type", context)?;
        if has_bounds && bounds.is_empty() {
          let token = self.current_token();
          let lexeme = self.get_token_lexeme(&token);
          self.emit(self.err_invalid_trait_bound(token.span, &lexeme));
          return Err(());
        }

        let where_clause = self.parse_where_clause(context)?;

        // Optional default: `= Type`
        // Your AST requires `ty: Type` (not Option), so require `= Type` here.
        if !matches!(self.current_token().kind, TokenKind::Eq) {
          let found = self.get_token_lexeme(&self.current_token());
          self.emit(self.err_unexpected_token(
            self.current_token().span,
            "associated type default `= Type`",
            &found,
          ));
          return Err(());
        }

        self.advance(); // consume '='
        let ty = self.parse_type(context)?;
        self.expect(TokenKind::Semi)?;

        if let k @ Ident::Self_ = name {
          let diagnostic = self
            .diagnostic(DiagnosticError::InvalidNameIdentifier, "invalid name")
            .with_label(
              self.current_token().span,
              Some(format!("You can not use `{}` as a type name", k.as_str()).to_string()),
              LabelStyle::Primary,
            )
            .with_help("use proper identifier names".to_string());
          self.emit(diagnostic);
          return Err(());
        }

        Ok(ImplItem::Type {
          attributes: outer_attributes,
          visibility,
          name,
          generics,
          where_clause,
          ty,
          span: *token.span.merge(self.last_token_span()),
        })
      },

      // associatedConstItem
      TokenKind::KwConst => {
        self.advance(); // consume "const"
        let name = self.parse_name(false)?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type(context)?;

        if !matches!(self.current_token().kind, TokenKind::Eq) {
          let found = self.get_token_lexeme(&self.current_token());
          self.emit(self.err_unexpected_token(self.current_token().span, "`= <expr>`", &found));
          return Err(());
        }

        self.advance(); // consume '='
        let value = self.parse_expression(vec![], context)?;
        self.expect(TokenKind::Semi)?;

        Ok(ImplItem::Const {
          attributes: outer_attributes,
          visibility,
          name,
          ty,
          value,
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
          Ok(ImplItem::Method(func))
        } else {
          let found = self.get_token_lexeme(&self.current_token());
          self.emit(self.err_unexpected_token(self.current_token().span, "impl item", &found));
          Err(())
        }
      },

      // macroInvocationSemi
      TokenKind::Ident => {
        let path = self.parse_path(false, context)?;
        self.expect(TokenKind::Bang)?;
        let mac = self.parse_macro_invocation(path)?;
        self.expect(TokenKind::Semi)?;
        Ok(ImplItem::Macro { mac })
      },

      _ => {
        let found = self.get_token_lexeme(&self.current_token());
        self.emit(self.err_unexpected_token(self.current_token().span, "impl item", &found));
        Err(())
      },
    }
  }

  fn parse_impl_polarity(&mut self) -> ImplPolarity {
    match self.current_token().kind {
      TokenKind::Bang => {
        self.advance(); // consume '!'
        ImplPolarity::Negative
      },
      _ => ImplPolarity::Positive,
    }
  }

  pub(crate) fn can_start_impl(&self) -> bool {
    let mut offset = 0;

    if matches!(self.peek(offset).kind, TokenKind::KwUnsafe) {
      offset += 1;
    }

    matches!(self.peek(offset).kind, TokenKind::KwImpl)
  }
}
