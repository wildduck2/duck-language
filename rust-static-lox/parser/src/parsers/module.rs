use crate::{
  ast::{
    Attribute, FnSig, ForeignItem, ForeignModDecl, Item, ModuleBody, ModuleDecl, VisItem,
    VisItemKind, Visibility,
  },
  parser_utils::ParserContext,
  Parser,
};

use lexer::token::{LiteralKind, TokenKind};

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
          items.push(self.parse_item(outer_attributes, visibility)?);
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

  pub(crate) fn parse_foreign_mod_decl(
    &mut self,
    attributes: Vec<Attribute>,
    visibility: Visibility,
    context: ParserContext,
  ) -> Result<Item, ()> {
    let mut token = self.current_token();
    if !attributes.is_empty() {
      token.span.merge(self.current_token().span);
    }

    let (is_unsafe, abi) = self.parse_foreign_mod_flavors()?;

    self.expect(TokenKind::LBrace)?;
    let inner_attributes = self.parse_inner_attributes(context)?;
    let mut items = vec![];
    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::RBrace) {
      items.push(self.parse_foreign_mod_item(context)?);
    }

    self.expect(TokenKind::RBrace)?;

    Ok(Item::Vis(VisItem {
      attributes,
      visibility,
      kind: VisItemKind::ForeignMod(ForeignModDecl {
        is_unsafe,
        abi,
        inner_attributes,
        items,
      }),
      span: *token.span.merge(self.last_token_span()),
    }))
  }

  fn parse_foreign_mod_item(&mut self, context: ParserContext) -> Result<ForeignItem, ()> {
    let mut token = self.current_token();
    let outer_attributes = self.parse_outer_attributes(context)?;
    let visibility = self.parse_visibility(context)?;

    match self.current_token().kind {
      TokenKind::KwFn => {
        self.advance();
        let name = self.parse_name(false)?;
        let generics = self.parse_generic_params(&mut token, context)?;
        let params = self.parse_function_params(false, context)?;
        let return_type = self.parse_return_type(context)?;
        let where_clause = self.parse_where_clause(context)?;
        self.expect(TokenKind::Semi)?;
        Ok(ForeignItem::Function {
          sig: FnSig {
            name,
            generics,
            params,
            return_type,
            where_clause,
          },
          attributes: outer_attributes,
          visibility,
          span: *token.span.merge(self.last_token_span()),
        })
      },

      TokenKind::KwStatic => {
        self.advance();
        let mutability = self.parse_mutability()?;
        let name = self.parse_name(false)?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type(context)?;
        self.expect(TokenKind::Semi)?;
        Ok(ForeignItem::Static {
          attributes: outer_attributes,
          visibility,
          name,
          ty,
          mutability,
          span: *token.span.merge(self.last_token_span()),
        })
      },

      TokenKind::KwType => {
        self.advance();
        let name = self.parse_name(false)?;
        self.expect(TokenKind::Semi)?;
        Ok(ForeignItem::Type {
          attributes: outer_attributes,
          visibility,
          name,
          span: *token.span.merge(self.last_token_span()),
        })
      },

      //FIX:    macro_rules! dummy {
      //        () => {};
      //    }
      TokenKind::Ident => {
        let path = self.parse_path(false, context)?;
        self.expect(TokenKind::Bang)?;
        let macro_invocation = self.parse_macro_invocation(path)?;
        self.expect(TokenKind::Semi)?;
        Ok(ForeignItem::MacroInvocationSemi {
          attributes: outer_attributes,
          invoc: macro_invocation,
          span: *token.span.merge(self.last_token_span()),
        })
      },

      _ => {
        let lexeme = self.get_token_lexeme(&token);
        self.emit(self.err_unexpected_token(token.span, "foreign item", &lexeme));
        Err(())
      },
    }
  }

  pub(crate) fn can_start_foreign_extern_crate(&self) -> bool {
    let mut offset = 0;

    if matches!(self.peek(offset).kind, TokenKind::KwUnsafe) {
      offset += 1;
    }

    if !matches!(self.peek(offset).kind, TokenKind::KwExtern) {
      return false;
    }
    offset += 1;

    if matches!(
      self.peek(offset).kind,
      TokenKind::Literal {
        kind: LiteralKind::Str
      } | TokenKind::Literal {
        kind: LiteralKind::RawCStr { .. }
      }
    ) {
      offset += 1;
    }

    matches!(self.peek(offset).kind, TokenKind::LBrace)
  }

  fn parse_foreign_mod_flavors(&mut self) -> Result<(bool, Option<String>), ()> {
    // foreignModItem -> "unsafe"? "extern" abi? foreignModBody
    let mut is_unsafe = false;
    let mut abi: Option<String> = None;

    if matches!(self.current_token().kind, TokenKind::KwUnsafe) {
      self.advance(); // consume "unsafe"
      is_unsafe = true;
    }

    // "extern" is required for a foreign module
    self.expect(TokenKind::KwExtern /* engine? */)?;

    // abi? is optional: STRING | RAW_STRING (whatever your lexer uses)
    match self.current_token().kind {
      TokenKind::Literal {
        kind: LiteralKind::Str,
      }
      | TokenKind::Literal {
        kind: LiteralKind::RawStr { .. },
      } => {
        let name = self.get_token_lexeme(&self.current_token());
        self.advance(); // consume abi literal
        abi = Some(name);
      },
      _ => {},
    }

    Ok((is_unsafe, abi))
  }
}
