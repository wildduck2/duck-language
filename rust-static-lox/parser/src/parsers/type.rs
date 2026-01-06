use crate::{
  ast::{
    Attribute, Item, Mutability, QSelf, Safety, Type, TypeAliasDecl, VisItem, VisItemKind,
    Visibility,
  },
  match_and_consume,
  parser_utils::ParserContext,
  Parser,
};
use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_type_alias_decl(
    &mut self,
    attributes: Vec<Attribute>,
    visibility: Visibility,
    context: ParserContext,
  ) -> Result<Item, ()> {
    let mut token = self.current_token();
    self.expect(TokenKind::KwType)?; // consume the "type"
    let name = self.parse_name(false)?;
    let generics = self.parse_generic_params(&mut token, context)?;
    let bounds = self.parse_trait_bounds("type alias", context)?;
    let where_clause = self.parse_where_clause(context)?;
    self.expect(TokenKind::Eq)?; // consume '='
    let ty = self.parse_type(context)?;
    self.expect(TokenKind::Semi)?;

    Ok(Item::Vis(VisItem {
      attributes,
      visibility,
      kind: VisItemKind::TypeAlias(TypeAliasDecl {
        name,
        generics,
        bounds,
        where_clause,
        ty,
      }),
      span: *token.span.merge(self.last_token_span()),
    }))
  }

  pub(crate) fn parse_type(&mut self, context: ParserContext) -> Result<Type, ()> {
    let mut token = self.current_token();
    let lexeme = self.get_token_lexeme(&token);
    self.advance(); // consume the first token of the type

    match token.kind {
      TokenKind::Lt => self.parse_qpath_type(context),
      TokenKind::KwImpl => self.parse_impl_trait_type(context),
      TokenKind::KwDyn => self.parse_trait_object_type(context),

      TokenKind::KwFn | TokenKind::KwFor | TokenKind::KwUnsafe => {
        self.parse_bare_function_type(context)
      },

      TokenKind::Bang => Ok(Type::Never),

      // Primitive names and user defined paths
      TokenKind::KwSelfType if matches!(self.peek(0).kind, TokenKind::ColonColon) => {
        // Reset position so parse_path can consume the ident or crate token
        self.current -= 1;
        Ok(Type::Path(self.parse_path(true, ParserContext::Type)?))
      },

      TokenKind::Ident
      | TokenKind::KwSuper
      | TokenKind::ColonColon
      | TokenKind::Dollar
      | TokenKind::KwCrate => {
        match lexeme.as_str() {
          "u8" => Ok(Type::U8),
          "u16" => Ok(Type::U16),
          "u32" => Ok(Type::U32),
          "u64" => Ok(Type::U64),
          "u128" => Ok(Type::U128),
          "usize" => Ok(Type::Usize),

          "i8" => Ok(Type::I8),
          "i16" => Ok(Type::I16),
          "i32" => Ok(Type::I32),
          "i64" => Ok(Type::I64),
          "i128" => Ok(Type::I128),
          "isize" => Ok(Type::Isize),

          "f32" => Ok(Type::F32),
          "f64" => Ok(Type::F64),
          // "f128" => Ok(Type::F128),
          "char" => Ok(Type::Char),
          "str" => Ok(Type::Str),

          "bool" => Ok(Type::Bool),
          "_" => Ok(Type::Infer),

          // Fallback: treat as a path type, possibly with generic arguments
          _ => {
            // Reset position so parse_path can consume the ident or crate token
            self.current -= 1;
            Ok(Type::Path(self.parse_path(true, ParserContext::Type)?))
          },
        }
      },

      // Tuple and parenthesized types: (T, U, V)
      TokenKind::LParen => self.parse_tuple_type(context),

      // Array type: [ T ; expr ]
      TokenKind::LBracket => self.parse_array_type(context),

      // Raw pointer: *const T or *mut T
      TokenKind::Star => self.parse_raw_pointer_type(context),

      // Reference types: &T, &'a T, &mut T, &'a mut T
      TokenKind::Amp => self.parse_reference_type(context),

      TokenKind::KwSelfType => Ok(Type::SelfType),

      // Reject bare mut or const at the start of a type
      _ if matches!(token.kind, TokenKind::KwMut | TokenKind::KwConst) => {
        token.span.merge(self.current_token().span);
        let lexeme = self.get_token_lexeme(&token);
        let diagnostic = self
          .diagnostic(
            DiagnosticError::InvalidMutabilityInField,
            format!("`{lexeme}` cannot be used in this position"),
          )
          .with_label(
            token.span,
            Some(format!("`{lexeme}` is not allowed before a bare type")),
            LabelStyle::Primary,
          )
          .with_note(
            "`mut` and `const` cannot modify field or type declarations directly".to_string(),
          )
          .with_help(
            "use `&mut T` or `*mut T` for mutable references or pointers, or make the binding itself mutable"
              .to_string(),
          );
        self.emit(diagnostic);
        Err(())
      },

      // Fallback for unknown type starts
      _ => {
        token.span.merge(self.current_token().span);
        let lexeme = self.get_token_lexeme(&token);
        let diagnostic = self
          .diagnostic(
            DiagnosticError::InvalidType,
            format!("expected type, found `{lexeme}`"),
          )
          .with_label(
            token.span,
            Some(format!("not a type: `{lexeme}`")),
            LabelStyle::Primary,
          )
          .with_help(
            "if this is a type name, ensure it is declared or imported into scope".to_string(),
          );
        self.emit(diagnostic);
        Err(())
      },
    }
  }
  fn parse_trait_object_type(&mut self, context: ParserContext) -> Result<Type, ()> {
    let bounds = self.parse_trait_bound_vec("trait object type", context)?;
    Ok(Type::TraitObject {
      bounds,
      lifetime: None,
      is_dyn: true,
    })
  }

  fn parse_impl_trait_type(&mut self, context: ParserContext) -> Result<Type, ()> {
    let bounds = self.parse_trait_bound_vec("impl trait type", context)?;
    Ok(Type::ImplTrait(bounds))
  }

  fn parse_reference_type(&mut self, context: ParserContext) -> Result<Type, ()> {
    // Forbid patterns like &const T which are not valid reference types.
    if matches!(self.current_token().kind, TokenKind::KwConst) {
      let diagnostic = self
        .diagnostic(
          DiagnosticError::InvalidMutabilityInField,
          "invalid const specifier in reference type",
        )
        .with_label(
          self.current_token().span,
          Some("const is not allowed after & in a reference type.".to_string()),
          LabelStyle::Primary,
        )
        .with_note(
          "const does not apply to references. Only raw pointers support const qualifiers."
            .to_string(),
        )
        .with_help(
          "Use *const T for a raw const pointer, or &T for an immutable reference.".to_string(),
        );

      self.emit(diagnostic);
      return Err(());
    }

    // Special case: &*const T or &*mut T
    if matches!(self.current_token().kind, TokenKind::Star) {
      return Ok(Type::Reference {
        lifetime: None,
        mutability: Mutability::Immutable,
        inner: Box::new(self.parse_type(context)?),
      });
    }

    // Optional lifetime right after &
    let lifetime = self.parse_type_lifetime()?;
    // Optional mut after lifetime: &mut T or &'a mut T
    let mutability = self.parse_mutability()?;

    Ok(Type::Reference {
      lifetime,
      mutability,
      inner: Box::new(self.parse_type(context)?),
    })
  }

  fn parse_raw_pointer_type(&mut self, context: ParserContext) -> Result<Type, ()> {
    // If we see *T directly, this is missing the const or mut qualifier.
    if matches!(self.current_token().kind, TokenKind::Ident) {
      let diagnostic = self
        .diagnostic(
          DiagnosticError::InvalidPointerType,
          "missing mutability qualifier for raw pointer type",
        )
        .with_label(
          self.current_token().span,
          Some("expected const or mut after *.".to_string()),
          LabelStyle::Primary,
        )
        .with_note(
          "Raw pointers in Rust must explicitly specify mutability, either *const T or *mut T."
            .to_string(),
        )
        .with_help(
          "Use *const T for an immutable raw pointer, or *mut T for a mutable one.".to_string(),
        );

      self.emit(diagnostic);
      return Err(());
    }

    // Reuse Mutability for pointer mutability:
    // *const T  maps to Mutability::Immutable
    // *mut T    maps to Mutability::Mutable
    let mutability = self.parse_mutability()?;

    Ok(Type::RawPointer {
      mutability,
      inner: Box::new(self.parse_type(context)?),
    })
  }

  fn parse_array_type(&mut self, context: ParserContext) -> Result<Type, ()> {
    let element = self.parse_type(context)?;
    if !matches!(self.current_token().kind, TokenKind::Semi) {
      self.expect(TokenKind::RBracket)?; // consume ']'
      return Ok(Type::Slice(Box::new(element)));
    }

    self.expect(TokenKind::Semi)?; // consume ';'
    let size = self.parse_expression(vec![], ParserContext::Default)?;
    self.expect(TokenKind::RBracket)?; // consume ']'

    Ok(Type::Array {
      element: Box::new(element),
      size: Box::new(size),
    })
  }

  fn parse_tuple_type(&mut self, context: ParserContext) -> Result<Type, ()> {
    let mut types = vec![];

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::RParen) {
      types.push(self.parse_type(context)?);
      match_and_consume!(self, TokenKind::Comma)?;
    }

    self.expect(TokenKind::RParen)?;

    if types.is_empty() {
      return Ok(Type::Unit);
    }

    Ok(Type::Tuple(types))
  }

  fn parse_bare_function_type(&mut self, context: ParserContext) -> Result<Type, ()> {
    self.current -= 1;
    let for_lifetimes = self.parse_for_lifetimes()?;

    let (.., is_unsafe, _, abi) = self.parse_function_flavors()?;
    self.expect(TokenKind::KwFn)?;
    self.expect(TokenKind::LParen)?;
    let (params, is_variadic) = self.parse_bare_function_type_params(context)?;
    self.expect(TokenKind::RParen)?;

    let return_type = if matches!(self.current_token().kind, TokenKind::ThinArrow) {
      self.advance(); // consume `->`
      Some(self.parse_type(context)?)
    } else {
      None
    };

    Ok(Type::BareFn {
      abi,
      params,
      return_type: return_type.map(Box::new),
      is_variadic,
      for_lifetimes,
      safety: match is_unsafe {
        true => Safety::Unsafe,
        false => Safety::Safe,
      },
    })
  }

  fn parse_bare_function_type_params(
    &mut self,
    context: ParserContext,
  ) -> Result<(Vec<Type>, bool), ()> {
    let mut params = vec![];
    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::RParen) {
      if matches!(self.current_token().kind, TokenKind::DotDot) {
        self.expect(TokenKind::DotDot)?; // consume `..`
        self.expect(TokenKind::Dot)?; // consume `.`

        if !matches!(self.current_token().kind, TokenKind::RParen) {
          let bad = self.current_token();

          let diagnostic = self
            .diagnostic(
              DiagnosticError::InvalidVariadic,
              "variadic parameters are not allowed in non-extern functions",
            )
            .with_label(
              bad.span,
              Some("variadic parameters are not allowed in non-extern functions".to_string()),
              LabelStyle::Primary,
            )
            .with_help("variadic parameters are only allowed in extern functions".to_string());
          self.emit(diagnostic);
          return Err(());
        }

        return Ok((params, true));
      }

      params.push(self.parse_type(context)?);
      match_and_consume!(self, TokenKind::Comma)?;
    }
    Ok((params, false))
  }

  fn parse_qpath_type(&mut self, context: ParserContext) -> Result<Type, ()> {
    self.current -= 1;
    // we unwrap here because we know we have a `<` token
    let qself = self.parse_qself_type_header(context)?;
    let path = self.parse_path(false, ParserContext::Type)?;

    Ok(Type::QualifiedPath { qself, path })
  }

  pub(crate) fn parse_type_lifetime(&mut self) -> Result<Option<String>, ()> {
    if matches!(self.current_token().kind, TokenKind::Lifetime { .. })
      && matches!(self.peek_prev(0).kind, TokenKind::Amp)
    {
      let token = self.current_token();
      self.advance(); // consume the lifetime token
      Ok(Some(self.get_token_lexeme(&token)))
    } else {
      Ok(None)
    }
  }

  pub(crate) fn parse_qself_type_header(&mut self, _context: ParserContext) -> Result<QSelf, ()> {
    if !matches!(self.current_token().kind, TokenKind::Lt)
      || !matches!(
        self.peek(1).kind,
        TokenKind::Ident
          | TokenKind::KwSelfType
          | TokenKind::ColonColon
          | TokenKind::KwSelf
          | TokenKind::KwSuper
          | TokenKind::Dollar
          | TokenKind::KwCrate
          | TokenKind::Amp
      )
    {
      let lexeme = self.get_token_lexeme(&self.current_token());
      let diagnostic = self
        .diagnostic(
          DiagnosticError::UnexpectedToken,
          format!("expected a type, found `{lexeme}`"),
        )
        .with_label(
          self.current_token().span,
          Some(format!("expected a type, found `{lexeme}`")),
          LabelStyle::Primary,
        )
        .with_help(format!(
          "If `{lexeme}` is a custom type, declare it or bring it into scope before use.",
        ));
      self.emit(diagnostic);

      return Err(());
    }

    if matches!(self.peek(1).kind, TokenKind::Dollar)
      && matches!(self.peek(2).kind, TokenKind::KwCrate)
    {
      let span = *self.peek(1).span.merge(self.peek(2).span);
      let diagnostic = self
        .diagnostic(DiagnosticError::UnexpectedToken, "invalid path segment")
        .with_label(
          span,
          Some("`$crate` is not allowed after `$`".to_string()),
          LabelStyle::Primary,
        )
        .with_help("`$crate` is only allowed as the first segment of a path.".to_string());
      self.emit(diagnostic);
      return Err(());
    }

    // assumes current token is `<`
    self.expect(TokenKind::Lt)?;
    let self_ty = Box::new(self.parse_type(ParserContext::Type)?);

    let as_trait = if match_and_consume!(self, TokenKind::KwAs)?
      && !matches!(
        self.peek(1).kind,
        TokenKind::Dollar
          | TokenKind::KwCrate
          | TokenKind::KwSelf
          | TokenKind::KwSuper
          | TokenKind::KwSelfType
      ) {
      Some(self.parse_path(true, ParserContext::Type)?)
    } else {
      None
    };

    self.expect(TokenKind::Gt)?;
    self.expect(TokenKind::ColonColon)?;

    Ok(QSelf { as_trait, self_ty })
  }
}
