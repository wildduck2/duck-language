use crate::{
  ast::{pattern::Pattern, *},
  match_and_consume,
  parser_utils::ParserContext,
  Parser,
};
use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::{LiteralKind, TokenKind};

impl Parser {
  pub(crate) fn parse_fn_decl(
    &mut self,
    attributes: Vec<Attribute>,
    visibility: Visibility,
    context: ParserContext,
  ) -> Result<Item, ()> {
    let mut token = self.current_token();

    let (is_async, is_const, is_unsafe, is_extern, abi) = self.parse_function_flavors()?;
    self.advance(); // consume the "fn"

    let name = self.parse_name(false)?;
    let generics = self.parse_generic_params(&mut token, context)?;
    let params = self.parse_function_params(is_extern, ParserContext::Function)?;
    let return_type = self.parse_return_type(context)?;
    let where_clause = self.parse_where_clause(context)?;

    let has_semi = matches!(self.current_token().kind, TokenKind::Semi);
    let has_body = matches!(self.current_token().kind, TokenKind::LBrace);

    let body = match context {
      ParserContext::Trait => {
        if has_semi {
          self.expect(TokenKind::Semi)?;
          None
        } else if has_body {
          Some(self.parse_block(None, ParserContext::Default, vec![])?)
        } else {
          let found = self.get_token_lexeme(&self.current_token());
          self.emit(self.err_unexpected_token(self.current_token().span, "`;` or `{`", &found));
          return Err(());
        }
      },
      ParserContext::Impl | ParserContext::Function => {
        if has_semi {
          let found = self.get_token_lexeme(&self.current_token());
          self.emit(self.err_unexpected_token(self.current_token().span, "`{`", &found));
          return Err(());
        }

        if has_body {
          Some(self.parse_block(None, ParserContext::Default, vec![])?)
        } else {
          let found = self.get_token_lexeme(&self.current_token());
          self.emit(self.err_unexpected_token(self.current_token().span, "`{`", &found));
          return Err(());
        }
      },
      ParserContext::Extern => {
        if has_semi {
          self.expect(TokenKind::Semi)?;
          None
        } else {
          let found = self.get_token_lexeme(&self.current_token());
          self.emit(self.err_unexpected_token(self.current_token().span, "`;`", &found));
          return Err(());
        }
      },
      _ => {
        if has_body {
          Some(self.parse_block(None, ParserContext::Default, vec![])?)
        } else {
          let found = self.get_token_lexeme(&self.current_token());
          self.emit(self.err_unexpected_token(self.current_token().span, "`{`", &found));
          return Err(());
        }
      },
    };

    Ok(Item::Vis(VisItem {
      attributes,
      visibility,
      kind: VisItemKind::Function(FnDecl {
        sig: FnSig {
          name,
          generics,
          params,
          return_type,
          where_clause,
        },
        body,
        is_async,
        is_const,
        is_unsafe,
        is_extern,
        abi,
      }),
      span: *token.span.merge(self.last_token_span()),
    }))
  }

  /// Returns true when the upcoming token sequence represents the start of a
  /// free function item. Rust allows a small set of qualifiers before `fn`
  /// (`const`, `async`, `unsafe`, `extern "abi"`), so this helper walks that
  /// prefix before deciding whether the parser should treat the construct as
  /// an item rather than an expression.
  pub(crate) fn can_start_fun(&self) -> bool {
    use TokenKind::{KwAsync, KwConst, KwExtern, KwFn, KwUnsafe};

    let mut offset = 0;

    while self.current + offset < self.tokens.len() && offset <= 6 {
      match self.peek(offset).kind {
        KwFn => return true,

        KwConst | KwAsync | KwUnsafe => {
          offset += 1;
        },

        KwExtern => {
          offset += 1;

          if self.current + offset >= self.tokens.len() {
            return false;
          }

          if matches!(self.peek(offset).kind, TokenKind::Literal { .. }) {
            offset += 1;
          }
        },

        _ => return false,
      }
    }

    false
  }

  pub(crate) fn parse_function_flavors(
    &mut self,
  ) -> Result<(bool, bool, bool, bool, Option<String>), ()> {
    //  const? async? unsafe? extern abi? fn
    let mut flavor = (false, false, false, false, None);

    if matches!(self.current_token().kind, TokenKind::KwConst) {
      self.advance(); // consume "const"
      flavor.0 = true;
    }

    if matches!(self.current_token().kind, TokenKind::KwAsync) {
      self.advance(); // consume "async"
      flavor.1 = true;
    }

    if matches!(self.current_token().kind, TokenKind::KwUnsafe) {
      self.advance(); // consume "unsafe"
      flavor.2 = true;
    }

    if matches!(self.current_token().kind, TokenKind::KwExtern) {
      self.advance(); // consume "extern"
      flavor.3 = true;
      if matches!(
        self.current_token().kind,
        TokenKind::Literal {
          kind: LiteralKind::Str
        } | TokenKind::Literal {
          kind: LiteralKind::RawCStr { .. }
        }
      ) {
        let name = self.get_token_lexeme(&self.current_token());
        if name != "C" {
          let diagnostic = self
            .diagnostic(DiagnosticError::InvalidAbi, "invalid ABI")
            .with_label(
              self.current_token().span,
              Some("invalid ABI".to_string()),
              LabelStyle::Primary,
            )
            .with_help("ABI must be either C or C-like".to_string());
          self.emit(diagnostic);
          return Err(());
        }
        self.advance(); // consume "abi"
        flavor.4 = Some(name);
      }
    }

    Ok(flavor)
  }

  pub(crate) fn parse_function_params(
    &mut self,
    is_extern: bool,
    context: ParserContext,
  ) -> Result<Vec<Param>, ()> {
    let mut params = vec![];

    self.expect(TokenKind::LParen)?; // consume '('
    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::RParen) {
      params.push(self.parse_function_param(is_extern, context)?);
      match_and_consume!(self, TokenKind::Comma)?;
    }
    self.expect(TokenKind::RParen)?; // consume ')'

    Ok(params)
  }

  fn parse_function_param(&mut self, is_extern: bool, context: ParserContext) -> Result<Param, ()> {
    let mut token = self.current_token();
    let attributes = self.parse_outer_attributes(context)?;

    let kind = if matches!(self.current_token().kind, TokenKind::DotDot) {
      if !is_extern {
        let mut token = self.current_token();
        let diagnostic = self
          .diagnostic(
            DiagnosticError::InvalidVariadic,
            "variadic parameters are not allowed in non-extern functions",
          )
          .with_label(
            *token.span.merge(diagnostic::Span {
              start: token.span.start,
              end: token.span.end + 1,
            }),
            Some("variadic parameters are not allowed in non-extern functions".to_string()),
            LabelStyle::Primary,
          )
          .with_help("variadic parameters are only allowed in extern functions".to_string());
        self.emit(diagnostic);
        return Err(());
      }

      self.expect(TokenKind::DotDot)?;
      self.expect(TokenKind::Dot)?;
      ParamKind::Variadic
    } else {
      let pattern = self.parse_pattern(context)?;

      let type_annotation = if matches!(self.current_token().kind, TokenKind::Colon) {
        self.advance();
        Some(self.parse_type(context)?)
      } else {
        None
      };

      if let Some(self_param) =
        self.lower_self_param(&pattern, type_annotation.as_ref(), context)?
      {
        ParamKind::SelfParam(self_param)
      } else {
        ParamKind::Normal {
          pattern,
          type_annotation,
        }
      }
    };

    Ok(Param {
      attributes,
      kind,
      span: *token.span.merge(self.last_token_span()),
    })
  }

  fn lower_self_param(
    &mut self,
    pattern: &Pattern,
    type_annotation: Option<&Type>,
    _context: ParserContext,
  ) -> Result<Option<SelfParam>, ()> {
    use crate::ast::{Mutability, Pattern};

    match pattern {
      // `self`
      Pattern::Ident {
        binding,
        name,
        subpattern: None,
        ..
      } if name == "self" => {
        let mutability = match binding {
          BindingMode::ByValue(m) => m.clone(),
          BindingMode::ByRef(_) => {
            let diagnostic = self
              .diagnostic(DiagnosticError::InvalidSelfParam, "invalid self parameter")
              .with_help("use `&self` or `&mut self`, not `ref self`".to_string());
            self.emit(diagnostic);
            return Err(());
          },
        };

        if let Some(ty) = type_annotation {
          return Ok(Some(SelfParam::Typed {
            mutability,
            ty: ty.clone(),
          }));
        }

        return Ok(Some(SelfParam::Shorthand {
          reference: None,
          mutability,
        }));
      },

      // `&self` or `&mut self` or `&'a self`
      Pattern::Reference {
        depth: 1,
        pattern: inner,
        ..
      } => match &**inner {
        Pattern::Ident {
          binding,
          name,
          subpattern: None,
          ..
        } if name == "self" => {
          let (mutability, lifetime) = match binding {
            BindingMode::ByValue(_) => (Mutability::Immutable, None),
            BindingMode::ByRef(m) => (m.clone(), None),
          };

          if type_annotation.is_some() {
            let diagnostic = self
              .diagnostic(
                DiagnosticError::InvalidSelfParam,
                "typed self parameters cannot be references",
              )
              .with_help("use `self: Type` or `&self` syntax, not both".to_string());
            self.emit(diagnostic);
            return Err(());
          }

          return Ok(Some(SelfParam::Shorthand {
            reference: Some(SelfRef { lifetime }),
            mutability,
          }));
        },

        _ => {},
      },

      // not a self parameter
      _ => {},
    }

    Ok(None)
  }

  pub(crate) fn parse_return_type(&mut self, context: ParserContext) -> Result<Option<Type>, ()> {
    if !matches!(self.current_token().kind, TokenKind::ThinArrow) {
      return Ok(None);
    }

    self.expect(TokenKind::ThinArrow)?; // consume the "->"
    let return_type = self.parse_type(context)?;

    Ok(Some(return_type))
  }
}
