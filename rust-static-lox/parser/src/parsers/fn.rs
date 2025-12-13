use crate::{
  ast::{pattern::Pattern, *},
  match_and_consume,
  parser_utils::ExprContext,
  DiagnosticEngine, Parser,
};
use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
};
use lexer::token::{LiteralKind, TokenKind};

impl Parser {
  pub(crate) fn parse_fn_decl(
    &mut self,
    attributes: Vec<Attribute>,
    visibility: Visibility,
    engine: &mut DiagnosticEngine,
  ) -> Result<Item, ()> {
    let mut token = self.current_token();

    let (is_async, is_const, is_unsafe, is_extern, abi) = self.parse_function_flavors(engine)?;
    self.advance(engine); // consume the "fn"

    let name = self.parse_name(false, engine)?;
    let generics = self.parse_generic_params(&mut token, engine)?;
    let params = self.parse_function_params(is_extern, ExprContext::Function, engine)?;
    let return_type = self.parse_return_type(engine)?;
    let where_clause = self.parse_where_clause(engine)?;
    let body = Some(self.parse_block(None, ExprContext::Default, vec![], engine)?);

    Ok(Item::Function(FnDecl {
      attributes,
      visibility,
      name,
      generics,
      params,
      return_type,
      where_clause,
      body,
      is_async,
      is_const,
      is_unsafe,
      is_extern,
      abi,
      span: *token.span.merge(self.current_token().span),
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
    engine: &mut DiagnosticEngine,
  ) -> Result<(bool, bool, bool, bool, Option<String>), ()> {
    //  const? async? unsafe? extern abi? fn
    let mut flavor = (false, false, false, false, None);

    if matches!(self.current_token().kind, TokenKind::KwConst) {
      self.advance(engine); // consume "const"
      flavor.0 = true;
    }

    if matches!(self.current_token().kind, TokenKind::KwAsync) {
      self.advance(engine); // consume "async"
      flavor.1 = true;
    }

    if matches!(self.current_token().kind, TokenKind::KwUnsafe) {
      self.advance(engine); // consume "unsafe"
      flavor.2 = true;
    }

    if matches!(self.current_token().kind, TokenKind::KwExtern) {
      self.advance(engine); // consume "extern"
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
        if name != "\"C\"" {
          let diagnostic = Diagnostic::new(
            DiagnosticCode::Error(DiagnosticError::InvalidAbi),
            "invalid ABI".to_string(),
            self.source_file.path.clone(),
          )
          .with_label(
            self.current_token().span,
            Some("invalid ABI".to_string()),
            LabelStyle::Primary,
          )
          .with_help("ABI must be either C or C-like".to_string());
          engine.add(diagnostic);
          return Err(());
        }
        self.advance(engine); // consume "abi"
        flavor.4 = Some(name);
      }
    }

    Ok(flavor)
  }

  fn parse_function_params(
    &mut self,
    is_extern: bool,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Vec<Param>, ()> {
    let mut params = vec![];

    self.expect(TokenKind::OpenParen, engine)?; // consume '('
    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::CloseParen) {
      params.push(self.parse_function_param(is_extern, context, engine)?);
      match_and_consume!(self, engine, TokenKind::Comma)?;
    }
    self.expect(TokenKind::CloseParen, engine)?; // consume ')'

    Ok(params)
  }

  fn parse_function_param(
    &mut self,
    is_extern: bool,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Param, ()> {
    let mut token = self.current_token();
    let attributes = self.parse_outer_attributes(engine)?;

    let kind = if matches!(self.current_token().kind, TokenKind::DotDot) {
      if !is_extern {
        let mut token = self.current_token();
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidVariadic),
          "variadic parameters are not allowed in non-extern functions".to_string(),
          self.source_file.path.clone(),
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
        engine.add(diagnostic);
        return Err(());
      }

      self.expect(TokenKind::DotDot, engine)?; // consume '..'
      self.expect(TokenKind::Dot, engine)?; // consume '.'
      ParamKind::Variadic
    } else {
      let pattern = self.parse_pattern(context, engine)?;
      let is_self = self.check_self_param(&pattern, context, engine)?;

      ParamKind::Normal {
        pattern,
        type_annotation: if matches!(self.current_token().kind, TokenKind::Colon) {
          self.advance(engine); // consume ':'
          Some(self.parse_type(engine)?)
        } else {
          None
        },
        is_self,
      }
    };

    Ok(Param {
      attributes,
      kind,
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn check_self_param(
    &mut self,
    pattern: &Pattern,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<bool, ()> {
    if let Pattern::Ident { name, span, .. } = pattern {
      if name == "self" {
        if matches!(context, ExprContext::Impl | ExprContext::Trait) {
          return Ok(true);
        }

        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidSelfInFreeFunction),
          "self is not allowed in free functions".to_string(),
          self.source_file.path.clone(),
        )
        .with_label(
          *span,
          Some("self is not allowed in free functions".to_string()),
          LabelStyle::Primary,
        )
        .with_help("self is not allowed in free functions".to_string());
        engine.add(diagnostic);
        return Err(());
      }
    }

    Ok(false)
  }

  fn parse_return_type(&mut self, engine: &mut DiagnosticEngine) -> Result<Option<Type>, ()> {
    if !matches!(self.current_token().kind, TokenKind::ThinArrow) {
      return Ok(None);
    }

    self.expect(TokenKind::ThinArrow, engine)?; // consume the "->"
    let return_type = self.parse_type(engine)?;

    Ok(Some(return_type))
  }
}
