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

    let name = self.parse_name_identifier(engine)?;
    let generics = self.parse_generic_params(&mut token, engine)?;
    let params = self.parse_function_params(ExprContext::Function, engine)?;
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

  fn parse_function_flavors(
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
        if name != "C" {
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
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Vec<Param>, ()> {
    let mut params = vec![];

    self.expect(TokenKind::OpenParen, engine)?; // consume '('
    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::CloseParen) {
      params.push(self.parse_function_param(context, engine)?);
      match_and_consume!(self, engine, TokenKind::Comma)?;
    }
    self.expect(TokenKind::CloseParen, engine)?; // consume ')'

    Ok(params)
  }

  fn parse_function_param(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Param, ()> {
    let attributes = self.parse_outer_attributes(engine)?;
    let pattern = self.parse_pattern(context, engine)?;
    let is_self = self.check_self_param(&pattern, context, engine)?;
    let type_annotation = if matches!(self.current_token().kind, TokenKind::Colon) {
      self.advance(engine); // consume ':'
      Some(self.parse_type(engine)?)
    } else {
      None
    };

    Ok(Param {
      attributes,
      pattern,
      type_annotation,
      is_self,
      is_variadic: false, // Is allowed only in extern functions using a C style ABI.
    })
  }

  fn check_self_param(
    &mut self,
    pattern: &Pattern,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<bool, ()> {
    match pattern {
      Pattern::Ident { name, .. } if matches!(context, ExprContext::Impl | ExprContext::Trait) => {
        if name == "self" {
          Ok(true)
        } else {
          Ok(false)
        }
      },
      _ => {
        let token = self.tokens[self.current - 1].clone();
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidSelfInFreeFunction),
          "self is not allowed in free functions".to_string(),
          self.source_file.path.clone(),
        )
        .with_label(
          token.span,
          Some("self is not allowed in free functions".to_string()),
          LabelStyle::Primary,
        )
        .with_help("self is not allowed in free functions".to_string());
        engine.add(diagnostic);
        Err(())
      },
    }
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
