use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::TokenKind;

use crate::{
  ast::{path::*, Expr, ExprKind},
  match_and_consume,
  parser_utils::ParserContext,
  Parser,
};

impl Parser {
  pub(crate) fn parse_path_expr(
    &mut self,
    with_args: bool,
    context: ParserContext,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();

    let path = self.parse_path(with_args, context)?;

    if match_and_consume!(self, TokenKind::Bang)? {
      // macro invocation expression
      let mac = self.parse_macro_invocation(path)?;

      return Ok(Expr {
        attributes: vec![],
        kind: ExprKind::Macro { mac },
        span: *token.span.merge(self.current_token().span),
      });
    }

    if matches!(self.current_token().kind, TokenKind::LBrace)
      && !matches!(
        context,
        ParserContext::Match | ParserContext::IfCondition | ParserContext::WhileCondition
      )
    {
      // macro struct expression
      return self.parse_struct_expr(path, context);
    }

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Path { qself: None, path },
      span: *token.span.merge(self.current_token().span),
    })
  }

  pub(crate) fn parse_qualified_path(&mut self, context: ParserContext) -> Result<Expr, ()> {
    let mut token = self.current_token();
    let qself = self.parse_qself_type_header(context)?;
    let path = self.parse_path(true, context)?;

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Path {
        qself: Some(qself),
        path,
      },
      span: *token.span.merge(self.current_token().span),
    })
  }

  pub(crate) fn parse_path(&mut self, with_args: bool, context: ParserContext) -> Result<Path, ()> {
    // Handle leading '::' (absolute paths)
    let mut leading_colon = false;
    if matches!(self.current_token().kind, TokenKind::ColonColon) {
      leading_colon = true;
      self.advance(); // consume '::'
    }

    // Parse the first segment
    let (first_segment, _) = self.parse_path_segment(with_args, context)?;
    let mut segments = vec![first_segment];

    // Parse additional `::`-separated segments
    while !self.is_eof()
      && !matches!(
        self.current_token().kind,
        TokenKind::RBracket
          | TokenKind::RBrace
          | TokenKind::Eq
          | TokenKind::Question
          | TokenKind::Lt
          | TokenKind::LParen
          | TokenKind::LBrace
          | TokenKind::LBracket
          | TokenKind::RParen
          | TokenKind::Comma
          | TokenKind::Semi
          | TokenKind::Gt
          | TokenKind::Or
          | TokenKind::Plus
          | TokenKind::Colon
          | TokenKind::KwAs
          | TokenKind::FatArrow
          | TokenKind::Bang
          | TokenKind::Dot
      )
    {
      self.expect(TokenKind::ColonColon)?; // require '::' separator

      if !matches!(
        self.current_token().kind,
        TokenKind::Ident
          | TokenKind::KwSelf
          | TokenKind::KwSuper
          | TokenKind::KwCrate
          | TokenKind::KwSelfType
          | TokenKind::Dollar
      ) {
        let found = self.get_token_lexeme(&self.current_token());
        self.emit(self.err_invalid_path_segment(self.current_token().span, &found));
        return Err(());
      }

      let (segment, is_dollar_crate) = self.parse_path_segment(with_args, context)?;
      if is_dollar_crate && !segments.is_empty() {
        let offending = self.peek_prev(0);
        self.emit(self.err_unexpected_token(offending.span, "path segment", "$crate"));
        return Err(());
      }

      segments.push(segment);
    }

    Ok(Path {
      leading_colon,
      segments,
    })
  }

  pub(crate) fn parse_path_segment(
    &mut self,
    with_args: bool,
    context: ParserContext,
  ) -> Result<(PathSegment, bool), ()> {
    let mut token = self.current_token();
    self.advance(); // consume the segment identifier or keyword

    if matches!(
      token.kind,
      TokenKind::KwSelf | TokenKind::KwSuper | TokenKind::KwCrate | TokenKind::Dollar
    ) && matches!(self.peek(1).kind, TokenKind::Lt)
    {
      let span = *token.span.merge(self.current_token().span);
      let diagnostic = self
        .diagnostic(DiagnosticError::UnexpectedToken, "invalid path segment")
        .with_label(
          span,
          Some("generic arguments are not allowed on this path segment".to_string()),
          LabelStyle::Primary,
        )
        .with_help(
          "`self`, `super`, and `crate` cannot have generic arguments. \
             Only types and identifiers may be generic."
            .to_string(),
        );
      self.emit(diagnostic);
      return Err(());
    }

    // This handles the case where we only expect a colon colon in any context other than a
    // type context
    if !matches!(context, ParserContext::Type) && matches!(self.peek(1).kind, TokenKind::Lt) {
      self.expect(TokenKind::ColonColon)?;
    }

    if matches!(
      context,
      ParserContext::WhileCondition | ParserContext::Closure
    ) {
      return Ok((
        PathSegment::new(PathSegmentKind::Ident(self.get_token_lexeme(&token)), None),
        false,
      ));
    }

    let args = match (with_args, self.peek(0).kind, self.peek(1).kind) {
      (true, TokenKind::Lt, TokenKind::Gt) => None,
      (true, TokenKind::Lt, _) => self.parse_generic_args(context)?,
      (_, TokenKind::ColonColon, TokenKind::Lt) => {
        self.advance();
        self.parse_generic_args(context)?
      },
      _ => None,
    };

    match token.kind {
      TokenKind::KwSelf => Ok((PathSegment::new(PathSegmentKind::Self_, args), false)),
      TokenKind::KwSuper => Ok((PathSegment::new(PathSegmentKind::Super, args), false)),
      TokenKind::KwCrate => Ok((PathSegment::new(PathSegmentKind::Crate, args), false)),
      TokenKind::Ident => Ok((
        PathSegment::new(PathSegmentKind::Ident(self.get_token_lexeme(&token)), args),
        false,
      )),
      TokenKind::KwSelfType => Ok((PathSegment::new(PathSegmentKind::SelfType, args), false)),
      TokenKind::Dollar if self.peek(0).kind == TokenKind::KwCrate => {
        self.advance(); // consume `$crate`
        Ok((PathSegment::new(PathSegmentKind::DollarCrate, args), true))
      },
      _ => {
        let lexeme = self.get_token_lexeme(&token);
        self.emit(self.err_invalid_path_segment(token.span, &lexeme));
        Err(())
      },
    }
  }
}
