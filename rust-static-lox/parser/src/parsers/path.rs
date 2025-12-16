use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
};
use lexer::token::TokenKind;

use crate::{
  ast::{path::*, Expr, ExprKind},
  DiagnosticEngine, Parser,
};

impl Parser {
  pub(crate) fn parse_qualified_path(&mut self, engine: &mut DiagnosticEngine) -> Result<Expr, ()> {
    let mut token = self.current_token();

    let qself = self.parse_qself_type_header(engine)?;
    let path = self.parse_path(true, engine)?;

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Path {
        qself: Some(qself),
        path,
      },
      span: *token.span.merge(self.current_token().span),
    })
  }

  pub(crate) fn parse_path_expr(
    &mut self,
    with_args: bool,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();
    let path = self.parse_path(with_args, engine)?;

    if matches!(
      self.current_token().kind,
      TokenKind::OpenBrace | TokenKind::OpenParen
    ) {
      return self.parse_struct_expr(path, engine);
    }

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Path { qself: None, path },
      span: *token.span.merge(self.current_token().span),
    })
  }

  pub(crate) fn parse_path(
    &mut self,
    with_args: bool,
    engine: &mut DiagnosticEngine,
  ) -> Result<Path, ()> {
    // Handle leading '::' (absolute paths)
    let mut leading_colon = false;
    if matches!(self.current_token().kind, TokenKind::ColonColon) {
      leading_colon = true;
      self.advance(engine); // consume '::'
    }

    // Parse the first segment
    let (first_segment, _) = self.parse_path_segment(with_args, engine)?;
    let mut segments = vec![first_segment];

    // Parse additional `::`-separated segments
    while !self.is_eof()
      && (!matches!(
        self.current_token().kind,
        TokenKind::CloseBracket
          | TokenKind::Lt
          | TokenKind::Eq
          | TokenKind::OpenParen
          | TokenKind::OpenBrace
          | TokenKind::CloseParen
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
      ) && !matches!(self.peek(1).kind, TokenKind::Lt))
    {
      self.expect(TokenKind::ColonColon, engine)?; // require '::' separator

      let (segment, is_dollar_crate) = self.parse_path_segment(with_args, engine)?;
      if is_dollar_crate {
        // `$crate` can only appear as the first path segment
        let offending = self.peek_prev(0);
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
          "Unexpected `$crate` segment in path".to_string(),
          self.source_file.path.clone(),
        )
        .with_label(
          offending.span,
          Some("`$crate` cannot appear in this position".to_string()),
          LabelStyle::Primary,
        )
        .with_help("`$crate` is only valid as the first path segment.".to_string());
        engine.add(diagnostic);
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
    engine: &mut DiagnosticEngine,
  ) -> Result<(PathSegment, bool), ()> {
    let token = self.current_token();
    self.advance(engine); // consume the segment identifier or keyword

    // Optional generic arguments
    let args = if with_args
      && matches!(self.current_token().kind, TokenKind::Lt)
      && !matches!(self.peek(1).kind, TokenKind::Lt)
    {
      self.parse_generic_args(engine)?
    } else if matches!(self.current_token().kind, TokenKind::ColonColon)
      && matches!(self.peek(1).kind, TokenKind::Lt)
    {
      self.advance(engine);
      self.parse_generic_args(engine)?
    } else {
      None
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
        self.advance(engine); // consume `$crate`
        Ok((PathSegment::new(PathSegmentKind::DollarCrate, args), true))
      },
      _ => {
        // Invalid path segment (e.g. `123::foo`)
        let lexeme = self.get_token_lexeme(&token);
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
          "Unexpected token in path segment".to_string(),
          self.source_file.path.clone(),
        )
        .with_label(
          token.span,
          Some(format!("Expected a path segment, found `{lexeme}`")),
          LabelStyle::Primary,
        )
        .with_help("Valid path segments are identifiers or keywords like `self`, `super`, `crate`, or `$crate`.".to_string());
        engine.add(diagnostic);
        Err(())
      },
    }
  }
}
