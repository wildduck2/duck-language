use crate::{
  ast::{
    attrs::Attribute,
    expr::{BlockFlavor, Expr, ExprKind},
  },
  match_and_consume,
  parser_utils::ExprContext,
  Parser,
};
use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
};
use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_block(
    &mut self,
    label: Option<String>,
    context: ExprContext,
    outer_attributes: Vec<Attribute>,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();
    if !outer_attributes.is_empty() {
      token.span.merge(outer_attributes[0].span);
    }

    let flavor = self.parse_block_expression_flavors(ExprContext::Default)?;
    self.advance(); // consume the "{"

    let mut stmts = vec![];

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::CloseBrace) {
      stmts.push(self.parse_stmt(context)?);
      match_and_consume!(self, TokenKind::Semi)?;
    }
    self.expect(TokenKind::CloseBrace)?;

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Block {
        tail: None, // TODO: implement tail
        outer_attributes,
        stmts,
        label,
        flavor,
      },
      span: *token.span.merge(self.current_token().span),
    })
  }

  pub(crate) fn parse_block_expression_flavors(
    &mut self,
    context: ExprContext,
  ) -> Result<BlockFlavor, ()> {
    if !matches!(context, ExprContext::Default) {
      let token = self.current_token();
      let diagnostic = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::InvalidBlockFlavorContext),
        "block flavor not allowed here".to_string(),
        self.source_file.path.clone(),
      )
      .with_label(
        token.span,
        Some("cannot start a flavored block in this position".to_string()),
        LabelStyle::Primary,
      )
      .with_help("allowed examples: unsafe { } or async { }".to_string());
      self.engine.borrow_mut().add(diagnostic);
      return Err(());
    }

    let start_span = self.current_token().span;

    let mut is_async = false;
    let mut is_move = false;
    let mut is_unsafe = false;
    let mut is_try = false;

    // First keyword
    match self.current_token().kind {
      TokenKind::KwAsync => {
        is_async = true;
        self.advance();

        // optional move
        if matches!(self.current_token().kind, TokenKind::KwMove) {
          is_move = true;
          self.advance();
        }

        // forbid any other keyword after async or async move
        match self.current_token().kind {
          TokenKind::KwAsync | TokenKind::KwUnsafe | TokenKind::KwTry | TokenKind::KwMove => {
            let diagnostic = Diagnostic::new(
              DiagnosticCode::Error(DiagnosticError::InvalidFlavorOrder),
              "invalid block flavor order".to_string(),
              self.source_file.path.clone(),
            )
            .with_label(
              start_span,
              Some("async blocks may only be followed by optional move".to_string()),
              LabelStyle::Primary,
            )
            .with_help("valid examples: async { } or async move { }".to_string());
            self.engine.borrow_mut().add(diagnostic);
            return Err(());
          },
          _ => {},
        }
      },

      TokenKind::KwUnsafe => {
        is_unsafe = true;
        self.advance();

        // unsafe cannot be followed by any other flavor
        match self.current_token().kind {
          TokenKind::KwAsync | TokenKind::KwMove | TokenKind::KwTry | TokenKind::KwUnsafe => {
            let diagnostic = Diagnostic::new(
              DiagnosticCode::Error(DiagnosticError::InvalidFlavorOrder),
              "invalid block flavor order".to_string(),
              self.source_file.path.clone(),
            )
            .with_label(
              start_span,
              Some("unsafe cannot be mixed with other block flavors".to_string()),
              LabelStyle::Primary,
            )
            .with_help("valid example: unsafe { }".to_string());
            self.engine.borrow_mut().add(diagnostic);
            return Err(());
          },
          _ => {},
        }
      },

      TokenKind::KwTry => {
        is_try = true;
        self.advance();

        // try cannot be followed by anything
        match self.current_token().kind {
          TokenKind::KwAsync | TokenKind::KwMove | TokenKind::KwUnsafe | TokenKind::KwTry => {
            let diagnostic = Diagnostic::new(
              DiagnosticCode::Error(DiagnosticError::InvalidFlavorOrder),
              "invalid block flavor order".to_string(),
              self.source_file.path.clone(),
            )
            .with_label(
              start_span,
              Some("try must appear alone before a block".to_string()),
              LabelStyle::Primary,
            )
            .with_help("valid example: try { }".to_string());
            self.engine.borrow_mut().add(diagnostic);
            return Err(());
          },
          _ => {},
        }
      },

      TokenKind::KwMove => {
        // move alone is not allowed for block
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidFlavorOrder),
          "invalid block flavor".to_string(),
          self.source_file.path.clone(),
        )
        .with_label(
          start_span,
          Some("move cannot introduce a block".to_string()),
          LabelStyle::Primary,
        )
        .with_help("use async move { } instead".to_string());
        self.engine.borrow_mut().add(diagnostic);
        return Err(());
      },

      _ => {},
    }

    // Check next token is a brace if any flavor was used
    if (is_async || is_move || is_unsafe || is_try)
      && !matches!(self.current_token().kind, TokenKind::OpenBrace)
    {
      let diagnostic = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::ExpectedBlockAfterFlavor),
        "expected block after flavor keyword".to_string(),
        self.source_file.path.clone(),
      )
      .with_label(
        start_span,
        Some("a flavored block must be followed by a left brace".to_string()),
        LabelStyle::Primary,
      )
      .with_label(
        self.current_token().span,
        Some("this token does not start a block".to_string()),
        LabelStyle::Secondary,
      )
      .with_help(
        "valid examples: async { } or async move { } or unsafe { } or try { }".to_string(),
      );
      self.engine.borrow_mut().add(diagnostic);
      return Err(());
    }

    let flavor = if is_async {
      if is_move {
        BlockFlavor::AsyncMove
      } else {
        BlockFlavor::Async
      }
    } else if is_unsafe {
      BlockFlavor::Unsafe
    } else if is_try {
      BlockFlavor::Try
    } else {
      BlockFlavor::Normal
    };

    Ok(flavor)
  }

  pub(crate) fn can_start_block_expression(&self) -> bool {
    let mut i = 0;

    // Case 1 async or async move
    if matches!(self.peek(i).kind, TokenKind::KwAsync) {
      i += 1;

      // optional move
      if matches!(self.peek(i).kind, TokenKind::KwMove) {
        i += 1;
      }

      return matches!(self.peek(i).kind, TokenKind::OpenBrace);
    }

    // Case 2 unsafe
    if matches!(self.peek(i).kind, TokenKind::KwUnsafe) {
      i += 1;
      return matches!(self.peek(i).kind, TokenKind::OpenBrace);
    }

    // Case 3 try (nightly only)
    if matches!(self.peek(i).kind, TokenKind::KwTry) {
      i += 1;
      return matches!(self.peek(i).kind, TokenKind::OpenBrace);
    }

    // Case 4 plain block
    matches!(self.peek(i).kind, TokenKind::OpenBrace)
  }
}
