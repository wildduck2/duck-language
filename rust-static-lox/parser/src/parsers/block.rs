use crate::{
  ast::{
    attrs::Attribute,
    expr::{BlockFlavor, Expr, ExprKind},
  },
  parser_utils::ParserContext,
  Parser,
};
use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_block(
    &mut self,
    label: Option<String>,
    context: ParserContext,
    outer_attributes: Vec<Attribute>,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();
    if !outer_attributes.is_empty() {
      token.span.merge(outer_attributes[0].span);
    }

    let flavor = self.parse_block_expression_flavors(context)?;
    self.advance(); // consume the "{"

    let mut stmts = vec![];

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::RBrace) {
      stmts.push(self.parse_stmt(context)?);
    }
    self.expect(TokenKind::RBrace)?;

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Block {
        tail: None,
        outer_attributes,
        stmts,
        label,
        flavor,
      },
      span: *token.span.merge(self.last_token_span()),
    })
  }

  pub(crate) fn parse_block_expression_flavors(
    &mut self,
    _context: ParserContext,
  ) -> Result<BlockFlavor, ()> {
    let start_span = self.current_token().span;

    let mut is_const = false;
    let mut is_async = false;
    let mut is_move = false;
    let mut is_unsafe = false;
    let mut is_try = false;

    // First keyword
    match self.current_token().kind {
      TokenKind::KwConst => {
        is_const = true;
        self.advance();

        match self.current_token().kind {
          TokenKind::KwAsync
          | TokenKind::KwMove
          | TokenKind::KwUnsafe
          | TokenKind::KwTry
          | TokenKind::KwConst => {
            let details = "const blocks cannot be combined with other flavors";
            let diagnostic = self
              .diagnostic(
                DiagnosticError::InvalidFlavorOrder,
                format!("invalid block flavor order: {details}"),
              )
              .with_label(start_span, Some(details.to_string()), LabelStyle::Primary)
              .with_help("use `const { ... }` by itself for a const block".to_string());
            self.emit(diagnostic);
            return Err(());
          },
          _ => {},
        }
      },

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
            let details = "async blocks may only be followed by optional move";
            let diagnostic = self
              .diagnostic(
                DiagnosticError::InvalidFlavorOrder,
                format!("invalid block flavor order: {details}"),
              )
              .with_label(start_span, Some(details.to_string()), LabelStyle::Primary)
              .with_help(
                "use `async` optionally followed by `move`, or use `unsafe`/`try` alone"
                  .to_string(),
              );
            self.emit(diagnostic);
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
            let details = "unsafe cannot be mixed with other block flavors";
            let diagnostic = self
              .diagnostic(
                DiagnosticError::InvalidFlavorOrder,
                format!("invalid block flavor order: {details}"),
              )
              .with_label(start_span, Some(details.to_string()), LabelStyle::Primary)
              .with_help(
                "use `async` optionally followed by `move`, or use `unsafe`/`try` alone"
                  .to_string(),
              );
            self.emit(diagnostic);
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
            let details = "try must appear alone before a block";
            let diagnostic = self
              .diagnostic(
                DiagnosticError::InvalidFlavorOrder,
                format!("invalid block flavor order: {details}"),
              )
              .with_label(start_span, Some(details.to_string()), LabelStyle::Primary)
              .with_help(
                "use `async` optionally followed by `move`, or use `unsafe`/`try` alone"
                  .to_string(),
              );
            self.emit(diagnostic);
            return Err(());
          },
          _ => {},
        }
      },

      TokenKind::KwMove => {
        let details = "move cannot introduce a block";
        let diagnostic = self
          .diagnostic(
            DiagnosticError::InvalidFlavorOrder,
            format!("invalid block flavor order: {details}"),
          )
          .with_label(start_span, Some(details.to_string()), LabelStyle::Primary)
          .with_help(
            "use `async` optionally followed by `move`, or use `unsafe`/`try` alone".to_string(),
          );
        self.emit(diagnostic);
        return Err(());
      },

      _ => {},
    }

    // Check next token is a brace if any flavor was used
    if (is_const || is_async || is_move || is_unsafe || is_try)
      && !matches!(self.current_token().kind, TokenKind::LBrace)
    {
      let flavor = if is_const {
        "const"
      } else if is_unsafe {
        "unsafe"
      } else if is_try {
        "try"
      } else {
        "async"
      };
      let diagnostic = self
        .diagnostic(
          DiagnosticError::ExpectedBlockAfterFlavor,
          format!("expected block after `{flavor}`"),
        )
        .with_label(
          start_span,
          Some(format!("expected a block `{{ ... }}` after `{flavor}`")),
          LabelStyle::Primary,
        )
        .with_help(format!("add a block after `{flavor}`"));
      self.emit(diagnostic);
      return Err(());
    }

    let flavor = if is_const {
      BlockFlavor::Const
    } else if is_async {
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

    // Case 1 const
    if matches!(self.peek(i).kind, TokenKind::KwConst) {
      i += 1;
      return matches!(self.peek(i).kind, TokenKind::LBrace);
    }

    // Case 2 async or async move
    if matches!(self.peek(i).kind, TokenKind::KwAsync) {
      i += 1;

      // optional move
      if matches!(self.peek(i).kind, TokenKind::KwMove) {
        i += 1;
      }

      return matches!(self.peek(i).kind, TokenKind::LBrace);
    }

    // Case 3 unsafe
    if matches!(self.peek(i).kind, TokenKind::KwUnsafe) {
      i += 1;
      return matches!(self.peek(i).kind, TokenKind::LBrace);
    }

    // Case 4 try (nightly only)
    if matches!(self.peek(i).kind, TokenKind::KwTry) {
      i += 1;
      return matches!(self.peek(i).kind, TokenKind::LBrace);
    }

    // Case 5 plain block
    matches!(self.peek(i).kind, TokenKind::LBrace)
  }
}
