use crate::{
  ast::{
    attrs::Attribute,
    expr::{BlockFlavor, Expr, ExprKind},
  },
  match_and_consume,
  parser_utils::ParserContext,
  Parser,
};
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

    let flavor = self.parse_block_expression_flavors(ParserContext::Default)?;
    self.advance(); // consume the "{"

    let mut stmts = vec![];

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::RBrace) {
      stmts.push(self.parse_stmt(context)?);
    }
    self.expect(TokenKind::RBrace)?;

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Block {
        tail: None, // TODO: implement tail
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
    context: ParserContext,
  ) -> Result<BlockFlavor, ()> {
    if !matches!(context, ParserContext::Default) {
      let token = self.current_token();
      let flavor = "block flavor";
      self.emit(self.err_invalid_block_flavor_context(token.span, flavor, "this context"));
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
            self.emit(self.err_invalid_flavor_order(
              start_span,
              "async blocks may only be followed by optional move",
            ));
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
            self.emit(self.err_invalid_flavor_order(
              start_span,
              "unsafe cannot be mixed with other block flavors",
            ));
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
            self.emit(
              self.err_invalid_flavor_order(start_span, "try must appear alone before a block"),
            );
            return Err(());
          },
          _ => {},
        }
      },

      TokenKind::KwMove => {
        self.emit(self.err_invalid_flavor_order(start_span, "move cannot introduce a block"));
        return Err(());
      },

      _ => {},
    }

    // Check next token is a brace if any flavor was used
    if (is_async || is_move || is_unsafe || is_try)
      && !matches!(self.current_token().kind, TokenKind::LBrace)
    {
      let flavor = if is_async {
        "async"
      } else if is_unsafe {
        "unsafe"
      } else if is_try {
        "try"
      } else {
        "move"
      };
      self.emit(self.err_expected_block_after_flavor(start_span, flavor));
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

      return matches!(self.peek(i).kind, TokenKind::LBrace);
    }

    // Case 2 unsafe
    if matches!(self.peek(i).kind, TokenKind::KwUnsafe) {
      i += 1;
      return matches!(self.peek(i).kind, TokenKind::LBrace);
    }

    // Case 3 try (nightly only)
    if matches!(self.peek(i).kind, TokenKind::KwTry) {
      i += 1;
      return matches!(self.peek(i).kind, TokenKind::LBrace);
    }

    // Case 4 plain block
    matches!(self.peek(i).kind, TokenKind::LBrace)
  }
}
