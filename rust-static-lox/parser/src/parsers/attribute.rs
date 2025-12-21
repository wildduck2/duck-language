use crate::{
  ast::{
    attrs::{AttrArgs, AttrInput, AttrStyle, Attribute},
    tokens::{Delimiter, TokenTree},
  },
  parser_utils::ExprContext,
  Parser,
};

use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_outer_attributes(&mut self) -> Result<Vec<Attribute>, ()> {
    let mut attr = vec![];
    while !self.is_eof() && matches!(self.current_token().kind, TokenKind::Pound) {
      attr.push(self.parse_outer_attribute()?);
    }
    Ok(attr)
  }

  pub(crate) fn parse_inner_attributes(&mut self) -> Result<Vec<Attribute>, ()> {
    let mut attr = vec![];
    while !self.is_eof() && matches!(self.current_token().kind, TokenKind::Pound) {
      attr.push(self.parse_inner_attribute()?);
    }
    Ok(attr)
  }

  fn parse_outer_attribute(&mut self) -> Result<Attribute, ()> {
    let mut start = self.current_token();

    self.expect(TokenKind::Pound)?;
    self.expect(TokenKind::OpenBracket)?;
    let input = self.parse_attribute_input()?;
    self.expect(TokenKind::CloseBracket)?;

    Ok(Attribute {
      style: AttrStyle::Outer,
      input,
      span: *start.span.merge(self.current_token().span),
    })
  }

  fn parse_inner_attribute(&mut self) -> Result<Attribute, ()> {
    let mut start = self.current_token();

    self.expect(TokenKind::Pound)?;
    self.expect(TokenKind::Bang)?;
    self.expect(TokenKind::OpenBracket)?;
    let input = self.parse_attribute_input()?;
    self.expect(TokenKind::CloseBracket)?;

    Ok(Attribute {
      style: AttrStyle::Inner,
      input,
      span: *start.span.merge(self.current_token().span),
    })
  }

  fn parse_attribute(&mut self) -> Result<Attribute, ()> {
    let mut start = self.current_token();

    let attr_style = match self.current_token().kind {
      TokenKind::Pound if self.peek(1).kind == TokenKind::Bang => {
        self.advance();
        self.advance();
        AttrStyle::Inner
      },
      TokenKind::Pound => {
        self.advance();
        AttrStyle::Outer
      },
      _ => {
        let offending = self.current_token();
        let lexeme = self.get_token_lexeme(&offending);
        self.emit(self.err_unexpected_token(offending.span, "`#` or `#!`", &lexeme));
        return Err(());
      },
    };

    self.expect(TokenKind::OpenBracket)?;
    let input = self.parse_attribute_input()?;
    self.expect(TokenKind::CloseBracket)?;

    Ok(Attribute {
      style: attr_style,
      input,
      span: *start.span.merge(self.current_token().span),
    })
  }

  // Parses attribute metadata like `derive(Debug)` or `path = value`.
  fn parse_attribute_input(&mut self) -> Result<AttrInput, ()> {
    // Kept as-is: parse_path(true) means "allow generic args" in your parser.
    let path = self.parse_path(true)?;
    let args = self.parse_attribute_input_tail()?;

    Ok(AttrInput { path, args })
  }

  // Parses the attribute tail: either '= expression' or a delimited token tree.
  fn parse_attribute_input_tail(&mut self) -> Result<Option<AttrArgs>, ()> {
    if self.current_token().kind == TokenKind::Eq {
      self.advance();

      // Matches grammar: attrInputTail -> "=" expression
      // NOTE: expression attrs and cfg eval happen later, parsing stays permissive.
      let expr = self.parse_expression(vec![], ExprContext::Default)?;
      return Ok(Some(AttrArgs::NameValue {
        value: Box::new(expr),
      }));
    }

    // Otherwise: attrInputTail -> delimTokenTree
    if matches!(
      self.current_token().kind,
      TokenKind::OpenParen | TokenKind::OpenBracket | TokenKind::OpenBrace
    ) {
      let tree = self.parse_delim_token_tree()?;
      return Ok(Some(AttrArgs::Delimited {
        delimiter: tree.delimiter(),
        tokens: tree.tokens(),
      }));
    }

    Ok(None)
  }

  // Recursively parses a delimited token tree into TokenTree.
  fn parse_delim_token_tree(&mut self) -> Result<TokenTree, ()> {
    let open = self.current_token();

    let delimiter = match open.kind {
      TokenKind::OpenParen => Delimiter::Paren,
      TokenKind::OpenBracket => Delimiter::Bracket,
      TokenKind::OpenBrace => Delimiter::Brace,
      _ => {
        let found = self.get_token_lexeme(&open);
        self.emit(self.err_unexpected_token(open.span, "delimiter start (`(`, `[`, or `{`)", &found));
        return Err(());
      },
    };

    self.advance();

    let mut tokens = Vec::new();

    while !self.is_eof() {
      let token = self.current_token();

      let is_close = matches!(
        (&token.kind, &delimiter),
        (TokenKind::CloseParen, Delimiter::Paren)
          | (TokenKind::CloseBracket, Delimiter::Bracket)
          | (TokenKind::CloseBrace, Delimiter::Brace)
      );

      if is_close {
        self.advance();
        break;
      }

      if matches!(
        token.kind,
        TokenKind::OpenParen | TokenKind::OpenBracket | TokenKind::OpenBrace
      ) {
        let nested = self.parse_delim_token_tree()?;
        tokens.push(nested);
        continue;
      }

      let lexeme = self.get_token_lexeme(&token);
      tokens.push(TokenTree::Token(lexeme));
      self.advance();
    }

    Ok(TokenTree::Delimited { delimiter, tokens })
  }
}

// Small helpers to avoid changing your TokenTree enum shape.
// If your TokenTree already has these variants, keep this impl.
// If not, adjust these to match your actual TokenTree definition.
impl TokenTree {
  fn delimiter(&self) -> Delimiter {
    match self {
      TokenTree::Delimited { delimiter, .. } => delimiter.clone(),
      _ => Delimiter::Paren,
    }
  }

  fn tokens(&self) -> Vec<TokenTree> {
    match self {
      TokenTree::Delimited { tokens, .. } => tokens.clone(),
      _ => vec![],
    }
  }
}
