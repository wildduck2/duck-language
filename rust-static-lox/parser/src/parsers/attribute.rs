use crate::{
  ast::{
    attrs::{AttrArgs, AttrInput, AttrStyle, Attribute},
    tokens::{Delimiter, TokenTree},
  },
  parser_utils::ParserContext,
  Parser,
};

use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_outer_attributes(
    &mut self,
    context: ParserContext,
  ) -> Result<Vec<Attribute>, ()> {
    let mut attr = vec![];
    while !self.is_eof() && matches!(self.current_token().kind, TokenKind::Pound) {
      attr.push(self.parse_outer_attribute(context)?);
    }
    Ok(attr)
  }

  pub(crate) fn parse_inner_attributes(
    &mut self,
    context: ParserContext,
  ) -> Result<Vec<Attribute>, ()> {
    let mut attr = vec![];
    while !self.is_eof()
      && matches!(self.current_token().kind, TokenKind::Pound)
      && matches!(self.peek(1).kind, TokenKind::Bang)
    {
      attr.push(self.parse_inner_attribute(context)?);
    }
    Ok(attr)
  }

  fn parse_outer_attribute(&mut self, context: ParserContext) -> Result<Attribute, ()> {
    let mut start = self.current_token();

    self.expect(TokenKind::Pound)?;
    self.expect(TokenKind::LBracket)?;
    let input = self.parse_attribute_input(context)?;
    self.expect(TokenKind::RBracket)?;

    Ok(Attribute {
      style: AttrStyle::Outer,
      input,
      span: *start.span.merge(self.last_token_span()),
    })
  }

  fn parse_inner_attribute(&mut self, context: ParserContext) -> Result<Attribute, ()> {
    let mut start = self.current_token();

    self.expect(TokenKind::Pound)?;
    self.expect(TokenKind::Bang)?;
    self.expect(TokenKind::LBracket)?;
    let input = self.parse_attribute_input(context)?;
    self.expect(TokenKind::RBracket)?;

    Ok(Attribute {
      style: AttrStyle::Inner,
      input,
      span: *start.span.merge(self.last_token_span()),
    })
  }

  pub(crate) fn parse_attribute(&mut self, context: ParserContext) -> Result<Attribute, ()> {
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

    self.expect(TokenKind::LBracket)?;
    let input = self.parse_attribute_input(context)?;
    self.expect(TokenKind::RBracket)?;

    Ok(Attribute {
      style: attr_style,
      input,
      span: *start.span.merge(self.last_token_span()),
    })
  }

  // Parses attribute metadata like `derive(Debug)` or `path = value`.
  fn parse_attribute_input(&mut self, context: ParserContext) -> Result<AttrInput, ()> {
    // Kept as-is: parse_path(true) means "allow generic args" in your parser.
    let path = self.parse_path(true, context)?;
    let args = self.parse_attribute_input_tail()?;

    Ok(AttrInput { path, args })
  }

  // Parses the attribute tail: either '= expression' or a delimited token tree.
  fn parse_attribute_input_tail(&mut self) -> Result<Option<AttrArgs>, ()> {
    if self.current_token().kind == TokenKind::Eq {
      self.advance();

      // Matches grammar: attrInputTail -> "=" expression
      // NOTE: expression attrs and cfg eval happen later, parsing stays permissive.
      let expr = self.parse_expression(vec![], ParserContext::Default)?;
      return Ok(Some(AttrArgs::NameValue {
        value: Box::new(expr),
      }));
    }

    // Otherwise: attrInputTail -> delimTokenTree
    if matches!(
      self.current_token().kind,
      TokenKind::LParen | TokenKind::LBracket | TokenKind::LBrace
    ) {
      let tree = self.parse_delim_token_tree()?;
      return Ok(Some(AttrArgs::Delimited {
        delimiter: tree.delimiter(),
        tokens: tree.tokens(),
      }));
    }

    Ok(None)
  }
}

impl TokenTree {
  pub(crate) fn delimiter(&self) -> Delimiter {
    match self {
      TokenTree::Delimited { delimiter, .. } => delimiter.clone(),
      _ => Delimiter::Paren,
    }
  }

  pub(crate) fn tokens(&self) -> Vec<TokenTree> {
    match self {
      TokenTree::Delimited { tokens, .. } => tokens.clone(),
      _ => vec![],
    }
  }
}
