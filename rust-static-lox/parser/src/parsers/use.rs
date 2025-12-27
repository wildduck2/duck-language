use lexer::token::TokenKind;

use crate::{
  ast::{Attribute, Ident, Item, UseDecl, UseTree, VisItem, VisItemKind, Visibility},
  match_and_consume,
  parser_utils::ParserContext,
  Parser,
};

impl Parser {
  pub(crate) fn parse_use_decl(
    &mut self,
    attributes: Vec<Attribute>,
    visibility: Visibility,
    context: ParserContext,
  ) -> Result<Item, ()> {
    let mut token = self.current_token();
    if !attributes.is_empty() {
      token.span.merge(self.current_token().span);
    }

    self.expect(TokenKind::KwUse)?;

    let tree = self.parse_use_tree(context)?;

    self.expect(TokenKind::Semi)?;

    Ok(Item::Vis(VisItem {
      attributes,
      visibility,
      kind: VisItemKind::Use(UseDecl { tree }),
      span: *token.span.merge(self.current_token().span),
    }))
  }

  fn parse_use_tree(&mut self, _context: ParserContext) -> Result<UseTree, ()> {
    let mut path: Vec<Ident> = vec![];

    while !self.is_eof()
      && !matches!(
        self.current_token().kind,
        TokenKind::Semi | TokenKind::Comma | TokenKind::RBrace | TokenKind::KwAs
      )
    {
      let name = self.parse_name(true)?;
      let ident = if name == "_" {
        Ident::Underscore
      } else {
        Ident::Name(name)
      };
      path.push(ident);
      match_and_consume!(self, TokenKind::ColonColon)?;
      if (matches!(self.current_token().kind, TokenKind::Ident)
        && !matches!(self.peek(1).kind, TokenKind::ColonColon))
        || matches!(
          self.current_token().kind,
          TokenKind::Star | TokenKind::LBrace
        )
      {
        break;
      }
    }

    let prefix = path
      .iter()
      .map(|s| s.as_str())
      .collect::<Vec<_>>()
      .join("::");

    println!("{:?}", self.get_token_lexeme(&self.current_token()));
    let tree = match self.current_token().kind {
      TokenKind::Ident => {
        let suffix = Box::new(UseTree::Name(self.get_token_lexeme(&self.current_token())));
        self.advance(); // consume the ident
        UseTree::Path { prefix, suffix }
      },
      TokenKind::Star => {
        self.advance(); // consume the '*'
        UseTree::Path {
          prefix,
          suffix: Box::new(UseTree::Glob),
        }
      },
      TokenKind::Semi | TokenKind::Comma | TokenKind::RBrace => UseTree::Name(prefix),
      TokenKind::KwAs => {
        self.advance(); // consume the "as"
        let alias = Ident::Name(self.parse_name(true)?);
        UseTree::Rename {
          name: prefix,
          alias,
        }
      },
      TokenKind::LBrace => {
        self.advance(); // consume '{'
        let mut list = vec![];
        while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::RBrace) {
          list.push(self.parse_use_tree(_context)?);
          match_and_consume!(self, TokenKind::Comma)?;
        }

        self.expect(TokenKind::RBrace)?;

        UseTree::Path {
          prefix,
          suffix: Box::new(UseTree::List(list)),
        }
      },

      _ => {
        let found = self.get_token_lexeme(&self.current_token());
        self.emit(self.err_unexpected_token(self.current_token().span, "use tree", &found));
        return Err(());
      },
    };
    Ok(tree)
  }
}
