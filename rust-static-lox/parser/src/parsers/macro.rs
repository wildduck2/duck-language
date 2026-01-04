use crate::{
  ast::{
    path::Path, Attribute, Delimiter, Expr, ExprKind, Item, MacroInvocation, MacroItem,
    MacroItemKind, MacroRule, MacroRulesDecl, RepeatKind, Stmt, TokenTree, VisItem, VisItemKind,
    Visibility,
  },
  match_and_consume,
  parser_utils::ParserContext,
  Parser,
};

use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_macro_rules_decl(
    &mut self,
    attributes: Vec<Attribute>,
    visibility: Visibility,
    context: ParserContext,
  ) -> Result<Item, ()> {
    let mut token = self.current_token();
    if !attributes.is_empty() {
      token.span.merge(self.current_token().span);
    }

    self.expect(TokenKind::KwMacroRules)?;
    self.expect(TokenKind::Bang)?;

    let name = self.parse_name(true)?;
    let rules = self.parse_macro_rules()?;

    Ok(Item::Macro(MacroItem {
      attributes,
      visibility,
      kind: MacroItemKind::MacroRules(MacroRulesDecl { name, rules }),
      span: *token.span.merge(self.last_token_span()),
    }))
  }

  pub(crate) fn parse_macro_rules(&mut self) -> Result<Vec<MacroRule>, ()> {
    match self.current_token().kind {
      TokenKind::LBrace => {
        self.advance(); // consume '{'
        let mut rules = vec![];
        while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::RBrace) {
          rules.push(self.parse_macro_rule()?);
          match_and_consume!(self, TokenKind::Semi)?;
        }
        self.expect(TokenKind::RBrace)?;
        Ok(rules)
      },
      _ => {
        let token = self.current_token();
        let lexeme = self.get_token_lexeme(&token);
        self.emit(self.err_unexpected_token(token.span, "`{`", &lexeme));
        Err(())
      },
    }
  }

  fn parse_macro_rule(&mut self) -> Result<MacroRule, ()> {
    let matcher = self.parse_macro_matcher()?;
    self.expect(TokenKind::FatArrow)?;
    let transcriber = self.parse_macro_transcriber()?;

    Ok(MacroRule {
      matcher,
      transcriber,
    })
  }

  fn parse_macro_transcriber(&mut self) -> Result<TokenTree, ()> {
    self.parse_delim_token_tree()
  }

  fn parse_macro_matcher(&mut self) -> Result<TokenTree, ()> {
    match self.current_token().kind {
      TokenKind::LParen => {
        self.advance(); // consume '('
        let mut tokens = vec![];
        while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::RParen) {
          tokens.push(self.parse_macro_match()?);
        }
        self.expect(TokenKind::RParen)?;
        Ok(TokenTree::Delimited {
          delimiter: Delimiter::Paren,
          tokens,
        })
      },
      _ => {
        let token = self.current_token();
        let lexeme = self.get_token_lexeme(&token);
        self.emit(self.err_unexpected_token(token.span, "`(`", &lexeme));
        Err(())
      },
    }
  }

  fn parse_macro_match(&mut self) -> Result<TokenTree, ()> {
    match self.current_token().kind {
      TokenKind::Dollar => {
        self.advance();
        let name = self.parse_name(false)?;
        self.expect(TokenKind::Colon)?;
        let frag = self.parse_name(false)?;
        Ok(TokenTree::MetaVar { name, frag })
      },
      TokenKind::Comma => {
        println!("{:?}", self.current_token().kind);
        self.advance();
        Ok(TokenTree::Token(",".to_string()))
      },
      _ => Err(()),
    }
  }

  fn parse_macro_frag_spec(&mut self, context: ParserContext) -> Result<TokenTree, ()> {
    let mut token = self.current_token();

    match self.current_token().kind {
      TokenKind::Ident => {
        self.advance();
        Ok(TokenTree::Token(self.get_token_lexeme(&token)))
      },
      _ => Err(()),
    }
  }

  pub(crate) fn parse_macro_invocation_statement(
    &mut self,
    context: ParserContext,
  ) -> Result<Stmt, ()> {
    let mut token = self.current_token();

    let expr = self.parse_macro_invocation_expression(context)?;
    let mac = match expr.kind {
      ExprKind::Macro { mac } => mac,
      _ => unreachable!(),
    };

    Ok(Stmt::Macro {
      mac,
      span: *token.span.merge(self.last_token_span()),
    })
  }

  pub(crate) fn parse_macro_invocation_expression(
    &mut self,
    context: ParserContext,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();

    let path = self.parse_path(true, context)?;

    self.expect(TokenKind::Bang)?;
    let mac = self.parse_macro_invocation(path)?;

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Macro { mac },
      span: *token.span.merge(self.last_token_span()),
    })
  }

  pub(crate) fn parse_macro_invocation(&mut self, path: Path) -> Result<MacroInvocation, ()> {
    let mut token = self.current_token();

    let (open_kind, delimiter, close_kind) = match token.kind {
      TokenKind::LParen => (TokenKind::LParen, Delimiter::Paren, TokenKind::RParen),
      TokenKind::LBracket => (TokenKind::LBracket, Delimiter::Bracket, TokenKind::RBracket),
      TokenKind::LBrace => (TokenKind::LBrace, Delimiter::Brace, TokenKind::RBrace),
      _ => {
        let lexeme = self.get_token_lexeme(&token);
        let diagnostic = self
          .diagnostic(
            DiagnosticError::UnexpectedToken,
            format!("unexpected token `{lexeme}` in macro invocation"),
          )
          .with_label(
            token.span,
            Some("Expected `(`, `[` or `{` to start macro arguments".to_string()),
            LabelStyle::Primary,
          )
          .with_help("Macro invocations must be followed by a delimited token tree.".to_string());
        self.emit(diagnostic);
        return Err(());
      },
    };

    self.expect(open_kind)?;
    let tokens = self.parse_macro_delim_token_tree()?;
    self.expect(close_kind)?;

    Ok(MacroInvocation {
      path,
      delimiter,
      tokens,
      span: *token.span.merge(self.last_token_span()),
    })
  }

  fn parse_macro_delim_token_tree(&mut self) -> Result<Vec<TokenTree>, ()> {
    let mut tokens = vec![];

    while !self.is_eof()
      && !matches!(
        self.current_token().kind,
        TokenKind::RParen | TokenKind::RBracket | TokenKind::RBrace
      )
    {
      tokens.push(self.parse_delim_token_tree()?);
      match_and_consume!(self, TokenKind::Comma)?;
    }

    Ok(tokens)
  }

  pub(crate) fn parse_delim_token_tree(&mut self) -> Result<TokenTree, ()> {
    let open = self.current_token();

    let delimiter = match open.kind {
      TokenKind::LParen => Delimiter::Paren,
      TokenKind::LBracket => Delimiter::Bracket,
      TokenKind::LBrace => Delimiter::Brace,
      _ => {
        let found = self.get_token_lexeme(&open);
        self.emit(self.err_unexpected_token(
          open.span,
          "delimiter start (`(`, `[`, or `{`)",
          &found,
        ));
        return Err(());
      },
    };

    self.advance();

    let mut tokens = Vec::new();

    while !self.is_eof() {
      let token = self.current_token();

      let is_close = matches!(
        (&token.kind, &delimiter),
        (TokenKind::RParen, Delimiter::Paren)
          | (TokenKind::RBracket, Delimiter::Bracket)
          | (TokenKind::RBrace, Delimiter::Brace)
      );

      if is_close {
        self.advance();
        break;
      }

      if matches!(
        token.kind,
        TokenKind::LParen | TokenKind::LBracket | TokenKind::LBrace
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
