use crate::{
  ast::{path::Path, Delimiter, Expr, ExprKind, MacroInvocation, RepeatKind, Stmt, TokenTree},
  match_and_consume,
  parser_utils::ParserContext,
  Parser,
};

use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::TokenKind;

// TODO: token tree parsing here is still a syntax only approximation.
// Right now it does not model full macro tt munching rules.
// Keep it as is until you implement full macro parsing.

impl Parser {
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
      span: *token.span.merge(self.current_token().span),
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
      span: *token.span.merge(self.current_token().span),
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
    let tokens = self.parse_macro_tokens()?;
    self.expect(close_kind)?;

    Ok(MacroInvocation {
      path,
      delimiter,
      tokens,
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_macro_tokens(&mut self) -> Result<Vec<TokenTree>, ()> {
    let mut tokens = vec![];

    while !self.is_eof()
      && !matches!(
        self.current_token().kind,
        TokenKind::RParen | TokenKind::RBracket | TokenKind::RBrace
      )
    {
      tokens.push(self.parse_token_tree()?);
      match_and_consume!(self, TokenKind::Comma)?;
    }

    Ok(tokens)
  }

  fn parse_token_tree(&mut self) -> Result<TokenTree, ()> {
    let token = self.current_token();

    match token.kind {
      TokenKind::Ident | TokenKind::Literal { .. } | TokenKind::KwTrue | TokenKind::KwFalse => {
        self.advance();
        Ok(TokenTree::Token(self.get_token_lexeme(&token)))
      },

      TokenKind::LParen | TokenKind::LBracket | TokenKind::LBrace => {
        let delimiter = match token.kind {
          TokenKind::LParen => Delimiter::Paren,
          TokenKind::LBracket => Delimiter::Bracket,
          TokenKind::LBrace => Delimiter::Brace,
          _ => unreachable!(),
        };

        self.advance(); // consume open
        let tokens = self.parse_macro_tokens()?;

        match delimiter {
          Delimiter::Paren => self.expect(TokenKind::RParen)?,
          Delimiter::Bracket => self.expect(TokenKind::RBracket)?,
          Delimiter::Brace => self.expect(TokenKind::RBrace)?,
        };

        Ok(TokenTree::Delimited { delimiter, tokens })
      },

      TokenKind::DotDot => {
        self.advance();

        let kind = match self.current_token().kind {
          TokenKind::DotDot => RepeatKind::ZeroOrMore,
          TokenKind::DotDotEq => RepeatKind::OneOrMore,
          TokenKind::Eq => RepeatKind::ZeroOrOne,
          _ => unreachable!(),
        };

        Ok(TokenTree::Repeat {
          tokens: vec![],
          separator: None,
          kind,
        })
      },

      _ => {
        let lexeme = self.get_token_lexeme(&token);
        let diagnostic = self
          .diagnostic(
            DiagnosticError::UnexpectedToken,
            format!("unexpected token `{lexeme}` in macro token tree"),
          )
          .with_label(
            token.span,
            Some("Expected a token-tree element".to_string()),
            LabelStyle::Primary,
          )
          .with_help("This macro parser is currently syntax only.".to_string());
        self.emit(diagnostic);
        Err(())
      },
    }
  }
}
