use crate::{
  ast::{
    path::Path, Attribute, Delimiter, Expr, MacroInvocation, QSelfHeader, RepeatKind, Stmt,
    TokenTree, Type,
  },
  match_and_consume,
  parser_utils::ExprContext,
  Parser,
};

use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
  DiagnosticEngine,
};
use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_macro_invocation_statement(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Stmt, ()> {
    let mut token = self.current_token();
    let mac = match self.parser_macro_invocation_expression(engine)? {
      Expr::Macro { mac, .. } => mac,
      _ => unreachable!(),
    };

    Ok(Stmt::Macro {
      mac,
      span: *token.span.merge(self.current_token().span),
    })
  }

  pub(crate) fn parser_macro_invocation_expression(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();
    let qself_header = self.parse_qself_type_header(engine)?;
    let mut path = self.parse_path(true, engine)?;

    let (qself, path) = match qself_header {
      Some(QSelfHeader { self_ty, trait_ref }) => match trait_ref {
        Some(mut trait_ref) => {
          trait_ref.segments.extend(path.segments);
          path.leading_colon = trait_ref.leading_colon;
          (Some(self_ty), trait_ref)
        },
        None => (Some(self_ty), path),
      },
      None => (None, path),
    };

    self.expect(TokenKind::Bang, engine)?;
    let mac = self.parse_macro_invocation(path, qself, engine)?;

    token.span.merge(self.current_token().span);

    Ok(Expr::Macro {
      mac,
      span: *token.span.merge(self.current_token().span),
    })
  }

  // TODO: implement full macro expansion
  pub(crate) fn parse_macro_invocation(
    &mut self,
    path: Path,
    qself: Option<Box<Type>>,
    engine: &mut DiagnosticEngine,
  ) -> Result<MacroInvocation, ()> {
    let mut token = self.current_token();

    match token.kind {
      TokenKind::OpenParen | TokenKind::OpenBracket | TokenKind::OpenBrace => {
        self.advance(engine); // consume the '('
        let tokens = self.parse_macro_tokens(engine)?;

        let delimiter = match token.kind {
          TokenKind::OpenParen => {
            self.expect(TokenKind::CloseParen, engine)?;
            Delimiter::Paren
          },
          TokenKind::OpenBracket => {
            self.expect(TokenKind::CloseBracket, engine)?;
            Delimiter::Bracket
          },
          TokenKind::OpenBrace => {
            self.expect(TokenKind::CloseBrace, engine)?;
            Delimiter::Brace
          },
          _ => unreachable!(),
        };

        Ok(MacroInvocation {
          qself,
          path,
          delimiter,
          tokens,
          span: *token.span.merge(self.current_token().span),
        })
      },

      _ => {
        let lexeme = self.get_token_lexeme(&token);
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
          format!("Unexpected token `{lexeme}` in macro invocation"),
          self.source_file.path.clone(),
        )
        .with_label(
          token.span,
          Some("Expected a macro invocation, found a primary expression".to_string()),
          LabelStyle::Primary,
        )
        .with_help("Macro invocations must be surrounded by parentheses or braces.".to_string());
        engine.add(diagnostic);
        Err(())
      },
    }
  }

  fn parse_macro_tokens(&mut self, engine: &mut DiagnosticEngine) -> Result<Vec<TokenTree>, ()> {
    let mut tokens = vec![];

    while !self.is_eof()
      && !matches!(
        self.current_token().kind,
        TokenKind::CloseParen | TokenKind::CloseBracket | TokenKind::CloseBrace
      )
    {
      tokens.push(self.parse_token_tree(engine)?);
      match_and_consume!(self, engine, TokenKind::Comma)?;
    }

    Ok(tokens)
  }

  fn parse_token_tree(&mut self, engine: &mut DiagnosticEngine) -> Result<TokenTree, ()> {
    let mut token = self.current_token();
    match token.kind {
      TokenKind::Ident | TokenKind::Literal { .. } | TokenKind::KwTrue | TokenKind::KwFalse => {
        self.advance(engine);
        Ok(TokenTree::Token(self.get_token_lexeme(&token)))
      },

      // FIX: this swhen you get to the macro full parsing
      TokenKind::OpenParen | TokenKind::OpenBracket | TokenKind::OpenBrace => {
        let tokens = self.parse_macro_tokens(engine)?;
        self.expect(TokenKind::CloseParen, engine)?;
        Ok(TokenTree::Delimited {
          delimiter: match token.kind {
            TokenKind::OpenParen => Delimiter::Paren,
            TokenKind::OpenBracket => Delimiter::Bracket,
            TokenKind::OpenBrace => Delimiter::Brace,
            _ => unreachable!(),
          },
          tokens,
        })
      },

      // FIX: this swhen you get to the macro full parsing
      TokenKind::DotDot => {
        self.advance(engine);

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

      // FIX: this swhen you get to the macro full parsing
      TokenKind::OpenBrace => {
        self.expect(TokenKind::CloseBrace, engine)?;
        Ok(TokenTree::MetaVar {
          name: self.get_token_lexeme(&token),
          kind: self.get_token_lexeme(&token),
        })
      },

      _ => {
        let lexeme = self.get_token_lexeme(&token);
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
          format!("Unexpected token `{lexeme}` in macro invocation"),
          self.source_file.path.clone(),
        )
        .with_label(
          token.span,
          Some("Expected a macro invocation, found a primary expression".to_string()),
          LabelStyle::Primary,
        )
        .with_help("Macro invocations must be surrounded by parentheses or braces.".to_string());
        engine.add(diagnostic);
        Err(())
      },
    }
  }
}
