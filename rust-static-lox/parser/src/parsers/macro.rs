use crate::{
  ast::{
    path::Path, Attribute, Delimiter, Item, Macro2Decl, MacroInvocation, MacroItem, MacroItemKind,
    MacroRule, MacroRulesDecl, RepeatKind, Stmt, TokenTree, Visibility,
  },
  match_and_consume,
  parser_utils::ParserContext,
  Parser,
};

use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_macro_decl(
    &mut self,
    attributes: Vec<Attribute>,
    visibility: Visibility,
    _context: ParserContext,
  ) -> Result<Item, ()> {
    let mut token = self.current_token();
    if !attributes.is_empty() {
      token.span.merge(self.current_token().span);
    }

    self.expect(TokenKind::KwMacro)?;
    let name = self.parse_name(false)?;
    self.expect(TokenKind::LParen)?;
    let params = self.parse_macro_params()?;
    self.expect(TokenKind::RParen)?;
    let body = self.parse_delim_token_tree()?;

    Ok(Item::Macro(MacroItem {
      attributes,
      visibility,
      kind: MacroItemKind::Macro2(Macro2Decl { name, params, body }),
      span: *token.span.merge(self.last_token_span()),
    }))
  }

  fn parse_macro_params(&mut self) -> Result<Vec<String>, ()> {
    // macroParams: IDENT ("," IDENT)* ","?
    let mut params = Vec::new();
    if matches!(self.current_token().kind, TokenKind::RParen) {
      return Ok(params);
    }

    loop {
      let param = self.parse_name(false)?;
      params.push(param.as_str().to_string());

      if matches!(self.current_token().kind, TokenKind::Comma) {
        self.advance(); // consume ','
        if matches!(self.current_token().kind, TokenKind::RParen) {
          break;
        }
        continue;
      }

      break;
    }

    Ok(params)
  }

  pub(crate) fn parse_macro_rules_decl(
    &mut self,
    attributes: Vec<Attribute>,
    visibility: Visibility,
    _context: ParserContext,
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
    let (close_kind, needs_semi) = match self.current_token().kind {
      TokenKind::LBrace => (TokenKind::RBrace, false),
      TokenKind::LBracket => (TokenKind::RBracket, false),
      TokenKind::LParen => (TokenKind::RParen, true),
      _ => {
        let token = self.current_token();
        let lexeme = self.get_token_lexeme(&token);
        let diagnostic = self
          .diagnostic(
            DiagnosticError::UnexpectedToken,
            format!("expected `{{`, `[`, or `(`, found `{lexeme}`"),
          )
          .with_label(
            token.span,
            Some("expected `{{`, `[`, or `(` here".to_string()),
            LabelStyle::Primary,
          )
          .with_note(format!("unexpected token: `{lexeme}`"))
          .with_help("use a valid macro_rules! body delimiter".to_string());
        self.emit(diagnostic);
        return Err(());
      },
    };

    // macro_rules! bodies use {}, [], or () + trailing ';' (paren form).
    self.advance(); // consume opening delimiter
    let mut rules = vec![];
    while !self.is_eof() && self.current_token().kind != close_kind {
      rules.push(self.parse_macro_rule()?);
      match_and_consume!(self, TokenKind::Semi)?;
    }
    self.expect(close_kind)?;
    if needs_semi {
      self.expect(TokenKind::Semi)?;
    }

    Ok(rules)
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
    let (open_kind, close_kind, delimiter) = match self.current_token().kind {
      TokenKind::LParen => (TokenKind::LParen, TokenKind::RParen, Delimiter::Paren),
      TokenKind::LBracket => (TokenKind::LBracket, TokenKind::RBracket, Delimiter::Bracket),
      TokenKind::LBrace => (TokenKind::LBrace, TokenKind::RBrace, Delimiter::Brace),
      _ => {
        let token = self.current_token();
        let lexeme = self.get_token_lexeme(&token);
        let diagnostic = self
          .diagnostic(
            DiagnosticError::UnexpectedToken,
            format!("expected `(`, `[`, or `{{`, found `{lexeme}`"),
          )
          .with_label(
            token.span,
            Some("expected `(`, `[`, or `{{` here".to_string()),
            LabelStyle::Primary,
          )
          .with_note(format!("unexpected token: `{lexeme}`"))
          .with_help("start a macro matcher with a delimiter".to_string());
        self.emit(diagnostic);
        return Err(());
      },
    };

    // Matchers are delimited token trees that may contain `$` metavars and repetitions.
    self.expect(open_kind)?;
    let mut tokens = vec![];
    while !self.is_eof() && self.current_token().kind != close_kind {
      tokens.push(self.parse_macro_match()?);
    }
    self.expect(close_kind)?;

    Ok(TokenTree::Delimited { delimiter, tokens })
  }

  fn parse_macro_match(&mut self) -> Result<TokenTree, ()> {
    match self.current_token().kind {
      TokenKind::Dollar => self.parse_macro_match_dollar(),
      TokenKind::LParen | TokenKind::LBracket | TokenKind::LBrace => self.parse_macro_matcher(),
      TokenKind::RParen | TokenKind::RBracket | TokenKind::RBrace => {
        let token = self.current_token();
        let lexeme = self.get_token_lexeme(&token);
        let diagnostic = self
          .diagnostic(
            DiagnosticError::UnexpectedToken,
            format!("expected `macro matcher token`, found `{lexeme}`"),
          )
          .with_label(
            token.span,
            Some("expected `macro matcher token` here".to_string()),
            LabelStyle::Primary,
          )
          .with_note(format!("unexpected token: `{lexeme}`"))
          .with_help("remove the stray closing delimiter or add a matcher token".to_string());
        self.emit(diagnostic);
        Err(())
      },
      _ => {
        let token = self.current_token();
        let lexeme = self.get_token_lexeme(&token);
        let kind = token.kind;
        self.advance();
        Ok(TokenTree::Token { kind, lexeme })
      },
    }
  }

  fn parse_macro_match_dollar(&mut self) -> Result<TokenTree, ()> {
    self.expect(TokenKind::Dollar)?;

    match self.current_token().kind {
      // $name:frag
      TokenKind::Ident => {
        let name = self.parse_name(false)?;
        self.expect(TokenKind::Colon)?;
        let frag = self.parse_macro_frag_spec()?;
        Ok(TokenTree::MetaVar { name, frag })
      },

      // $( ... ) separator? rep_op
      TokenKind::LParen => self.parse_macro_repetition(),

      _ => {
        let token = self.current_token();
        let lexeme = self.get_token_lexeme(&token);
        let diagnostic = self
          .diagnostic(
            DiagnosticError::UnexpectedToken,
            format!("expected `macro matcher`, found `{lexeme}`"),
          )
          .with_label(
            token.span,
            Some("expected `macro matcher` here".to_string()),
            LabelStyle::Primary,
          )
          .with_note(format!("unexpected token: `{lexeme}`"))
          .with_help("use `$name:fragment` or `$(...)` repetition syntax".to_string());
        self.emit(diagnostic);
        Err(())
      },
    }
  }

  fn parse_macro_repetition(&mut self) -> Result<TokenTree, ()> {
    self.expect(TokenKind::LParen)?;

    if matches!(self.current_token().kind, TokenKind::RParen) {
      let token = self.current_token();
      let lexeme = self.get_token_lexeme(&token);
      let diagnostic = self
        .diagnostic(
          DiagnosticError::UnexpectedToken,
          format!("expected `macro matcher`, found `{lexeme}`"),
        )
        .with_label(
          token.span,
          Some("expected `macro matcher` here".to_string()),
          LabelStyle::Primary,
        )
        .with_note(format!("unexpected token: `{lexeme}`"))
        .with_help("add a matcher token inside the repetition".to_string());
      self.emit(diagnostic);
      return Err(());
    }

    let mut tokens = vec![];
    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::RParen) {
      tokens.push(self.parse_macro_match()?);
    }
    self.expect(TokenKind::RParen)?;

    let (separator, kind) = self.parse_macro_repetition_op()?;
    Ok(TokenTree::Repeat {
      tokens,
      separator,
      kind,
    })
  }

  fn parse_macro_repetition_op(&mut self) -> Result<(Option<String>, RepeatKind), ()> {
    // If the next token is a repetition operator, there's no separator.
    let direct_op = match self.current_token().kind {
      TokenKind::Star => Some(RepeatKind::ZeroOrMore),
      TokenKind::Plus => Some(RepeatKind::OneOrMore),
      TokenKind::Question => Some(RepeatKind::ZeroOrOne),
      _ => None,
    };

    if let Some(kind) = direct_op {
      self.advance();
      return Ok((None, kind));
    }

    // Otherwise treat the next token as a separator and require a repetition op after it.
    let separator_token = self.current_token();
    if matches!(
      separator_token.kind,
      TokenKind::LParen
        | TokenKind::LBracket
        | TokenKind::LBrace
        | TokenKind::RParen
        | TokenKind::RBracket
        | TokenKind::RBrace
    ) {
      let lexeme = self.get_token_lexeme(&separator_token);
      let diagnostic = self
        .diagnostic(
          DiagnosticError::UnexpectedToken,
          format!("expected `macro repetition operator`, found `{lexeme}`"),
        )
        .with_label(
          separator_token.span,
          Some("expected `macro repetition operator` here".to_string()),
          LabelStyle::Primary,
        )
        .with_note(format!("unexpected token: `{lexeme}`"))
        .with_help("use `*`, `+`, or `?` to end a repetition".to_string());
      self.emit(diagnostic);
      return Err(());
    }

    let separator = self.get_token_lexeme(&separator_token);
    self.advance();

    let kind = match self.current_token().kind {
      TokenKind::Star => RepeatKind::ZeroOrMore,
      TokenKind::Plus => RepeatKind::OneOrMore,
      TokenKind::Question => RepeatKind::ZeroOrOne,
      _ => {
        let token = self.current_token();
        let lexeme = self.get_token_lexeme(&token);
        let diagnostic = self
          .diagnostic(
            DiagnosticError::UnexpectedToken,
            format!("expected `macro repetition operator`, found `{lexeme}`"),
          )
          .with_label(
            token.span,
            Some("expected `macro repetition operator` here".to_string()),
            LabelStyle::Primary,
          )
          .with_note(format!("unexpected token: `{lexeme}`"))
          .with_help("use `*`, `+`, or `?` after the separator".to_string());
        self.emit(diagnostic);
        return Err(());
      },
    };
    self.advance();

    Ok((Some(separator), kind))
  }

  fn parse_macro_frag_spec(&mut self) -> Result<crate::ast::Ident, ()> {
    let token = self.current_token();
    let frag = self.parse_name(false)?;
    let frag_name = frag.as_str();

    let is_valid = matches!(
      frag_name,
      "block"
        | "expr"
        | "ident"
        | "item"
        | "lifetime"
        | "literal"
        | "meta"
        | "pat"
        | "pat_param"
        | "path"
        | "stmt"
        | "tt"
        | "ty"
        | "vis"
    );

    if !is_valid {
      let diagnostic = self
        .diagnostic(
          DiagnosticError::UnexpectedToken,
          format!("expected `macro fragment specifier`, found `{frag_name}`"),
        )
        .with_label(
          token.span,
          Some("expected `macro fragment specifier` here".to_string()),
          LabelStyle::Primary,
        )
        .with_note(format!("unexpected token: `{frag_name}`"))
        .with_help("use a valid fragment specifier like `expr`, `ident`, or `ty`".to_string());
      self.emit(diagnostic);
      return Err(());
    }

    Ok(frag)
  }

  pub(crate) fn parse_macro_invocation_item(
    &mut self,
    attributes: Vec<Attribute>,
    visibility: Visibility,
    context: ParserContext,
  ) -> Result<Item, ()> {
    let mut token = self.current_token();
    if !attributes.is_empty() {
      token.span.merge(self.current_token().span);
    }

    let path = self.parse_path(true, context)?;
    self.expect(TokenKind::Bang)?;
    let mac = self.parse_macro_invocation(path)?;
    match_and_consume!(self, TokenKind::Semi)?;

    Ok(Item::Macro(MacroItem {
      attributes,
      visibility,
      kind: MacroItemKind::Invocation(mac),
      span: *token.span.merge(self.last_token_span()),
    }))
  }

  pub(crate) fn parse_macro_invocation_statement(
    &mut self,
    context: ParserContext,
  ) -> Result<Stmt, ()> {
    let mut token = self.current_token();

    let path = self.parse_path(true, context)?;
    self.expect(TokenKind::Bang)?;
    let mac = self.parse_macro_invocation(path)?;
    match_and_consume!(self, TokenKind::Semi)?;

    Ok(Stmt::Macro {
      mac,
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
        let expected = "a delimiter start (`(`, `[`, or `{`)";
        let diagnostic = self
          .diagnostic(
            DiagnosticError::UnexpectedToken,
            format!("expected macro arguments, found `{lexeme}`"),
          )
          .with_label(
            token.span,
            Some(format!("expected {expected} to start macro arguments")),
            LabelStyle::Primary,
          )
          .with_help("macro invocations must be followed by a delimited token tree".to_string())
          .with_note("examples: `foo!(...)`, `foo![...]`, `foo!{...}`".to_string());
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
      if matches!(
        self.current_token().kind,
        TokenKind::LParen | TokenKind::LBracket | TokenKind::LBrace
      ) {
        // Nested delimited tree inside a macro invocation.
        tokens.push(self.parse_delim_token_tree()?);
        match_and_consume!(self, TokenKind::Comma)?;
        continue;
      }

      // Accept any other token as a raw token tree element.
      let token = self.current_token();
      let lexeme = self.get_token_lexeme(&token);
      let kind = token.kind;
      tokens.push(TokenTree::Token { kind, lexeme });
      self.advance();
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
        let diagnostic = self
          .diagnostic(
            DiagnosticError::UnexpectedToken,
            format!("expected `delimiter start (`(`, `[`, or `{{`)`, found `{found}`"),
          )
          .with_label(
            open.span,
            Some("expected `delimiter start (`(`, `[`, or `{{`)` here".to_string()),
            LabelStyle::Primary,
          )
          .with_note(format!("unexpected token: `{found}`"))
          .with_help("start a delimited token tree with `(`, `[`, or `{`".to_string());
        self.emit(diagnostic);
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
      tokens.push(TokenTree::Token {
        kind: token.kind,
        lexeme,
      });
      self.advance();
    }

    Ok(TokenTree::Delimited { delimiter, tokens })
  }
}
