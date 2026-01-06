use std::{cell::RefCell, rc::Rc};

use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
  DiagnosticEngine, SourceFile, Span,
};
use lexer::token::{Token, TokenKind};

use crate::ast::Item;

pub mod ast;
mod decoder;
mod parser_utils;
mod parsers;
#[cfg(test)]
mod tests;

#[macro_use]
mod macros;

/// Recursive-descent parser that transforms tokens into an AST while reporting diagnostics.
pub struct Parser {
  pub tokens: Vec<Token>,
  pub ast: Vec<Item>,
  pub current: usize,
  pub source_file: SourceFile,
  pub engine: Rc<RefCell<DiagnosticEngine>>,
}

impl Parser {
  /// Creates a parser seeded with the lexer output.
  pub fn new(
    tokens: Vec<Token>,
    source_file: SourceFile,
    engine: Rc<RefCell<DiagnosticEngine>>,
  ) -> Self {
    if tokens.is_empty() {
      // Parser always expects at least an EOF sentinel, bail early otherwise.
      panic!("Parser::new: tokens is empty");
    }

    Self {
      tokens,
      ast: Vec::new(),
      current: 0,
      source_file,
      engine,
    }
  }

  /// Parses the entire token stream, accumulating AST nodes and diagnostics.
  pub fn parse(&mut self) {
    // Delegate to the grammar entry point defined in `parser_utils`.
    self.parse_program()
  }

  // Create a parser diagnostic pre-populated with the current file path.
  pub(crate) fn diagnostic(&self, code: DiagnosticError, message: impl Into<String>) -> Diagnostic {
    Diagnostic::new(
      DiagnosticCode::Error(code),
      message.into(),
      self.source_file.path.clone(),
    )
  }

  // Emit a diagnostic through the parser's engine.
  pub(crate) fn emit(&mut self, diagnostic: Diagnostic) {
    self.engine.borrow_mut().add(diagnostic);
  }

  /// Returns the token at the current cursor position.
  fn current_token(&self) -> Token {
    self.tokens[self.current].clone()
  }

  /// Peeks one token ahead without advancing.
  fn peek(&self, n: usize) -> Token {
    let idx = self.current + n;

    if idx >= self.tokens.len() {
      return Token {
        kind: TokenKind::Eof,
        span: Span::new(0, 0),
      };
    }

    self.tokens[idx].clone()
  }

  /// Returns the token `n` positions behind the cursor without rewinding.
  /// Useful for diagnostics that need to point at the most recently consumed lexeme.
  fn peek_prev(&self, n: usize) -> Token {
    let index = self.current.saturating_sub(n + 1);
    self.tokens[index].clone()
  }

  /// Returns the span of the most recently consumed token.
  fn last_token_span(&self) -> Span {
    self.peek_prev(0).span
  }

  /// Advances to the next token, emitting an unterminated-string diagnostic if we passed EOF.
  fn advance(&mut self) {
    if self.is_eof() {
      let current_token = self.current_token();
      let diagnostic = self
        .diagnostic(
          DiagnosticError::UnterminatedString,
          "unterminated string literal",
        )
        .with_label(
          current_token.span,
          Some("string literal is not terminated".to_string()),
          LabelStyle::Primary,
        )
        .with_help("add a closing quote to terminate the string literal".to_string());
      self.emit(diagnostic);
      return;
    }

    // Consume the token successfully.
    self.current += 1;
  }

  /// Reports whether the cursor points at the synthetic EOF token.
  fn is_eof(&self) -> bool {
    self.current == (self.tokens.len() - 1)
  }

  /// Function that consume the code until there's valid tokens to start a new expr
  pub(crate) fn synchronize(&mut self) {
    while !self.is_eof() {
      match self.current_token().kind {
        TokenKind::Semi => {
          // Stop skipping once we hit a statement boundary.
          self.advance();
          break;
        },
        _ => {
          // Otherwise keep discarding tokens until we reach a safe point.
          self.advance();
        },
      }
    }
  }

  /// Expects a specific token type and provides detailed error diagnostics if not found
  fn expect(&mut self, expected: TokenKind) -> Result<Token, ()> {
    if self.is_eof() {
      // Reached EOF before finding the expected token.
      self.error_expected_token_eof(expected);
      return Err(());
    }

    let current = self.current_token();

    if current.kind == expected {
      // Consume and return the matching token.
      self.advance();
      Ok(current)
    } else {
      // Emit a detailed diagnostic and leave recovery to the caller.
      self.error_expected_token(expected, current);
      Err(())
    }
  }

  /// Error for when we expect a token but hit EOF
  fn error_expected_token_eof(&mut self, expected: TokenKind) {
    let token = self.current_token();
    let expected_str = format!("{expected:?}");
    let diagnostic = self
      .diagnostic(
        DiagnosticError::UnexpectedToken,
        format!("expected `{expected_str}`, found `end of file`"),
      )
      .with_label(
        token.span,
        Some(format!("expected `{expected_str}` here")),
        LabelStyle::Primary,
      )
      .with_note("unexpected token: `end of file`".to_string())
      .with_help(format!(
        "use `{expected_str}` here or adjust the surrounding syntax"
      ));
    self.emit(diagnostic);
  }

  /// Error for when we expect a token but find something else
  fn error_expected_token(&mut self, expected: TokenKind, found: Token) {
    let current_token = self.current_token();
    let lexeme = self
      .source_file
      .src
      .get(current_token.span.start..current_token.span.end)
      .unwrap();
    let expected_str = format!("{expected:?}");
    let mut diag = self
      .diagnostic(
        DiagnosticError::UnexpectedToken,
        format!("expected `{expected_str}`, found `{lexeme}`"),
      )
      .with_label(
        current_token.span,
        Some(format!("expected `{expected_str}` here")),
        LabelStyle::Primary,
      )
      .with_note(format!("unexpected token: `{lexeme}`"))
      .with_help(format!(
        "use `{expected_str}` here or adjust the surrounding syntax"
      ));

    let token_help = Parser::get_token_help(&expected, &found);
    if !token_help.is_empty() {
      diag = diag.with_help(token_help);
    }
    self.emit(diag);
  }

  /// Provides contextual help based on what was expected vs found
  fn get_token_help(expected: &TokenKind, found: &Token) -> String {
    match (expected, &found.kind) {
      (TokenKind::Semi, _) => "Statements must end with a semicolon".to_string(),
      (TokenKind::LParen, TokenKind::Semi) => {
        "Did you forget to close the parentheses before the semicolon?".to_string()
      },
      (TokenKind::RBrace, TokenKind::Eof) => {
        "Did you forget to close a block with '}'?".to_string()
      },
      (TokenKind::RParen, _) => {
        "Control flow statements require parentheses around conditions".to_string()
      },
      (TokenKind::Colon, TokenKind::Semi) => {
        "Ternary expressions use ':' to separate the branches".to_string()
      },
      (TokenKind::Eq, _) => "Use '=' for assignment".to_string(),
      _ => String::new(),
    }
  }

  /// Returns the substring that corresponds to `token`.
  pub(crate) fn get_token_lexeme(&mut self, token: &Token) -> String {
    self
      .source_file
      .src
      .get(token.span.start..token.span.end)
      .unwrap()
      .to_string()
  }

  pub(crate) fn check_comma_with_trailing(&mut self, trailing: bool) -> Result<bool, ()> {
    let bad = self.current_token();
    if matches!(bad.kind, TokenKind::Comma) {
      match self.peek(1).kind {
        kind if kind.token_starts_expression() => {
          self.advance(); // consume comma
          Ok(true)
        },
        kind if matches!(kind, TokenKind::RParen) && trailing => {
          self.advance(); // consume comma
          Ok(true)
        },

        _ => {
          let found = self.get_token_lexeme(&bad);
          let diagnostic = self
            .diagnostic(
              DiagnosticError::InvalidComma,
              "unexpected comma".to_string(),
            )
            .with_label(
              bad.span,
              Some(format!("expected expression or ')', found `{found}`")),
              LabelStyle::Primary,
            )
            .with_help("remove the comma or add a valid element after it".to_string());
          self.emit(diagnostic);
          Err(())
        },
      }
    } else {
      Ok(false)
    }
  }
}
