//! Keyword and identifier lexer.
//!
//! This module recognizes every reserved word the language understands while
//! still emitting ordinary identifiers (including `r#raw` forms) and reporting
//! malformed ones with clear diagnostics.

use diagnostic::Span;

use crate::{token::TokenKind, Lexer};

impl Lexer {
  /// Lexes a keyword, identifier, or raw identifier.
  ///
  /// The method consumes `[A-Za-z0-9_]` after the initial character and then
  /// checks whether the resulting lexeme matches a reserved keyword. It also
  /// understands `r#raw_ident` syntax and diagnoses identifiers that start
  /// with digits.
  pub(crate) fn lex_keywords(&mut self) -> Result<TokenKind, ()> {
    self.consume_identifier_body();

    let mut lexeme = self.get_current_lexeme();
    if self.should_parse_raw_identifier(lexeme) {
      self.advance(); // consume '#'
      self.consume_identifier_body();
      lexeme = self.get_current_lexeme();
    }
    let lexeme_owned = lexeme.to_string();

    match lexeme_owned.as_str() {
      // Control Flow Keywords
      "if" => Ok(TokenKind::KwIf),
      "else" => Ok(TokenKind::KwElse),
      "match" => Ok(TokenKind::KwMatch),
      "loop" => Ok(TokenKind::KwLoop),
      "while" => Ok(TokenKind::KwWhile),
      "for" => Ok(TokenKind::KwFor),
      "break" => Ok(TokenKind::KwBreak),
      "continue" => Ok(TokenKind::KwContinue),
      "return" => Ok(TokenKind::KwReturn),

      // Declaration Keywords
      "let" => Ok(TokenKind::KwLet),
      "fn" => Ok(TokenKind::KwFn),
      "struct" => Ok(TokenKind::KwStruct),
      "enum" => Ok(TokenKind::KwEnum),
      "union" => Ok(TokenKind::KwUnion),
      "trait" => Ok(TokenKind::KwTrait),
      "impl" => Ok(TokenKind::KwImpl),
      "type" => Ok(TokenKind::KwType),
      "mod" => Ok(TokenKind::KwMod),
      "use" => Ok(TokenKind::KwUse),
      "const" => Ok(TokenKind::KwConst),
      "static" => Ok(TokenKind::KwStatic),
      "extern" => Ok(TokenKind::KwExtern),
      "macro" => Ok(TokenKind::KwMacro),
      "auto" => Ok(TokenKind::KwAuto),
      "default" => Ok(TokenKind::KwDefault),
      "macro_rules" => Ok(TokenKind::KwMacroRules),

      // Modifier Keywords
      "pub" => Ok(TokenKind::Kwpub),
      "mut" => Ok(TokenKind::KwMut),
      "ref" => Ok(TokenKind::KwRef),
      "move" => Ok(TokenKind::KwMove),
      "unsafe" => Ok(TokenKind::KwUnsafe),
      "async" => Ok(TokenKind::KwAsync),
      "await" => Ok(TokenKind::KwAwait),
      "dyn" => Ok(TokenKind::KwDyn),

      // Special Identifiers
      "self" => Ok(TokenKind::KwSelf),
      "Self" => Ok(TokenKind::KwSelfType),
      "super" => Ok(TokenKind::KwSuper),
      "crate" => Ok(TokenKind::KwCrate),

      // Literal Keywords
      "true" => Ok(TokenKind::KwTrue),
      "false" => Ok(TokenKind::KwFalse),

      // Other Keywords
      "as" => Ok(TokenKind::KwAs),
      "in" => Ok(TokenKind::KwIn),
      "where" => Ok(TokenKind::KwWhere),

      // Reserved Keywords (not yet used, but reserved for future use)
      "abstract" => Ok(TokenKind::KwAbstract),
      "become" => Ok(TokenKind::KwBecome),
      "box" => Ok(TokenKind::KwBox),
      "do" => Ok(TokenKind::KwDo),
      "final" => Ok(TokenKind::KwFinal),
      "override" => Ok(TokenKind::KwOverride),
      "try" => Ok(TokenKind::KwTry),
      "typeof" => Ok(TokenKind::KwTypeof),
      "unsized" => Ok(TokenKind::KwUnsized),
      "virtual" => Ok(TokenKind::KwVirtual),
      "yield" => Ok(TokenKind::KwYield),

      _ => {
        // Handles regular identifiers (foo, _bar, Baz) and raw identifiers (r#type, r#match)
        // according to Rust’s lexical rules.

        if lexeme_owned.starts_with("r#") && lexeme_owned.len() > 2 {
          // r# followed by a valid identifier
          Ok(TokenKind::RawIdent)
        } else if lexeme_owned
          .chars()
          .next()
          .map(|ch| ch.is_ascii_digit())
          .unwrap_or(false)
        {
          // Invalid identifier (starts with a digit)

          self.report_invalid_identifier(lexeme_owned)
        } else {
          // Normal identifier
          Ok(TokenKind::Ident)
        }
      },
    }
  }

  fn consume_identifier_body(&mut self) {
    while let Some(ch) = self.peek() {
      if ch.is_ascii_alphanumeric() || ch == '_' {
        self.advance();
      } else {
        break;
      }
    }
  }

  fn should_parse_raw_identifier(&self, current_lexeme: &str) -> bool {
    current_lexeme == "r" && self.peek() == Some('#')
  }

  fn report_invalid_identifier(&mut self, lexeme_owned: String) -> Result<TokenKind, ()> {
    let span = Span::new(self.start, self.current);
    self.emit_diagnostic(self.err_invalid_identifier(span, &lexeme_owned));
    Err(())
  }
}
