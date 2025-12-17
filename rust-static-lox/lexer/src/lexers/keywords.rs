//! Lexer for keywords and identifiers.
//!
//! Recognizes Rust keywords and distinguishes them from regular identifiers.
//! Also handles raw identifiers (`r#type`) and invalid identifiers.

use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
  DiagnosticEngine, Span,
};

use crate::{token::TokenKind, Lexer};

impl Lexer {
  /// Lexes a keyword or identifier.
  ///
  /// Consumes alphanumeric characters and underscores to form a complete
  /// identifier, then checks if it matches a known keyword. Also handles
  /// raw identifiers (`r#...`) and detects invalid identifiers (starting with digits).
  ///
  /// # Returns
  ///
  /// `Some(TokenKind)` - Keyword token, `Ident`, `RawIdent`, or `InvalidIdent`
  pub fn lex_keywords(&mut self, engine: &mut DiagnosticEngine) -> Result<TokenKind, ()> {
    // Consume valid identifier characters
    while let Some(ch) = self.peek() {
      if !ch.is_ascii_alphanumeric() && ch != '_' {
        break;
      }
      self.advance();
    }

    match self.get_current_lexeme() {
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

        let lexeme = self.get_current_lexeme();

        if lexeme.starts_with("r#") && lexeme.len() > 2 {
          // r# followed by a valid identifier
          Ok(TokenKind::RawIdent)
        } else if lexeme
          .chars()
          .next()
          .map(|ch| ch.is_ascii_digit())
          .unwrap_or(false)
        {
          // Invalid identifier (starts with a digit)

          let diagnostic = Diagnostic::new(
            DiagnosticCode::Error(DiagnosticError::InvalidIdentifier),
            format!("Invalid identifier: {}", lexeme),
            self.source.path.to_string(),
          )
          .with_label(
            Span::new(self.start, self.current),
            Some("Invalid identifier".to_string()),
            LabelStyle::Primary,
          )
          .with_help("Identifiers must start with a letter.".to_string());

          engine.add(diagnostic);

          return Err(());
        } else {
          // Normal identifier
          Ok(TokenKind::Ident)
        }
      },
    }
  }
}
