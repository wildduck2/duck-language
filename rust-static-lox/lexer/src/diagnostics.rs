use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
  Span,
};

use crate::Lexer;

impl Lexer {
  // Create a lexer diagnostic pre-populated with this source file path.
  pub(crate) fn diagnostic(
    &self,
    code: DiagnosticError,
    message: impl Into<String>,
  ) -> Diagnostic {
    Diagnostic::new(DiagnosticCode::Error(code), message.into(), self.source.path.clone())
  }

  // Emit a diagnostic through the lexer's engine.
  pub(crate) fn emit_diagnostic(&mut self, diagnostic: Diagnostic) {
    self.engine.borrow_mut().add(diagnostic);
  }

  // Shebang and character errors
  pub(crate) fn err_invalid_shebang(&self, span: Span, details: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidShebang,
        format!("invalid shebang: {details}"),
      )
      .with_label(span, Some(details.to_string()), LabelStyle::Primary)
      .with_help("shebangs must start with `#!` and be on the first line".to_string())
  }

  pub(crate) fn err_invalid_character(&self, span: Span, char: char) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidCharacter,
        format!("invalid character: `{char}`"),
      )
      .with_label(
        span,
        Some(format!("character `{char}` is not valid in source code")),
        LabelStyle::Primary,
      )
      .with_help("remove this character or replace it with a valid one".to_string())
  }

  // String and character literal errors
  pub(crate) fn err_unterminated_string(&self, span: Span, string_type: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::UnterminatedString,
        format!("unterminated {string_type} literal"),
      )
      .with_label(
        span,
        Some(format!("{string_type} literal is not terminated")),
        LabelStyle::Primary,
      )
      .with_help(format!("add a closing quote to terminate the {string_type} literal"))
  }

  pub(crate) fn err_too_many_raw_str_hashes(&self, span: Span, count: usize, max: usize) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::TooManyRawStrHashes,
        format!("raw string literal uses {count} `#` characters; maximum is {max}"),
      )
      .with_label(
        span,
        Some(format!("too many `#` characters here (maximum: {max})")),
        LabelStyle::Primary,
      )
      .with_help(format!("reduce the number of `#` characters to at most {max}"))
  }

  pub(crate) fn err_invalid_string_start(&self, span: Span, found: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidStringStart,
        format!("expected string literal start, found `{found}`"),
      )
      .with_label(
        span,
        Some("expected `\"` to start a string literal".to_string()),
        LabelStyle::Primary,
      )
      .with_help("string literals must start with `\"` or a valid prefix like `r#\"` or `b\"`".to_string())
  }

  pub(crate) fn err_invalid_escape(&self, span: Span, escape: &str, context: Option<&str>) -> Diagnostic {
    let msg = if let Some(ctx) = context {
      format!("invalid escape sequence `\\{escape}` in {ctx}")
    } else {
      format!("invalid escape sequence `\\{escape}`")
    };
    let mut diag = self
      .diagnostic(DiagnosticError::InvalidEscape, msg)
      .with_label(
        span,
        Some(format!("unknown escape sequence: `\\{escape}`")),
        LabelStyle::Primary,
      );
    if let Some(ctx) = context {
      diag = diag.with_note(format!("in {ctx}, only certain escape sequences are allowed"));
    }
    diag.with_help("valid escape sequences are: `\\n`, `\\r`, `\\t`, `\\\\`, `\\'`, `\\\"`, `\\xHH`, and `\\u{{HHHH}}`".to_string())
  }

  // Prefix errors
  pub(crate) fn err_unknown_prefix(&self, span: Span, prefix: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::UnknownPrefix,
        format!("unknown literal prefix: `{prefix}`"),
      )
      .with_label(
        span,
        Some(format!("`{prefix}` is not a recognized literal prefix")),
        LabelStyle::Primary,
      )
      .with_help("valid prefixes are: `b` (byte), `r` (raw), `c` (C string), `br` (byte raw), `cr` (C raw)".to_string())
  }

  pub(crate) fn err_reserved_prefix(&self, span: Span, prefix: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::ReservedPrefix,
        format!("`{prefix}` is a reserved prefix for string literals"),
      )
      .with_label(
        span,
        Some("this prefix is reserved for future use".to_string()),
        LabelStyle::Primary,
      )
      .with_help(format!("use a different prefix or remove `{prefix}`"))
  }

  // Lifetime errors
  pub(crate) fn err_invalid_lifetime(&self, span: Span, lexeme: &str, reason: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidLifetime,
        format!("`{lexeme}` is not a valid lifetime"),
      )
      .with_label(span, Some(reason.to_string()), LabelStyle::Primary)
      .with_help("lifetime names must start with a letter or underscore, like `'a` or `'_`".to_string())
  }

  pub(crate) fn err_lifetime_starts_with_digit(&self, span: Span, lexeme: &str) -> Diagnostic {
    self.err_invalid_lifetime(
      span,
      lexeme,
      "lifetime names cannot start with digits",
    )
    .with_help("rename the lifetime so it begins with a letter or `_`".to_string())
  }

  pub(crate) fn err_lifetime_missing_name(&self, span: Span) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidLifetime,
        "expected identifier after `'`",
      )
      .with_label(
        span,
        Some("no lifetime name provided".to_string()),
        LabelStyle::Primary,
      )
      .with_help("try providing a name like `'a` or use `'_`".to_string())
  }

  // Number literal errors
  pub(crate) fn err_invalid_integer(&self, span: Span, details: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidInteger,
        format!("invalid integer literal: {details}"),
      )
      .with_label(span, Some(details.to_string()), LabelStyle::Primary)
  }

  pub(crate) fn err_integer_starts_with_underscore(&self, span: Span, number: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidInteger,
        format!("`{number}` is not a valid integer literal"),
      )
      .with_label(
        span,
        Some("underscores may be used only between digits, never at the start or end".to_string()),
        LabelStyle::Primary,
      )
      .with_help("examples of valid literals: `1_000`, `0xFF_A0`, `123`".to_string())
      .with_note("examples of invalid literals: `_123`, `123_`, `0x_12`".to_string())
  }

  pub(crate) fn err_binary_literal_no_digits(&self, span: Span) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidInteger,
        "binary literal has no digits after `0b` prefix",
      )
      .with_label(
        span,
        Some("expected binary digits (`0` or `1`) after `0b`".to_string()),
        LabelStyle::Primary,
      )
      .with_help("add binary digits after `0b`, e.g. `0b1010`".to_string())
  }

  pub(crate) fn err_octal_literal_no_digits(&self, span: Span) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidInteger,
        "octal literal has no digits after `0o` prefix",
      )
      .with_label(
        span,
        Some("expected octal digits (`0`-`7`) after `0o`".to_string()),
        LabelStyle::Primary,
      )
      .with_help("add octal digits after `0o`, e.g. `0o755`".to_string())
  }

  pub(crate) fn err_hex_literal_no_digits(&self, span: Span) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidInteger,
        "hexadecimal literal has no digits after `0x` prefix",
      )
      .with_label(
        span,
        Some("expected hexadecimal digits (`0`-`9`, `a`-`f`, `A`-`F`) after `0x`".to_string()),
        LabelStyle::Primary,
      )
      .with_help("add hexadecimal digits after `0x`, e.g. `0xFF`".to_string())
  }

  pub(crate) fn err_invalid_digit_in_base(&self, span: Span, char: char, base: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidInteger,
        format!("invalid digit `{char}` in {base} literal"),
      )
      .with_label(
        span,
        Some(format!("expected a valid {base} digit here")),
        LabelStyle::Primary,
      )
  }

  pub(crate) fn err_consecutive_underscores(&self, span: Span, base: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidInteger,
        format!("invalid `_` placement in {base} literal"),
      )
      .with_label(
        span,
        Some("consecutive underscores are not allowed".to_string()),
        LabelStyle::Primary,
      )
      .with_help("underscores can only appear between digits, not consecutively".to_string())
  }

  pub(crate) fn err_underscore_after_prefix(&self, span: Span, prefix: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidInteger,
        format!("{prefix} literal cannot start with `_` after the prefix"),
      )
      .with_label(
        span,
        Some("remove this underscore".to_string()),
        LabelStyle::Primary,
      )
      .with_help(format!("add digits after `{prefix}` before any underscores"))
  }

  // Identifier errors
  pub(crate) fn err_invalid_identifier(&self, span: Span, found: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidIdentifier,
        format!("expected identifier, found `{found}`"),
      )
      .with_label(
        span,
        Some(format!("`{found}` is not a valid identifier")),
        LabelStyle::Primary,
      )
      .with_help("identifiers must start with a letter or underscore and contain only alphanumeric characters and underscores".to_string())
  }

  pub(crate) fn err_invalid_literal(&self, span: Span, details: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidLiteral,
        format!("invalid literal: {details}"),
      )
      .with_label(span, Some(details.to_string()), LabelStyle::Primary)
  }

  pub(crate) fn err_empty_char(&self, span: Span) -> Diagnostic {
    self
      .diagnostic(DiagnosticError::EmptyChar, "empty character literal")
      .with_label(
        span,
        Some("character literal must contain exactly one character".to_string()),
        LabelStyle::Primary,
      )
  }
}
