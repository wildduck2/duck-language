use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
  Span,
};

use crate::Parser;

impl Parser {
  // Create a parser diagnostic pre-populated with the current file path.
  pub(crate) fn diagnostic(
    &self,
    code: DiagnosticError,
    message: impl Into<String>,
  ) -> Diagnostic {
    Diagnostic::new(DiagnosticCode::Error(code), message.into(), self.source_file.path.clone())
  }

  // Emit a diagnostic through the parser's engine.
  pub(crate) fn emit(&mut self, diagnostic: Diagnostic) {
    self.engine.borrow_mut().add(diagnostic);
  }

  // Token and syntax errors
  pub(crate) fn err_unexpected_token(&self, span: Span, expected: &str, found: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::UnexpectedToken,
        format!("expected `{expected}`, found `{found}`"),
      )
      .with_label(
        span,
        Some(format!("expected `{expected}` here")),
        LabelStyle::Primary,
      )
      .with_note(format!("unexpected token: `{found}`"))
  }

  pub(crate) fn err_missing_closing_bracket(
    &self,
    span: Span,
    bracket_type: &str,
  ) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::MissingClosingBracket,
        format!("missing closing bracket `{bracket_type}`"),
      )
      .with_label(
        span,
        Some(format!("opening bracket `{bracket_type}` is not closed")),
        LabelStyle::Primary,
      )
      .with_help(format!("add a closing `{bracket_type}` to match the opening bracket"))
  }

  // Literal errors
  pub(crate) fn err_invalid_literal(&self, span: Span, details: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidLiteral,
        format!("invalid literal: {details}"),
      )
      .with_label(span, Some(details.to_string()), LabelStyle::Primary)
  }

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

  pub(crate) fn err_empty_char(&self, span: Span) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::EmptyChar,
        "empty character literal",
      )
      .with_label(span, Some("character literal cannot be empty".to_string()), LabelStyle::Primary)
      .with_help("character literals must contain exactly one Unicode scalar value".to_string())
  }

  // Visibility and naming errors
  pub(crate) fn err_invalid_visibility_restriction(
    &self,
    span: Span,
    restriction: &str,
  ) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidVisibilityRestriction,
        format!("invalid visibility restriction: `{restriction}`"),
      )
      .with_label(
        span,
        Some(format!("`{restriction}` is not a valid visibility modifier")),
        LabelStyle::Primary,
      )
      .with_help("valid visibility modifiers are `pub`, `pub(crate)`, `pub(super)`, or `pub(in path)`".to_string())
  }

  pub(crate) fn err_invalid_name_identifier(&self, span: Span, found: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidNameIdentifier,
        format!("expected identifier, found `{found}`"),
      )
      .with_label(
        span,
        Some(format!("`{found}` is not a valid identifier")),
        LabelStyle::Primary,
      )
      .with_help("identifiers must start with a letter or underscore and contain only alphanumeric characters and underscores".to_string())
  }

  // Type errors
  pub(crate) fn err_invalid_type(&self, span: Span, found: &str, context: Option<&str>) -> Diagnostic {
    let msg = if let Some(ctx) = context {
      format!("expected type, found `{found}` in {ctx}")
    } else {
      format!("expected type, found `{found}`")
    };
    let mut diag = self
      .diagnostic(DiagnosticError::InvalidType, msg.clone())
      .with_label(
        span,
        Some(format!("not a type: `{found}`")),
        LabelStyle::Primary,
      );
    if let Some(ctx) = context {
      diag = diag.with_note(format!("in {ctx}, a type was expected here"));
    }
    diag.with_help("if this is a type name, ensure it is declared or imported into scope".to_string())
  }

  pub(crate) fn err_invalid_mutability_in_field(&self, span: Span, specifier: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidMutabilityInField,
        format!("`{specifier}` cannot be used in this position"),
      )
      .with_label(
        span,
        Some(format!("`{specifier}` is not allowed before a bare type")),
        LabelStyle::Primary,
      )
      .with_note("`mut` and `const` cannot modify field or type declarations directly".to_string())
      .with_help("use `&mut T` or `*mut T` for mutable references or pointers, or make the binding itself mutable".to_string())
  }

  pub(crate) fn err_invalid_pointer_type(&self, span: Span, details: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidPointerType,
        format!("invalid pointer type: {details}"),
      )
      .with_label(span, Some(details.to_string()), LabelStyle::Primary)
      .with_help("pointer types must be `*const T` or `*mut T` where T is a type".to_string())
  }

  pub(crate) fn err_invalid_mutability(&self, span: Span, context: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidMutability,
        format!("invalid mutability specifier in {context}"),
      )
      .with_label(
        span,
        Some(format!("mutability specifier not allowed in {context}")),
        LabelStyle::Primary,
      )
  }

  // Lifetime errors
  pub(crate) fn err_unexpected_lifetime(&self, span: Span, context: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::UnexpectedLifetime,
        format!("unexpected lifetime in {context}"),
      )
      .with_label(
        span,
        Some(format!("lifetimes are not allowed in {context}")),
        LabelStyle::Primary,
      )
      .with_help("remove the lifetime parameter or move it to an allowed position".to_string())
  }

  pub(crate) fn err_expected_lifetime(&self, span: Span, found: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::UnexpectedLifetime,
        format!("expected lifetime, found `{found}`"),
      )
      .with_label(
        span,
        Some("expected a valid lifetime here".to_string()),
        LabelStyle::Primary,
      )
      .with_note("a lifetime must be a valid identifier, like `'a` or `'b`".to_string())
  }

  // Generic and trait bound errors
  pub(crate) fn err_invalid_where_predicate(&self, span: Span, found: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidWherePredicate,
        format!("expected where-clause predicate, found `{found}`"),
      )
      .with_label(
        span,
        Some(format!("expected a predicate like `T: Trait` or `'a: 'b`, found `{found}`")),
        LabelStyle::Primary,
      )
      .with_note("where-clause predicates must be of the form `Type: Bound`, `'lifetime: 'bound`, or `Type::Assoc = Type`".to_string())
  }

  pub(crate) fn err_invalid_where_clause(&self, span: Span, found: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidWhereClause,
        format!("expected where-clause predicate, found `{found}`"),
      )
      .with_label(
        span,
        Some("expected a valid where-clause predicate here".to_string()),
        LabelStyle::Primary,
      )
      .with_note("where-clauses must contain at least one predicate, like `T: Clone` or `'a: 'b`".to_string())
  }

  pub(crate) fn err_invalid_trait_bound(&self, span: Span, found: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidTraitBound,
        format!("expected trait bound, found `{found}`"),
      )
      .with_label(
        span,
        Some("expected a trait bound here".to_string()),
        LabelStyle::Primary,
      )
      .with_note("trait bounds must be trait names, like `Clone` or `Copy`".to_string())
  }

  pub(crate) fn err_invalid_trait_bound_modifier(&self, span: Span, modifier: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidTraitBoundModifier,
        format!("invalid trait bound modifier: `{modifier}`"),
      )
      .with_label(
        span,
        Some(format!("`{modifier}` is not a valid trait bound modifier")),
        LabelStyle::Primary,
      )
      .with_help("valid trait bound modifiers are `?` for optional bounds".to_string())
  }

  pub(crate) fn err_invalid_generic_args(&self, span: Span, details: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidGenericArgs,
        format!("invalid generic arguments: {details}"),
      )
      .with_label(span, Some(details.to_string()), LabelStyle::Primary)
      .with_help("generic arguments must be types, lifetimes, or const values".to_string())
  }

  pub(crate) fn err_empty_generic_args(&self, span: Span) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::EmptyGenericArgs,
        "empty generic arguments list",
      )
      .with_label(
        span,
        Some("generic arguments list cannot be empty".to_string()),
        LabelStyle::Primary,
      )
      .with_help("remove the angle brackets `<>` or add type parameters".to_string())
  }

  // Path errors
  pub(crate) fn err_invalid_path_segment(&self, span: Span, found: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidPathSegment,
        format!("expected path segment, found `{found}`"),
      )
      .with_label(
        span,
        Some(format!("expected a path segment, found `{found}`")),
        LabelStyle::Primary,
      )
      .with_help("valid path segments are identifiers or keywords like `self`, `super`, `crate`, or `$crate`".to_string())
  }

  // Syntax and structure errors
  pub(crate) fn err_invalid_trailing_comma(&self, span: Span, context: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidTrailingComma,
        format!("trailing comma not allowed in {context}"),
      )
      .with_label(
        span,
        Some(format!("remove this trailing comma")),
        LabelStyle::Primary,
      )
      .with_note(format!("trailing commas are not permitted in {context}"))
  }

  // Function and parameter errors
  pub(crate) fn err_invalid_abi(&self, span: Span, abi: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidAbi,
        format!("invalid ABI: `{abi}`"),
      )
      .with_label(
        span,
        Some(format!("`{abi}` is not a recognized ABI")),
        LabelStyle::Primary,
      )
      .with_help("common ABIs include `C`, `system`, `rust-intrinsic`, etc.".to_string())
  }

  pub(crate) fn err_invalid_variadic(&self, span: Span, context: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidVariadic,
        format!("variadic functions are not allowed in {context}"),
      )
      .with_label(
        span,
        Some("variadic functions are not supported here".to_string()),
        LabelStyle::Primary,
      )
      .with_help("use a fixed number of parameters or a slice/array parameter instead".to_string())
  }

  pub(crate) fn err_invalid_self_param(&self, span: Span, context: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidSelfParam,
        format!("invalid `self` parameter in {context}"),
      )
      .with_label(
        span,
        Some(format!("`self` parameter not allowed in {context}")),
        LabelStyle::Primary,
      )
      .with_help("`self` can only be used as the first parameter of associated functions or methods".to_string())
  }

  pub(crate) fn err_invalid_self_in_free_function(&self, span: Span) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidSelfInFreeFunction,
        "`self` cannot be used in a free function",
      )
      .with_label(
        span,
        Some("`self` is only valid as a parameter in associated functions or methods".to_string()),
        LabelStyle::Primary,
      )
      .with_help("remove `self` or move this function into an `impl` block".to_string())
  }

  // Control flow errors
  pub(crate) fn err_return_outside_function(&self, span: Span) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::ReturnOutsideFunction,
        "`return` statement outside of function",
      )
      .with_label(
        span,
        Some("`return` can only be used inside a function body".to_string()),
        LabelStyle::Primary,
      )
      .with_help("remove this `return` statement or move it inside a function".to_string())
  }

  pub(crate) fn err_continue_outside_loop(&self, span: Span) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::ContinueOutsideLoop,
        "`continue` statement outside of loop",
      )
      .with_label(
        span,
        Some("`continue` can only be used inside a loop body".to_string()),
        LabelStyle::Primary,
      )
      .with_help("remove this `continue` statement or move it inside a loop".to_string())
  }

  pub(crate) fn err_break_outside_loop(&self, span: Span) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::BreakOutsideLoop,
        "`break` statement outside of loop",
      )
      .with_label(
        span,
        Some("`break` can only be used inside a loop body".to_string()),
        LabelStyle::Primary,
      )
      .with_help("remove this `break` statement or move it inside a loop".to_string())
  }

  pub(crate) fn err_invalid_condition(&self, span: Span, found: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidCondition,
        format!("expected boolean expression, found `{found}`"),
      )
      .with_label(
        span,
        Some("expected a condition that evaluates to a boolean".to_string()),
        LabelStyle::Primary,
      )
      .with_help("conditions in `if`, `while`, and `match` expressions must be boolean".to_string())
  }

  // Block and flavor errors
  pub(crate) fn err_expected_block_after_flavor(&self, span: Span, flavor: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::ExpectedBlockAfterFlavor,
        format!("expected block after `{flavor}`"),
      )
      .with_label(
        span,
        Some(format!("expected a block `{{ ... }}` after `{flavor}`")),
        LabelStyle::Primary,
      )
      .with_help(format!("add a block after `{flavor}`"))
  }

  pub(crate) fn err_invalid_block_flavor_context(
    &self,
    span: Span,
    flavor: &str,
    context: &str,
  ) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidBlockFlavorContext,
        format!("`{flavor}` block not allowed in {context}"),
      )
      .with_label(
        span,
        Some(format!("`{flavor}` blocks cannot be used in {context}")),
        LabelStyle::Primary,
      )
  }

  pub(crate) fn err_invalid_flavor_order(&self, span: Span, details: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::InvalidFlavorOrder,
        format!("invalid block flavor order: {details}"),
      )
      .with_label(span, Some(details.to_string()), LabelStyle::Primary)
  }

  // Trailing `+` in bounds (existing, but improved)
  pub(crate) fn err_trailing_plus_in_bounds(&self, span: Span, context: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::UnexpectedToken,
        format!("trailing `+` in {context} bounds"),
      )
      .with_label(
        span,
        Some("remove this trailing `+` or add another bound".to_string()),
        LabelStyle::Primary,
      )
      .with_help("write bounds like `Clone + Copy` without a dangling `+`".to_string())
  }

  pub(crate) fn err_expected_bound_after_plus(&self, span: Span, context: &str) -> Diagnostic {
    self
      .diagnostic(
        DiagnosticError::UnexpectedToken,
        format!("expected another bound after `+` in {context}"),
      )
      .with_label(
        span,
        Some("add a bound after `+`, e.g. `Trait` or `'a`".to_string()),
        LabelStyle::Primary,
      )
      .with_note("bounds are separated by `+`, such as `Clone + 'a`".to_string())
  }
}

// Decoder-specific diagnostic helpers (static functions that don't require Parser)
impl Parser {
  pub(crate) fn decoder_err_invalid_literal(path: &str, span: Span, details: &str) -> Diagnostic {
    Diagnostic::new(
      DiagnosticCode::Error(DiagnosticError::InvalidLiteral),
      format!("invalid literal: {details}"),
      path.to_string(),
    )
    .with_label(span, Some(details.to_string()), LabelStyle::Primary)
  }

  pub(crate) fn decoder_err_unterminated_string(path: &str, span: Span, details: &str) -> Diagnostic {
    Diagnostic::new(
      DiagnosticCode::Error(DiagnosticError::UnterminatedString),
      format!("unterminated string literal: {details}"),
      path.to_string(),
    )
    .with_label(span, Some(details.to_string()), LabelStyle::Primary)
    .with_help("add a closing quote to terminate the string literal".to_string())
  }

  pub(crate) fn decoder_err_invalid_escape(path: &str, span: Span, escape: &str, context: Option<&str>) -> Diagnostic {
    let msg = if let Some(ctx) = context {
      format!("invalid escape sequence `\\{escape}` in {ctx}")
    } else {
      format!("invalid escape sequence `\\{escape}`")
    };
    let mut diag = Diagnostic::new(
      DiagnosticCode::Error(DiagnosticError::InvalidEscape),
      msg,
      path.to_string(),
    )
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

  pub(crate) fn decoder_err_char_too_long(path: &str, span: Span, chars: &[char]) -> Diagnostic {
    Diagnostic::new(
      DiagnosticCode::Error(DiagnosticError::EmptyChar),
      format!("character literal contains {} character(s), expected exactly 1", chars.len()),
      path.to_string(),
    )
    .with_label(
      span,
      Some("character literal must contain exactly one Unicode scalar value".to_string()),
      LabelStyle::Primary,
    )
    .with_help("character literals must be a single Unicode scalar or a single escape sequence".to_string())
  }

  pub(crate) fn decoder_err_byte_out_of_range(path: &str, span: Span) -> Diagnostic {
    Diagnostic::new(
      DiagnosticCode::Error(DiagnosticError::InvalidLiteral),
      "byte literal out of range".to_string(),
      path.to_string(),
    )
    .with_label(
      span,
      Some("value does not fit in u8 (0-255)".to_string()),
      LabelStyle::Primary,
    )
    .with_help("byte literals must be in the range 0-255".to_string())
  }

  pub(crate) fn decoder_err_byte_string_non_byte(path: &str, span: Span) -> Diagnostic {
    Diagnostic::new(
      DiagnosticCode::Error(DiagnosticError::InvalidLiteral),
      "byte string contains non-byte value".to_string(),
      path.to_string(),
    )
    .with_label(
      span,
      Some("value does not fit in u8 (0-255)".to_string()),
      LabelStyle::Primary,
    )
    .with_help("byte strings can only contain values in the range 0-255".to_string())
  }
}
