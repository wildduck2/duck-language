use crate::ast::*;
use diagnostic::Span;

/// Binding mode for identifier patterns.
///
/// This models:
/// - `x`         => ByValue(Immutable)
/// - `mut x`     => ByValue(Mutable)
/// - `ref x`     => ByRef(Immutable)
/// - `ref mut x` => ByRef(Mutable)
#[derive(Debug, Clone, PartialEq)]
pub enum BindingMode {
  ByValue(Mutability),
  ByRef(Mutability),
}

/// All pattern forms supported by the language.
///
/// Patterns appear in let bindings, match arms, and destructuring.
/// They follow Rust-style semantics with a few extensions.
///
/// Core categories:
/// - Wildcards (`_`)
/// - Bindings (`x`, `ref x`, `mut x`, `x @ pat`)
/// - Destructuring (tuple, slice, struct, tuple-struct)
/// - Literals and constants
/// - Rest patterns (`..`, `..name`)
/// - Ranges (`1..5`, `a..=z`)
/// - OR patterns (`p1 | p2`)
/// - Reference and deref patterns (`&p`, `*p`)
/// - Parenthesized patterns
#[derive(Debug, Clone)]
pub(crate) enum Pattern {
  /// `_` matches anything and binds nothing.
  Wildcard { span: Span },

  /// `..` or `..name` rest (spread) pattern for slices.
  Rest { name: Option<String>, span: Span },

  /// Pure literal pattern: number, string, bool, or char.
  Literal { expr: Box<Expr>, span: Span },

  /// Compile-time constant pattern (consts, enum variants, and paths).
  Const { expr: Box<Expr>, span: Span },

  /// Binding pattern: `x`, `mut x`, `ref x`, `x @ pat`.
  Ident {
    binding: BindingMode,
    name: String,
    subpattern: Option<Box<Pattern>>,
    span: Span,
  },

  /// Path pattern: enum variants, type constructors, namespaces.
  Path {
    qself: Option<Box<Type>>,
    path: Path,
    span: Span,
  },

  /// Tuple pattern: `(a, b, c)`.
  Tuple {
    attributes: Vec<Attribute>,
    patterns: Vec<Pattern>,
    span: Span,
  },

  /// Slice pattern `[before..., ..middle?, after...]`.
  ///
  /// Examples: `[a, b]`, `[a, ..]`, `[a, ..rest, b]`
  Slice {
    before: Vec<Pattern>,
    middle: Option<Box<Pattern>>,
    after: Vec<Pattern>,
    span: Span,
  },

  /// Struct pattern: `Point { x, y }`, `Point { x: a, .. }`.
  Struct {
    qself: Option<Box<Type>>,
    path: Path,
    fields: Vec<FieldPattern>,
    has_rest: bool,
    span: Span,
  },

  /// Tuple-struct pattern: `Some(x)`, `Color(r, g, b)`.
  TupleStruct {
    qself: Option<Box<Type>>,
    path: Path,
    patterns: Vec<Pattern>,
    span: Span,
  },

  /// OR pattern: `p1 | p2 | p3`.
  Or { patterns: Vec<Pattern>, span: Span },

  /// Range pattern: `a..b`, `a..=b`, `..b`, `a..`.
  Range {
    start: Option<Box<Expr>>,
    end: Option<Box<Expr>>,
    kind: RangeKind,
    span: Span,
  },

  /// Reference pattern: `&p` or `&mut p`.
  Reference {
    depth: usize,
    pattern: Box<Pattern>,
    span: Span,
  },

  /// Dereference pattern: `*p`.
  Deref { pattern: Box<Pattern>, span: Span },

  /// Box pattern: `box p`.
  Box { pattern: Box<Pattern>, span: Span },

  /// Macro pattern: `m!(...)`.
  Macro { mac: MacroInvocation },

  /// Parenthesized pattern `(p)`.
  Paren { pattern: Box<Pattern>, span: Span },

  /// Never pattern `!` which matches no value.
  Never { span: Span },
}

/// A single field inside a struct pattern.
///
/// Each field may be:
/// - a shorthand binding: `{ x }`
/// - a renamed field: `{ x: y }`
/// - a nested pattern: `{ p: Some(v) }`
///
/// Examples:
/// ```rust
/// { x, y: renamed, z: Some(value) }
/// ```
#[derive(Debug, Clone)]
pub(crate) struct FieldPattern {
  /// Optional attributes attached to the field.
  pub attributes: Vec<Attribute>,
  /// The field name appearing in the struct.
  pub name: String,
  /// Optional nested pattern if the field is not shorthand.
  pub pattern: Option<Pattern>,
  /// True for shorthand forms like `{ x }`.
  pub is_shorthand: bool,
}
