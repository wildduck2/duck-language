// src/ast/pattern.rs
//
// Pattern nodes aligned with the grammar section "Patterns".

use diagnostic::Span;

use crate::ast::{Attribute, Expr, Path, Type};

#[derive(Debug, Clone, PartialEq)]
pub enum BindingMode {
  ByValue(crate::ast::Mutability),
  ByRef(crate::ast::Mutability),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Pattern {
  Wildcard {
    span: Span,
  },
  Rest {
    span: Span,
  },

  Literal {
    expr: Box<Expr>,
    span: Span,
  },

  Ident {
    binding: BindingMode,
    name: String,
    subpattern: Option<Box<Pattern>>,
    span: Span,
  },

  Path {
    qself: Option<Box<Type>>,
    path: Path,
    span: Span,
  },

  Tuple {
    patterns: Vec<Pattern>,
    span: Span,
  },

  Slice {
    before: Vec<Pattern>,
    has_rest: bool,
    after: Vec<Pattern>,
    span: Span,
  },

  Struct {
    qself: Option<Box<Type>>,
    path: Path,
    fields: Vec<FieldPattern>,
    has_rest: bool,
    span: Span,
  },

  TupleStruct {
    qself: Option<Box<Type>>,
    path: Path,
    patterns: Vec<Pattern>,
    span: Span,
  },

  Or {
    patterns: Vec<Pattern>,
    span: Span,
  },

  Range {
    start: Option<Box<Expr>>,
    end: Option<Box<Expr>>,
    kind: RangeKind,
    span: Span,
  },

  Reference {
    depth: usize, // & or && in grammar for referencePattern
    mutability: crate::ast::Mutability,
    pattern: Box<Pattern>,
    span: Span,
  },

  Group {
    pattern: Box<Pattern>,
    span: Span,
  },

  Macro {
    path: Path,
    span: Span,
  },
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FieldPattern {
  pub attributes: Vec<Attribute>,
  pub name: String,
  pub pattern: Option<Pattern>,
  pub is_shorthand: bool,
  pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum RangeKind {
  Exclusive,         // a..b
  Inclusive,         // a..=b
  ObsoleteInclusive, // a...b
  From,              // a..
  To,                // ..b
  ToInclusive,       // ..=b
  Full,              // ..
}
