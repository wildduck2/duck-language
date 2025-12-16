// Matches:
// outerAttr -> "#[" attrInput "]"
// innerAttr -> "#![" attrInput "]"
// attrInput -> simplePath (delimTokenTree | "=" expression)?

use diagnostic::Span;

use crate::ast::{Delimiter, Expr, Path, TokenTree};

#[derive(Debug, Clone)]
pub(crate) enum AttrStyle {
  Outer,
  Inner,
}

#[derive(Debug, Clone)]
pub(crate) struct Attribute {
  pub style: AttrStyle,
  pub input: AttrInput,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct AttrInput {
  pub path: Path,
  pub args: Option<AttrArgs>,
}

#[derive(Debug, Clone)]
pub(crate) enum AttrArgs {
  Delimited {
    delimiter: Delimiter,
    tokens: Vec<TokenTree>,
  },
  NameValue {
    value: Box<Expr>,
  },
}
