// Matches:
// outerAttr -> "#[" attrInput "]"
// innerAttr -> "#![" attrInput "]"
// attrInput -> simplePath (delimTokenTree | "=" expression)?

use diagnostic::Span;

use crate::ast::{Delimiter, Expr, Path, TokenTree};

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum AttrStyle {
  Outer,
  Inner,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
  pub style: AttrStyle,
  pub input: AttrInput,
  pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AttrInput {
  pub path: Path,
  pub args: Option<AttrArgs>,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum AttrArgs {
  Delimited {
    delimiter: Delimiter,
    tokens: Vec<TokenTree>,
  },
  NameValue {
    value: Box<Expr>,
  },
}
