// Matches macro items and macro invocations from the grammar.
// This is syntax only.

use diagnostic::Span;

use crate::ast::{Delimiter, Path, TokenTree};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct MacroInvocation {
  pub path: Path,
  pub delimiter: Delimiter,
  pub tokens: Vec<TokenTree>,
  pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct MacroRulesDecl {
  pub name: String,
  pub rules: Vec<MacroRule>,
  pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct MacroRule {
  pub matcher: TokenTree,     // Delimited(...)
  pub transcriber: TokenTree, // Delimited(...)
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Macro2Decl {
  pub name: String,
  pub params: Vec<String>,
  pub body: TokenTree, // Delimited(...)
  pub span: Span,
}
