// Matches macro items and macro invocations from the grammar.
// This is syntax only.

use diagnostic::Span;

use crate::ast::{Delimiter, Ident, Path, TokenTree};

#[derive(Debug, Clone, PartialEq)]
pub struct MacroInvocation {
  pub path: Path,
  pub delimiter: Delimiter,
  pub tokens: Vec<TokenTree>,
  pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MacroRulesDecl {
  pub name: Ident,
  pub rules: Vec<MacroRule>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MacroRule {
  pub matcher: TokenTree,     // Delimited(...)
  pub transcriber: TokenTree, // Delimited(...)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Macro2Decl {
  pub name: String,
  pub params: Vec<String>,
  pub body: TokenTree, // Delimited(...)
  pub span: Span,
}
