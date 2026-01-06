// Token trees used by attributes and macros.
// This is purely syntactic (no expansion).

use crate::ast::Ident;
use lexer::token::TokenKind;

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum Delimiter {
  Paren,
  Brace,
  Bracket,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum RepeatKind {
  ZeroOrMore,
  OneOrMore,
  ZeroOrOne,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum TokenTree {
  Token {
    kind: TokenKind,
    lexeme: String,
  },
  Delimited {
    delimiter: Delimiter,
    tokens: Vec<TokenTree>,
  },
  Repeat {
    tokens: Vec<TokenTree>,
    separator: Option<String>,
    kind: RepeatKind,
  },
  MetaVar {
    name: Ident,
    frag: Ident,
  },
}
