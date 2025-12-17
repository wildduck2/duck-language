// Token trees used by attributes and macros.
// This is purely syntactic (no expansion).

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
  Token(String),
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
    name: String,
    frag: String,
  },
}
