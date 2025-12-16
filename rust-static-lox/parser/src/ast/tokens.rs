// Token trees used by attributes and macros.
// This is purely syntactic (no expansion).

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Delimiter {
  Paren,
  Brace,
  Bracket,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum RepeatKind {
  ZeroOrMore,
  OneOrMore,
  ZeroOrOne,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TokenTree {
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
