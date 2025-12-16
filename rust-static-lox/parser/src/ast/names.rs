// Small shared name types that show up everywhere.

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Ident {
  Name(String),
  Underscore,
}

impl Ident {
  pub fn as_str(&self) -> &str {
    match self {
      Ident::Name(s) => s,
      Ident::Underscore => "_",
    }
  }
}

pub(crate) type Lifetime = String;
pub(crate) type Label = String;
