// Small shared name types that show up everywhere.

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum Ident {
  Name(String),
  Underscore,
  Self_,
}

impl Ident {
  pub fn as_str(&self) -> &str {
    match self {
      Ident::Name(s) => s,
      Ident::Underscore => "_",
      Ident::Self_ => "self",
    }
  }
}

pub type Lifetime = String;
pub type Label = String;
