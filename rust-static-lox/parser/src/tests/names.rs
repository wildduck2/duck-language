#[cfg(test)]
mod names_tests {
  use crate::ast::Ident;

  #[test]
  fn ident_name_as_str() {
    let ident = Ident::Name("foo".to_string());
    assert_eq!(ident.as_str(), "foo");
  }

  #[test]
  fn ident_underscore_as_str() {
    let ident = Ident::Underscore;
    assert_eq!(ident.as_str(), "_");
  }

  #[test]
  fn ident_self_as_str() {
    let ident = Ident::Self_;
    assert_eq!(ident.as_str(), "self");
  }

  #[test]
  fn ident_self_type_as_str() {
    let ident = Ident::SelfType;
    assert_eq!(ident.as_str(), "Self");
  }

  #[test]
  fn ident_crate_as_str() {
    let ident = Ident::Crate;
    assert_eq!(ident.as_str(), "crate");
  }

  #[test]
  fn ident_super_as_str() {
    let ident = Ident::Super;
    assert_eq!(ident.as_str(), "super");
  }
}

