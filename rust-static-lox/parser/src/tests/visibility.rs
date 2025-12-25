#[cfg(test)]
mod visibility_tests {
  use crate::{
    ast::{path::PathSegmentKind, Visibility},
    parser_utils::ParserContext,
    tests::support::run_parser,
  };

  fn parse_visibility(input: &str) -> Result<Visibility, ()> {
    run_parser(input, "visibility_test_temp", |parser| {
      parser.parse_visibility(ParserContext::Default)
    })
  }

  fn assert_err(input: &str) {
    assert!(parse_visibility(input).is_err(), "expected error for {input:?}");
  }

  #[test]
  fn defaults_to_private() {
    assert_eq!(parse_visibility("").unwrap(), Visibility::Private);
  }

  #[test]
  fn parses_public_visibility() {
    assert_eq!(parse_visibility("pub").unwrap(), Visibility::Public);
  }

  #[test]
  fn parses_restricted_visibilities() {
    assert_eq!(parse_visibility("pub(crate)").unwrap(), Visibility::PublicCrate);
    assert_eq!(parse_visibility("pub(self)").unwrap(), Visibility::PublicSelf);
    assert_eq!(parse_visibility("pub(super)").unwrap(), Visibility::PublicSuper);
  }

  #[test]
  fn parses_in_path_visibility() {
    match parse_visibility("pub(in foo::bar)").unwrap() {
      Visibility::PublicIn(path) => {
        assert!(!path.leading_colon);
        assert_eq!(path.segments.len(), 2);
        match &path.segments[0].kind {
          PathSegmentKind::Ident(name) => assert_eq!(name, "foo"),
          other => panic!("expected ident path segment, got: {:?}", other),
        }
        match &path.segments[1].kind {
          PathSegmentKind::Ident(name) => assert_eq!(name, "bar"),
          other => panic!("expected ident path segment, got: {:?}", other),
        }
      },
      other => panic!("expected pub(in ...) visibility, got: {:?}", other),
    }
  }

  #[test]
  fn parses_in_path_with_leading_colon() {
    match parse_visibility("pub(in ::foo)").unwrap() {
      Visibility::PublicIn(path) => {
        assert!(path.leading_colon);
        assert_eq!(path.segments.len(), 1);
        match &path.segments[0].kind {
          PathSegmentKind::Ident(name) => assert_eq!(name, "foo"),
          other => panic!("expected ident path segment, got: {:?}", other),
        }
      },
      other => panic!("expected pub(in ...) visibility, got: {:?}", other),
    }
  }

  #[test]
  fn rejects_invalid_restrictions() {
    assert_err("pub()");
    assert_err("pub(in)");
    assert_err("pub(foo)");
  }

  #[test]
  fn rejects_missing_closing_paren() {
    assert_err("pub(crate");
    assert_err("pub(in crate::foo");
  }

  #[test]
  fn rejects_in_path_with_trailing_colon() {
    assert_err("pub(in crate::)");
  }
}
