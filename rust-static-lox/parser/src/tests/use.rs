#[cfg(test)]
mod use_tests {
  use crate::{
    ast::{Ident, Item, UseDecl, UseTree, VisItem, VisItemKind, Visibility},
    parser_utils::ParserContext,
    tests::support::parse_item,
  };

  fn parse_use_item(input: &str) -> Result<VisItem, ()> {
    parse_item(input, "use_decl_test_temp", ParserContext::Default).and_then(|item| match item {
      Item::Vis(vis) => Ok(vis),
      _ => Err(()),
    })
  }

  fn parse_use_tree(input: &str) -> Result<UseTree, ()> {
    let vis = parse_use_item(input)?;
    match vis.kind {
      VisItemKind::Use(UseDecl { tree }) => Ok(tree),
      other => panic!("expected use decl, got: {:?}", other),
    }
  }

  fn assert_use_tree(input: &str, expected: UseTree) {
    let tree = parse_use_tree(input).unwrap();
    assert_eq!(tree, expected);
  }

  fn assert_use_err(input: &str) {
    assert!(parse_use_item(input).is_err(), "expected error for {input:?}");
  }

  #[test]
  fn parses_simple_use_name() {
    assert_use_tree("use foo;", UseTree::Name("foo".to_string()));
  }

  #[test]
  fn parses_use_path() {
    assert_use_tree(
      "use foo::bar;",
      UseTree::Path {
        prefix: "foo".to_string(),
        suffix: Box::new(UseTree::Name("bar".to_string())),
      },
    );
  }

  #[test]
  fn parses_use_nested_path() {
    assert_use_tree(
      "use foo::bar::baz;",
      UseTree::Path {
        prefix: "foo::bar".to_string(),
        suffix: Box::new(UseTree::Name("baz".to_string())),
      },
    );
  }

  #[test]
  fn parses_use_glob() {
    assert_use_tree(
      "use foo::*;",
      UseTree::Path {
        prefix: "foo".to_string(),
        suffix: Box::new(UseTree::Glob),
      },
    );
  }

  #[test]
  fn parses_use_list() {
    assert_use_tree(
      "use foo::{bar, baz};",
      UseTree::Path {
        prefix: "foo".to_string(),
        suffix: Box::new(UseTree::List(vec![
          UseTree::Name("bar".to_string()),
          UseTree::Name("baz".to_string()),
        ])),
      },
    );
  }

  #[test]
  fn parses_use_list_with_path_and_rename() {
    assert_use_tree(
      "use foo::{bar::baz, qux as quux};",
      UseTree::Path {
        prefix: "foo".to_string(),
        suffix: Box::new(UseTree::List(vec![
          UseTree::Path {
            prefix: "bar".to_string(),
            suffix: Box::new(UseTree::Name("baz".to_string())),
          },
          UseTree::Rename {
            name: "qux".to_string(),
            alias: Ident::Name("quux".to_string()),
          },
        ])),
      },
    );
  }

  #[test]
  fn parses_use_rename() {
    assert_use_tree(
      "use foo as bar;",
      UseTree::Rename {
        name: "foo".to_string(),
        alias: Ident::Name("bar".to_string()),
      },
    );
  }

  #[test]
  fn parses_use_with_attributes_and_visibility() {
    let vis = parse_use_item("#[cfg(test)] pub use foo;").unwrap();
    assert!(!vis.attributes.is_empty());
    assert_eq!(vis.visibility, Visibility::Public);
  }

  #[test]
  fn rejects_missing_semicolon() {
    assert_use_err("use foo");
  }

  #[test]
  fn rejects_missing_suffix() {
    assert_use_err("use foo::");
  }
}
