#[cfg(test)]
mod extern_tests {
  use crate::{
    ast::{ExternCrateDecl, Ident, Item, Stmt, VisItem, VisItemKind, Visibility},
    parser_utils::ParserContext,
    tests::support::{parse_item, run_parser},
  };

  fn parse_extern_item(input: &str) -> Result<VisItem, ()> {
    parse_item(input, "extern_item_test_temp", ParserContext::Default).and_then(|item| match item {
      Item::Vis(vis) => Ok(vis),
      _ => Err(()),
    })
  }

  fn parse_stmt(input: &str) -> Result<Stmt, ()> {
    run_parser(input, "extern_stmt_test_temp", |parser| {
      parser.parse_stmt(ParserContext::Default)
    })
  }

  fn assert_extern_decl(vis: &VisItem, name: &str, alias: Option<Ident>) {
    match &vis.kind {
      VisItemKind::ExternCrate(ExternCrateDecl { name: decl_name, alias: decl_alias }) => {
        assert_eq!(decl_name, &Ident::Name(name.to_string()));
        assert_eq!(decl_alias, &alias);
      },
      other => panic!("expected extern crate item, got: {:?}", other),
    }
  }

  fn assert_err(input: &str) {
    assert!(parse_extern_item(input).is_err(), "expected error for {input:?}");
  }

  #[test]
  fn extern_crate_basic() {
    let vis = parse_extern_item("extern crate foo;").unwrap();
    assert_eq!(vis.visibility, Visibility::Private);
    assert_extern_decl(&vis, "foo", None);
  }

  #[test]
  fn extern_crate_with_alias() {
    let vis = parse_extern_item("extern crate foo as bar;").unwrap();
    assert_extern_decl(&vis, "foo", Some(Ident::Name("bar".to_string())));
  }

  #[test]
  fn extern_crate_with_underscore_alias() {
    let vis = parse_extern_item("extern crate foo as _;").unwrap();
    assert_extern_decl(&vis, "foo", Some(Ident::Underscore));
  }

  #[test]
  fn extern_crate_with_visibility() {
    let vis = parse_extern_item("pub extern crate foo;").unwrap();
    assert_eq!(vis.visibility, Visibility::Public);
    assert_extern_decl(&vis, "foo", None);
  }

  #[test]
  fn extern_crate_with_attribute_and_visibility() {
    let vis = parse_extern_item("#[attr] pub extern crate foo;").unwrap();
    assert_eq!(vis.visibility, Visibility::Public);
    assert_eq!(vis.attributes.len(), 1);
    assert_extern_decl(&vis, "foo", None);
  }

  #[test]
  fn extern_crate_as_statement() {
    let stmt = parse_stmt("extern crate foo;").unwrap();
    match stmt {
      Stmt::Item(item) => match *item {
        Item::Vis(vis) => assert_extern_decl(&vis, "foo", None),
        other => panic!("expected vis item, got: {:?}", other),
      },
      other => panic!("expected item statement, got: {:?}", other),
    }
  }

  #[test]
  fn extern_crate_missing_crate_keyword_errors() {
    assert_err("extern foo;");
  }

  #[test]
  fn extern_crate_missing_name_errors() {
    assert_err("extern crate;");
  }

  #[test]
  fn extern_crate_missing_semicolon_errors() {
    assert_err("extern crate foo");
  }

  #[test]
  fn extern_crate_missing_alias_errors() {
    assert_err("extern crate foo as;");
  }
}
