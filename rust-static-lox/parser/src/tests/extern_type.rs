#[cfg(test)]
mod extern_type_tests {
  use crate::{
    ast::{ExternTypeDecl, Item, Stmt, VisItem, VisItemKind, Visibility},
    parser_utils::ParserContext,
    tests::support::{parse_item, run_parser},
  };

  fn parse_extern_type_item(input: &str) -> Result<VisItem, ()> {
    parse_item(input, "extern_type_test_temp", ParserContext::Default).and_then(|item| match item {
      Item::Vis(vis) => Ok(vis),
      _ => Err(()),
    })
  }

  fn parse_extern_type_decl_direct(input: &str) -> Result<Item, ()> {
    run_parser(input, "extern_type_decl_test_temp", |parser| {
      parser.parse_extern_type_decl(Vec::new(), Visibility::Private)
    })
  }

  fn parse_stmt(input: &str) -> Result<Stmt, ()> {
    run_parser(input, "extern_type_stmt_test_temp", |parser| {
      parser.parse_stmt(ParserContext::Default)
    })
  }

  fn assert_extern_type_decl(vis: &VisItem, name: &str) {
    match &vis.kind {
      VisItemKind::ExternType(ExternTypeDecl { name: decl_name }) => {
        assert_eq!(decl_name, name);
      },
      other => panic!("expected extern type item, got: {:?}", other),
    }
  }

  fn assert_err(input: &str) {
    assert!(parse_extern_type_item(input).is_err(), "expected error for {input:?}");
  }

  #[test]
  fn extern_type_basic() {
    let vis = parse_extern_type_item("extern type Foo;").unwrap();
    assert_eq!(vis.visibility, Visibility::Private);
    assert_extern_type_decl(&vis, "Foo");
  }

  #[test]
  fn extern_type_with_attribute_and_visibility() {
    let vis = parse_extern_type_item("#[attr] pub extern type Foo;").unwrap();
    assert_eq!(vis.visibility, Visibility::Public);
    assert_eq!(vis.attributes.len(), 1);
    assert_extern_type_decl(&vis, "Foo");
  }

  #[test]
  fn extern_type_as_statement() {
    let stmt = parse_stmt("extern type Foo;").unwrap();
    match stmt {
      Stmt::Item(item) => match *item {
        Item::Vis(vis) => assert_extern_type_decl(&vis, "Foo"),
        other => panic!("expected vis item, got: {:?}", other),
      },
      other => panic!("expected item statement, got: {:?}", other),
    }
  }

  #[test]
  fn extern_type_missing_name_errors() {
    assert_err("extern type;");
  }

  #[test]
  fn extern_type_missing_extern_keyword_errors() {
    assert!(parse_extern_type_decl_direct("type Foo;").is_err());
  }

  #[test]
  fn extern_type_missing_type_keyword_errors() {
    assert!(parse_extern_type_decl_direct("extern Foo;").is_err());
  }

  #[test]
  fn extern_type_missing_semicolon_errors() {
    assert_err("extern type Foo");
  }

  #[test]
  fn extern_type_with_generics_errors() {
    assert_err("extern type Foo<T>;");
  }
}
