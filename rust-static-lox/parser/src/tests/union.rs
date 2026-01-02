#[cfg(test)]
mod union_tests {
  use crate::{
    ast::{FieldDecl, Ident, Item, Stmt, Type, UnionDecl, VisItem, VisItemKind, Visibility},
    parser_utils::ParserContext,
    tests::support::{parse_item, run_parser},
  };

  fn parse_union_item(input: &str) -> Result<VisItem, ()> {
    parse_item(input, "union_decl_test_temp", ParserContext::Default).and_then(|item| match item {
      Item::Vis(vis) => Ok(vis),
      _ => Err(()),
    })
  }

  fn parse_stmt(input: &str) -> Result<Stmt, ()> {
    run_parser(input, "union_stmt_test_temp", |parser| {
      parser.parse_stmt(ParserContext::Default)
    })
  }

  fn get_union_decl(vis: &VisItem) -> &UnionDecl {
    match &vis.kind {
      VisItemKind::Union(decl) => decl,
      other => panic!("expected union item, got: {:?}", other),
    }
  }

  fn assert_field(field: &FieldDecl, name: &str, ty: Type) {
    assert_eq!(field.name.as_str(), name);
    assert_eq!(field.ty, ty);
  }

  fn assert_err(input: &str) {
    assert!(parse_union_item(input).is_err(), "expected error for {input:?}");
  }

  #[test]
  fn union_decl_empty_fields() {
    let vis = parse_union_item("union Foo {}").unwrap();
    let decl = get_union_decl(&vis);
    assert_eq!(decl.name.as_str(), "Foo");
    assert!(decl.fields.is_empty());
  }

  #[test]
  fn union_decl_one_field() {
    let vis = parse_union_item("union Foo { x: i32 }").unwrap();
    let decl = get_union_decl(&vis);
    assert_eq!(decl.fields.len(), 1);
    assert_field(&decl.fields[0], "x", Type::I32);
  }

  #[test]
  fn union_decl_trailing_comma() {
    let vis = parse_union_item("union Foo { x: i32, }").unwrap();
    let decl = get_union_decl(&vis);
    assert_eq!(decl.fields.len(), 1);
  }

  #[test]
  fn union_decl_multiple_fields() {
    let vis = parse_union_item("union Foo { x: i32, y: u8 }").unwrap();
    let decl = get_union_decl(&vis);
    assert_eq!(decl.fields.len(), 2);
    assert_field(&decl.fields[0], "x", Type::I32);
    assert_field(&decl.fields[1], "y", Type::U8);
  }

  #[test]
  fn union_decl_field_attr_and_vis() {
    let vis = parse_union_item("union Foo { #[a] pub x: i32 }").unwrap();
    let decl = get_union_decl(&vis);
    assert_eq!(decl.fields.len(), 1);
    assert_eq!(decl.fields[0].visibility, Visibility::Public);
    assert_eq!(decl.fields[0].attributes.len(), 1);
  }

  #[test]
  fn union_decl_outer_attr_and_pub_vis() {
    let vis = parse_union_item("#[a] pub union Foo { x: i32 }").unwrap();
    assert_eq!(vis.visibility, Visibility::Public);
    assert_eq!(vis.attributes.len(), 1);
    let decl = get_union_decl(&vis);
    assert_eq!(decl.name.as_str(), "Foo");
  }

  #[test]
  fn union_decl_with_generics_and_where_clause() {
    let vis = parse_union_item("union Foo<T> where T: Copy { x: T }").unwrap();
    let decl = get_union_decl(&vis);
    assert!(decl.generics.is_some());
    assert!(decl.where_clause.is_some());
    assert_eq!(decl.fields.len(), 1);
  }

  #[test]
  fn union_decl_as_statement() {
    let stmt = parse_stmt("union Foo { x: i32 }").unwrap();
    match stmt {
      Stmt::Item(item) => match *item {
        Item::Vis(vis) => {
          let decl = get_union_decl(&vis);
          assert_eq!(decl.name.as_str(), "Foo");
        },
        other => panic!("expected vis item, got: {:?}", other),
      },
      other => panic!("expected item statement, got: {:?}", other),
    }
  }

  #[test]
  fn union_decl_missing_name_errors() {
    assert_err("union { x: i32 }");
  }

  #[test]
  fn union_decl_missing_open_brace_errors() {
    assert_err("union Foo x: i32 }");
  }

  #[test]
  fn union_decl_missing_close_brace_errors() {
    assert_err("union Foo { x: i32");
  }

  #[test]
  fn union_decl_missing_colon_errors() {
    assert_err("union Foo { x i32 }");
  }

  #[test]
  fn union_decl_missing_type_errors() {
    assert_err("union Foo { x: }");
  }

  #[test]
  fn union_decl_trailing_semicolon_errors() {
    assert_err("union Foo {};");
  }

  #[test]
  fn union_decl_allows_underscore_name() {
    let vis = parse_union_item("union Foo { _: i32 }").unwrap();
    let decl = get_union_decl(&vis);
    assert_eq!(decl.fields.len(), 1);
    assert_eq!(decl.fields[0].name, Ident::Underscore);
  }
}
