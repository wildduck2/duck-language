#[cfg(test)]
mod static_tests {
  use crate::{
    ast::{
      expr::ExprKind, Ident, Item, Lit, Mutability, Stmt, Type, VisItem, VisItemKind, Visibility,
    },
    parser_utils::ParserContext,
    tests::support::{parse_item, run_parser},
  };

  fn parse_static_item(input: &str) -> Result<VisItem, ()> {
    parse_item(input, "static_decl_test_temp", ParserContext::Default).and_then(|item| match item {
      Item::Vis(vis) => Ok(vis),
      _ => Err(()),
    })
  }

  fn parse_stmt(input: &str) -> Result<Stmt, ()> {
    run_parser(input, "static_stmt_test_temp", |parser| {
      parser.parse_stmt(ParserContext::Default)
    })
  }

  fn assert_static_decl(
    vis: &VisItem,
    expected_name: Ident,
    expected_mutability: Mutability,
    expected_value: Option<i128>,
  ) {
    match &vis.kind {
      VisItemKind::Static(decl) => {
        assert_eq!(decl.name, expected_name);
        assert_eq!(decl.ty, Type::I32);
        assert_eq!(decl.mutability, expected_mutability);
        match (expected_value, &decl.value) {
          (Some(expected), Some(expr)) => match &expr.kind {
            ExprKind::Literal(Lit::Integer { value, .. }) => assert_eq!(*value, expected),
            other => panic!("expected integer literal static value, got: {:?}", other),
          },
          (None, None) => {},
          (Some(_), None) => panic!("expected static value, got none"),
          (None, Some(_)) => panic!("expected no static value, got some"),
        }
      },
      other => panic!("expected static item, got: {:?}", other),
    }
  }

  fn assert_err(input: &str) {
    assert!(
      parse_static_item(input).is_err(),
      "expected error for {input:?}"
    );
  }

  #[test]
  fn static_item_parses_simple() {
    let vis = parse_static_item("static FOO: i32 = 1;").unwrap();
    assert_eq!(vis.visibility, Visibility::Private);
    assert_static_decl(
      &vis,
      Ident::Name("FOO".to_string()),
      Mutability::Immutable,
      Some(1),
    );
  }

  #[test]
  fn static_item_parses_mutable() {
    let vis = parse_static_item("static mut FOO: i32 = 2;").unwrap();
    assert_static_decl(
      &vis,
      Ident::Name("FOO".to_string()),
      Mutability::Mutable,
      Some(2),
    );
  }

  #[test]
  fn static_item_parses_underscore_name() {
    let vis = parse_static_item("static _: i32 = 3;").unwrap();
    assert_static_decl(&vis, Ident::Underscore, Mutability::Immutable, Some(3));
  }

  #[test]
  fn static_item_without_initializer() {
    let vis = parse_static_item("static FOO: i32;").unwrap();
    assert_static_decl(
      &vis,
      Ident::Name("FOO".to_string()),
      Mutability::Immutable,
      None,
    );
  }

  #[test]
  fn static_item_mut_without_initializer() {
    let vis = parse_static_item("static mut FOO: i32;").unwrap();
    assert_static_decl(
      &vis,
      Ident::Name("FOO".to_string()),
      Mutability::Mutable,
      None,
    );
  }

  #[test]
  fn static_item_with_attribute_and_visibility() {
    let vis = parse_static_item("#[attr] pub static FOO: i32 = 1;").unwrap();
    assert_eq!(vis.visibility, Visibility::Public);
    assert_eq!(vis.attributes.len(), 1);
    assert_static_decl(
      &vis,
      Ident::Name("FOO".to_string()),
      Mutability::Immutable,
      Some(1),
    );
  }

  #[test]
  fn static_item_as_statement() {
    let stmt = parse_stmt("static FOO: i32 = 1;").unwrap();
    match stmt {
      Stmt::Item(item) => match *item {
        Item::Vis(vis) => assert_static_decl(
          &vis,
          Ident::Name("FOO".to_string()),
          Mutability::Immutable,
          Some(1),
        ),
        other => panic!("expected vis item, got: {:?}", other),
      },
      other => panic!("expected item statement, got: {:?}", other),
    }
  }

  #[test]
  fn static_item_missing_name_errors() {
    assert_err("static : i32 = 1;");
  }

  #[test]
  fn static_item_missing_colon_errors() {
    assert_err("static FOO i32 = 1;");
  }

  #[test]
  fn static_item_missing_type_errors() {
    assert_err("static FOO: = 1;");
  }

  #[test]
  fn static_item_missing_value_errors() {
    assert_err("static FOO: i32 = ;");
  }

  #[test]
  fn static_item_missing_semicolon_errors() {
    assert_err("static FOO: i32 = 1");
  }

  #[test]
  fn static_item_const_keyword_errors() {
    assert_err("static const FOO: i32 = 1;");
  }
}
