#[cfg(test)]
mod const_tests {
  use crate::{
    ast::{expr::ExprKind, ConstDecl, Ident, Item, Lit, Stmt, Type, VisItemKind, Visibility},
    parser_utils::ParserContext,
    tests::support::{parse_item, run_parser},
  };

  fn parse_const_item(input: &str) -> Result<Item, ()> {
    parse_item(input, "const_item_test_temp", ParserContext::Default)
  }

  fn parse_stmt(input: &str) -> Result<Stmt, ()> {
    run_parser(input, "const_stmt_test_temp", |parser| {
      parser.parse_stmt(ParserContext::Default)
    })
  }

  fn assert_const_decl(decl: &ConstDecl, expected_name: &str, expected_value: i128) {
    assert_eq!(decl.name, Ident::Name(expected_name.to_string()));
    assert_eq!(decl.ty, Type::I32);
    match &decl.value.kind {
      ExprKind::Literal(Lit::Integer { value, .. }) => assert_eq!(*value, expected_value),
      other => panic!("expected integer literal const value, got: {:?}", other),
    }
  }

  #[test]
  fn const_item_parses_simple() {
    let item = parse_const_item("const FOO: i32 = 1;").unwrap();
    match item {
      Item::Vis(vis) => match vis.kind {
        VisItemKind::Const(decl) => assert_const_decl(&decl, "FOO", 1),
        other => panic!("expected const item, got: {:?}", other),
      },
      other => panic!("expected vis item, got: {:?}", other),
    }
  }

  #[test]
  fn const_item_with_visibility() {
    let item = parse_const_item("pub const FOO: i32 = 1;").unwrap();
    match item {
      Item::Vis(vis) => {
        assert_eq!(vis.visibility, Visibility::Public);
        match vis.kind {
          VisItemKind::Const(decl) => assert_const_decl(&decl, "FOO", 1),
          other => panic!("expected const item, got: {:?}", other),
        }
      },
      other => panic!("expected vis item, got: {:?}", other),
    }
  }

  #[test]
  fn const_item_with_attribute() {
    let item = parse_const_item("#[attr] const FOO: i32 = 1;").unwrap();
    match item {
      Item::Vis(vis) => {
        assert_eq!(vis.attributes.len(), 1);
        match vis.kind {
          VisItemKind::Const(decl) => assert_const_decl(&decl, "FOO", 1),
          other => panic!("expected const item, got: {:?}", other),
        }
      },
      other => panic!("expected vis item, got: {:?}", other),
    }
  }

  #[test]
  fn const_item_as_statement() {
    let stmt = parse_stmt("const FOO: i32 = 1;").unwrap();
    match stmt {
      Stmt::Item(item) => match *item {
        Item::Vis(vis) => match vis.kind {
          VisItemKind::Const(decl) => assert_const_decl(&decl, "FOO", 1),
          other => panic!("expected const item, got: {:?}", other),
        },
        other => panic!("expected vis item, got: {:?}", other),
      },
      other => panic!("expected item statement, got: {:?}", other),
    }
  }

  #[test]
  fn const_item_missing_initializer_errors() {
    assert!(parse_const_item("const FOO: i32;").is_err());
  }

  #[test]
  fn const_item_missing_semicolon_errors() {
    assert!(parse_const_item("const FOO: i32 = 1").is_err());
  }
}
