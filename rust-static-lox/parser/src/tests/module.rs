#[cfg(test)]
mod module_tests {
  use crate::{
    ast::{ExprKind, Item, ModuleDecl, VisItem, VisItemKind, Visibility},
    parser_utils::ParserContext,
    tests::support::parse_item,
  };

  fn parse_module_item(input: &str) -> Result<VisItem, ()> {
    parse_item(input, "module_test_temp", ParserContext::Default).and_then(|item| match item {
      Item::Vis(vis) => Ok(vis),
      _ => Err(()),
    })
  }

  fn get_module(vis: &VisItem) -> &ModuleDecl {
    match &vis.kind {
      VisItemKind::Module(decl) => decl,
      other => panic!("expected module item, got: {:?}", other),
    }
  }

  #[test]
  fn module_decl_with_semicolon() {
    let vis = parse_module_item("mod foo;").unwrap();
    let decl = get_module(&vis);
    assert_eq!(decl.name.as_str(), "foo");
    assert!(decl.body.is_none());
  }

  #[test]
  fn module_decl_with_empty_body() {
    let vis = parse_module_item("mod foo {}").unwrap();
    let decl = get_module(&vis);
    let body = decl.body.as_ref().expect("expected module body");
    assert_eq!(decl.name.as_str(), "foo");
    assert!(body.items.is_empty());
  }

  #[test]
  fn module_decl_with_items() {
    let vis = parse_module_item("mod foo { const A: i32 = 1; fn f() {} }").unwrap();
    let decl = get_module(&vis);
    let body = decl.body.as_ref().expect("expected module body");
    assert_eq!(body.items.len(), 2);

    match &body.items[0] {
      Item::Vis(vis) => match &vis.kind {
        VisItemKind::Const(_) => {},
        other => panic!("expected const item, got: {:?}", other),
      },
      other => panic!("expected vis item, got: {:?}", other),
    }

    match &body.items[1] {
      Item::Vis(vis) => match &vis.kind {
        VisItemKind::Function(_) => {},
        other => panic!("expected function item, got: {:?}", other),
      },
      other => panic!("expected vis item, got: {:?}", other),
    }
  }

  #[test]
  fn module_decl_with_attribute_and_visibility() {
    let vis = parse_module_item("#[attr] pub mod foo {}").unwrap();
    let decl = get_module(&vis);
    assert_eq!(decl.name.as_str(), "foo");
    assert_eq!(vis.visibility, Visibility::Public);
    assert_eq!(vis.attributes.len(), 1);
  }

  #[test]
  fn module_decl_missing_body_errors() {
    assert!(parse_module_item("mod foo").is_err());
  }

  #[test]
  fn module_decl_invalid_body_token_errors() {
    assert!(parse_module_item("mod foo = {}").is_err());
  }

  #[test]
  fn module_item_spans_stop_at_inner_items() {
    let src = "mod m { const A: i32 = 1; fn f() {} }";
    let vis = parse_module_item(src).unwrap();
    let decl = get_module(&vis);
    let body = decl.body.as_ref().expect("expected module body");
    assert_eq!(body.items.len(), 2);

    let const_item = match &body.items[0] {
      Item::Vis(vis) => vis,
      other => panic!("expected const item, got: {:?}", other),
    };
    let const_start = src.find("const").unwrap();
    let const_semi = src[const_start..].find(';').unwrap() + const_start;
    let const_end = const_semi + 1;

    match &const_item.kind {
      VisItemKind::Const(decl) => {
        assert_eq!(const_item.span.start, const_start);
        assert_eq!(const_item.span.end, const_end);

        let literal_start = src.find("= 1").unwrap() + 2;
        let literal_end = literal_start + 1;
        assert_eq!(decl.value.span.start, literal_start);
        assert_eq!(decl.value.span.end, literal_end);
      },
      other => panic!("expected const item, got: {:?}", other),
    }

    let fn_item = match &body.items[1] {
      Item::Vis(vis) => vis,
      other => panic!("expected function item, got: {:?}", other),
    };
    let fn_start = src.find("fn f").unwrap();
    let fn_block_start = src[fn_start..].find('{').unwrap() + fn_start;
    let fn_block_end = src[fn_block_start..].find('}').unwrap() + fn_block_start + 1;

    match &fn_item.kind {
      VisItemKind::Function(func) => {
        assert_eq!(fn_item.span.start, fn_start);
        assert_eq!(fn_item.span.end, fn_block_end);

        let body = func.body.as_ref().expect("expected function body");
        match &body.kind {
          ExprKind::Block { .. } => {},
          other => panic!("expected block body, got: {:?}", other),
        }
        assert_eq!(body.span.start, fn_block_start);
        assert_eq!(body.span.end, fn_block_end);
      },
      other => panic!("expected function item, got: {:?}", other),
    }
  }
}
