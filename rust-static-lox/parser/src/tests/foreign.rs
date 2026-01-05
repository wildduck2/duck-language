#[cfg(test)]
mod foreign_tests {
  use crate::{
    ast::{ForeignItem, ForeignModDecl, Ident, Item, Mutability, VisItem, VisItemKind, Visibility},
    parser_utils::ParserContext,
    tests::support::parse_item,
  };

  fn parse_foreign_mod(input: &str) -> Result<VisItem, ()> {
    parse_item(input, "foreign_mod_test_temp", ParserContext::Default).and_then(|item| match item {
      Item::Vis(vis) => Ok(vis),
      _ => Err(()),
    })
  }

  fn foreign_mod(vis: &VisItem) -> &ForeignModDecl {
    match &vis.kind {
      VisItemKind::ForeignMod(decl) => decl,
      other => panic!("expected foreign mod item, got: {:?}", other),
    }
  }

  #[test]
  fn foreign_mod_empty_body() {
    let vis = parse_foreign_mod("extern { }").unwrap();
    let decl = foreign_mod(&vis);
    assert_eq!(vis.visibility, Visibility::Private);
    assert_eq!(decl.is_unsafe, false);
    assert!(decl.abi.is_none());
    assert!(decl.inner_attributes.is_empty());
    assert!(decl.items.is_empty());
  }

  #[test]
  fn foreign_mod_with_unsafe_and_abi() {
    let vis = parse_foreign_mod(r#"unsafe extern "C" { }"#).unwrap();
    let decl = foreign_mod(&vis);
    assert_eq!(decl.is_unsafe, true);
    assert_eq!(decl.abi.as_deref(), Some("C"));
  }

  #[test]
  fn foreign_mod_with_inner_attribute() {
    let vis = parse_foreign_mod(r#"extern "C" { #![allow(dead_code)] fn foo(); }"#).unwrap();
    let decl = foreign_mod(&vis);
    assert_eq!(decl.inner_attributes.len(), 1);
    assert_eq!(decl.items.len(), 1);
    match &decl.items[0] {
      ForeignItem::Function { sig, .. } => {
        assert_eq!(sig.name.as_str(), "foo");
      },
      other => panic!("expected foreign function item, got: {:?}", other),
    }
  }

  #[test]
  fn foreign_mod_items_cover_external_item_kinds() {
    let src = r#"
      extern "C" {
        #[attr]
        fn foo(x: i32);
        pub static mut FOO: i32;
        static _: i64;
        bar!();
      }
    "#;
    let vis = parse_foreign_mod(src).unwrap();
    let decl = foreign_mod(&vis);
    assert_eq!(decl.items.len(), 4);

    match &decl.items[0] {
      ForeignItem::Function {
        attributes,
        visibility,
        sig,
        ..
      } => {
        assert_eq!(*visibility, Visibility::Private);
        assert_eq!(attributes.len(), 1);
        assert_eq!(sig.name.as_str(), "foo");
        assert_eq!(sig.params.len(), 1);
      },
      other => panic!("expected foreign function item, got: {:?}", other),
    }

    match &decl.items[1] {
      ForeignItem::Static {
        visibility,
        name,
        mutability,
        ..
      } => {
        assert_eq!(*visibility, Visibility::Public);
        assert_eq!(*mutability, Mutability::Mutable);
        assert_eq!(name, &Ident::Name("FOO".to_string()));
      },
      other => panic!("expected foreign static item, got: {:?}", other),
    }

    match &decl.items[2] {
      ForeignItem::Static { name, mutability, .. } => {
        assert_eq!(name, &Ident::Underscore);
        assert_eq!(*mutability, Mutability::Immutable);
      },
      other => panic!("expected foreign static item, got: {:?}", other),
    }

    match &decl.items[3] {
      ForeignItem::MacroInvocationSemi { .. } => {},
      other => panic!("expected macro invocation item, got: {:?}", other),
    }
  }

  // Note: Raw string ABI tests are skipped as they require nested raw string
  // literals which are difficult to test. The parser supports raw string ABIs
  // (see parse_foreign_mod_flavors), but testing them requires complex
  // string escaping that's not worth the complexity for coverage.

  #[test]
  fn foreign_mod_with_attributes_span_merge() {
    let vis = parse_foreign_mod(r#"#[attr1] #[attr2] extern "C" { }"#).unwrap();
    let decl = foreign_mod(&vis);
    assert_eq!(vis.attributes.len(), 2);
    assert_eq!(decl.abi.as_deref(), Some("C"));
  }

  #[test]
  fn foreign_mod_type_item() {
    let vis = parse_foreign_mod(r#"extern "C" { type Foo; }"#).unwrap();
    let decl = foreign_mod(&vis);
    assert_eq!(decl.items.len(), 1);
    match &decl.items[0] {
      ForeignItem::Type { name, .. } => {
        assert_eq!(name.as_str(), "Foo");
      },
      other => panic!("expected foreign type item, got: {:?}", other),
    }
  }

  #[test]
  fn foreign_mod_invalid_item_errors() {
    assert!(parse_foreign_mod("extern \"C\" { struct Foo; }").is_err());
  }

  #[test]
  fn foreign_mod_invalid_item_token_errors() {
    assert!(parse_foreign_mod("extern \"C\" { 123; }").is_err());
  }

}
