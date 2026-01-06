#[cfg(test)]
mod trait_tests {
  use crate::{
    ast::{
      expr::ExprKind, Item, Lit, TraitDecl, TraitItem, Type, TypeBound, VisItem, VisItemKind,
      Visibility,
    },
    parser_utils::ParserContext,
    tests::support::{parse_item, simple_path, simplify_path},
  };

  fn parse_trait_item(input: &str) -> Result<VisItem, ()> {
    parse_item(input, "trait_test_temp", ParserContext::Default).and_then(|item| match item {
      Item::Vis(vis) => Ok(vis),
      _ => Err(()),
    })
  }

  fn trait_decl(vis: &VisItem) -> &TraitDecl {
    match &vis.kind {
      VisItemKind::Trait(decl) => decl,
      other => panic!("expected trait item, got: {:?}", other),
    }
  }

  fn assert_trait_err(input: &str) {
    assert!(parse_trait_item(input).is_err(), "expected error for {input:?}");
  }

  fn assert_trait_bound_path(bound: &TypeBound, segments: &[&str]) {
    match bound {
      TypeBound::Trait { path, .. } => {
        assert_eq!(simplify_path(path), simple_path(segments.iter().copied()));
      },
      other => panic!("expected trait bound, got: {:?}", other),
    }
  }

  fn assert_lifetime_bound(bound: &TypeBound, name: &str) {
    match bound {
      TypeBound::Lifetime { name: bound_name } => assert_eq!(bound_name, name),
      other => panic!("expected lifetime bound, got: {:?}", other),
    }
  }

  #[test]
  fn trait_decl_basic() {
    let vis = parse_trait_item("trait Foo {}").unwrap();
    assert_eq!(vis.visibility, Visibility::Private);
    assert!(vis.attributes.is_empty());

    let decl = trait_decl(&vis);
    assert_eq!(decl.name.as_str(), "Foo");
    assert!(!decl.is_auto);
    assert!(!decl.is_unsafe);
    assert!(decl.generics.is_none());
    assert!(decl.supertraits.is_empty());
    assert!(decl.where_clause.is_none());
    assert!(decl.inner_attributes.is_empty());
    assert!(decl.items.is_empty());
  }

  #[test]
  fn unsafe_trait_decl() {
    let vis = parse_trait_item("unsafe trait Foo {}").unwrap();
    let decl = trait_decl(&vis);
    assert!(decl.is_unsafe);
    assert!(!decl.is_auto);
  }

  #[test]
  fn auto_trait_decl() {
    let vis = parse_trait_item("auto trait Foo {}").unwrap();
    let decl = trait_decl(&vis);
    assert!(decl.is_auto);
    assert!(!decl.is_unsafe);
  }

  #[test]
  fn unsafe_auto_trait_with_generics_bounds_where() {
    let vis = parse_trait_item(
      "#[outer] pub unsafe auto trait Mega<'a, T>: Clone + 'a where T: Copy {}",
    )
    .unwrap();
    assert_eq!(vis.visibility, Visibility::Public);
    assert_eq!(vis.attributes.len(), 1);

    let decl = trait_decl(&vis);
    assert!(decl.is_auto);
    assert!(decl.is_unsafe);
    assert_eq!(decl.generics.as_ref().unwrap().params.len(), 2);
    assert_eq!(decl.supertraits.len(), 2);
    assert_trait_bound_path(&decl.supertraits[0], &["Clone"]);
    assert_lifetime_bound(&decl.supertraits[1], "'a");
    assert!(decl.where_clause.as_ref().is_some());
  }

  #[test]
  fn trait_body_items_with_attributes_and_defaults() {
    let src = r#"trait Foo {
  #![inner]
  #[type_attr]
  type Assoc<T>: Clone where T: Copy = i32;
  #[const_attr]
  const ID: i32 = 1;
  fn f();
  fn g() {}
  my_macro!();
}"#;

    let vis = parse_trait_item(src).unwrap();
    let decl = trait_decl(&vis);
    assert_eq!(decl.inner_attributes.len(), 1);
    assert_eq!(decl.items.len(), 5);

    match &decl.items[0] {
      TraitItem::Type {
        attributes,
        name,
        generics,
        bounds,
        where_clause,
        default,
        ..
      } => {
        assert_eq!(attributes.len(), 1);
        assert_eq!(name.as_str(), "Assoc");
        assert_eq!(generics.as_ref().unwrap().params.len(), 1);
        assert_eq!(bounds.len(), 1);
        assert_trait_bound_path(&bounds[0], &["Clone"]);
        assert!(where_clause.is_some());
        assert!(matches!(default, Some(Type::I32)));
      },
      other => panic!("expected associated type item, got: {:?}", other),
    }

    match &decl.items[1] {
      TraitItem::Const {
        attributes,
        name,
        ty,
        default,
        ..
      } => {
        assert_eq!(attributes.len(), 1);
        assert_eq!(name.as_str(), "ID");
        assert_eq!(ty, &Type::I32);
        let expr = default.as_ref().expect("expected const default");
        match &expr.kind {
          ExprKind::Literal(Lit::Integer { value, .. }) => assert_eq!(*value, 1),
          other => panic!("expected integer literal, got: {:?}", other),
        }
      },
      other => panic!("expected const item, got: {:?}", other),
    }

    match &decl.items[2] {
      TraitItem::Method(func) => {
        assert_eq!(func.sig.name.as_str(), "f");
        assert!(func.body.is_none());
      },
      other => panic!("expected method item, got: {:?}", other),
    }

    match &decl.items[3] {
      TraitItem::Method(func) => {
        assert_eq!(func.sig.name.as_str(), "g");
        assert!(func.body.is_some());
      },
      other => panic!("expected method item, got: {:?}", other),
    }

    match &decl.items[4] {
      TraitItem::Macro { mac } => {
        assert_eq!(simplify_path(&mac.path), simple_path(["my_macro"]));
      },
      other => panic!("expected macro item, got: {:?}", other),
    }
  }

  #[test]
  fn trait_assoc_type_without_bounds_or_default() {
    let vis = parse_trait_item("trait Foo { type Assoc; }").unwrap();
    let decl = trait_decl(&vis);
    assert_eq!(decl.items.len(), 1);
    match &decl.items[0] {
      TraitItem::Type {
        generics,
        bounds,
        where_clause,
        default,
        ..
      } => {
        assert!(generics.is_none());
        assert!(bounds.is_empty());
        assert!(where_clause.is_none());
        assert!(default.is_none());
      },
      other => panic!("expected associated type item, got: {:?}", other),
    }
  }

  #[test]
  fn trait_const_without_default() {
    let vis = parse_trait_item("trait Foo { const ID: i32; }").unwrap();
    let decl = trait_decl(&vis);
    assert_eq!(decl.items.len(), 1);
    match &decl.items[0] {
      TraitItem::Const {
        name, ty, default, ..
      } => {
        assert_eq!(name.as_str(), "ID");
        assert_eq!(ty, &Type::I32);
        assert!(default.is_none());
      },
      other => panic!("expected const item, got: {:?}", other),
    }
  }

  #[test]
  fn trait_type_missing_semicolon_errors() {
    assert_trait_err("trait Foo { type Assoc = i32 }");
  }

  #[test]
  fn trait_macro_missing_bang_errors() {
    assert_trait_err("trait Foo { foo() }");
  }

  #[test]
  fn trait_macro_missing_semicolon_errors() {
    assert_trait_err("trait Foo { foo!() }");
  }

  #[test]
  fn trait_body_unexpected_item_errors() {
    assert_trait_err("trait Foo { struct Bar; }");
  }

  #[test]
  fn trait_body_missing_closing_brace_errors() {
    assert_trait_err("trait Foo { fn f();");
  }
}
