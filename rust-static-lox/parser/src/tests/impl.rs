#[cfg(test)]
mod impl_tests {
  use crate::{
    ast::{
      expr::ExprKind, ImplBlock, ImplItem, ImplPolarity, Item, Lit, Type, VisItem, VisItemKind,
      Visibility,
    },
    parser_utils::ParserContext,
    tests::support::{parse_item, run_parser, simple_path, simplify_path},
  };
  fn parse_impl_item(input: &str) -> Result<VisItem, ()> {
    parse_item(input, "impl_test_temp", ParserContext::Default).and_then(|item| match item {
      Item::Vis(vis) => Ok(vis),
      _ => Err(()),
    })
  }

  fn impl_block(vis: &VisItem) -> &ImplBlock {
    match &vis.kind {
      VisItemKind::Impl(block) => block,
      other => panic!("expected impl item, got: {:?}", other),
    }
  }

  fn assert_impl_err(input: &str) {
    assert!(
      parse_impl_item(input).is_err(),
      "expected error for {input:?}"
    );
  }

  fn assert_impl_decl_err(input: &str) {
    let result = run_parser(input, "impl_decl_err", |parser| {
      parser.parse_impl_decl(vec![], Visibility::Private, ParserContext::Default)
    });
    assert!(result.is_err(), "expected impl decl error for {input:?}");
  }

  fn assert_type_path(ty: &Type, segments: &[&str]) {
    match ty {
      Type::Path(path) => {
        assert_eq!(simplify_path(path), simple_path(segments.iter().copied()));
      },
      other => panic!("expected path type, got: {:?}", other),
    }
  }

  #[test]
  fn inherent_impl_with_generics_empty_body() {
    let vis = parse_impl_item("impl<T> Foo<T> {}").unwrap();
    assert_eq!(vis.visibility, Visibility::Private);
    let block = impl_block(&vis);
    assert!(!block.is_unsafe);
    assert!(!block.is_const);
    assert_eq!(block.polarity, ImplPolarity::Positive);
    assert!(block.trait_ref.is_none());
    assert!(block.generics.is_some());
    assert!(block.where_clause.is_none());
    assert!(block.inner_attributes.is_empty());
    assert!(block.items.is_empty());
    assert_type_path(&block.self_ty, &["Foo"]);
  }

  #[test]
  fn trait_impl_with_const_negative_and_where() {
    let vis =
      parse_impl_item("#[attr] pub unsafe impl const !Trait for Foo where Foo: Copy { fn f() {} }")
        .unwrap();
    assert_eq!(vis.visibility, Visibility::Public);
    assert_eq!(vis.attributes.len(), 1);
    let block = impl_block(&vis);
    assert!(block.is_unsafe);
    assert!(block.is_const);
    assert_eq!(block.polarity, ImplPolarity::Negative);
    assert!(block.generics.is_none());
    assert!(block.where_clause.is_some());

    let trait_ref = block.trait_ref.as_ref().expect("expected trait ref");
    assert_eq!(simplify_path(trait_ref), simple_path(["Trait"]));
    assert_type_path(&block.self_ty, &["Foo"]);

    assert_eq!(block.items.len(), 1);
    match &block.items[0] {
      ImplItem::Method(func) => assert_eq!(func.sig.name.as_str(), "f"),
      other => panic!("expected method item, got: {:?}", other),
    }
  }

  #[test]
  fn inherent_impl_with_items_and_outer_attributes() {
    let src = r#"impl Foo {
  fn pre() {}
  #[const_attr]
  pub const A: i32 = 1;
  fn f() {}
  foo!()
}"#;
    let vis = parse_impl_item(src).unwrap();
    let block = impl_block(&vis);
    assert!(block.inner_attributes.is_empty());
    assert_eq!(block.items.len(), 4);

    match &block.items[0] {
      ImplItem::Method(func) => assert_eq!(func.sig.name.as_str(), "pre"),
      other => panic!("expected method item, got: {:?}", other),
    }

    match &block.items[1] {
      ImplItem::Const {
        attributes,
        visibility,
        name,
        ty,
        value,
        ..
      } => {
        assert_eq!(attributes.len(), 1);
        assert_eq!(visibility, &Visibility::Public);
        assert_eq!(name.as_str(), "A");
        assert_eq!(ty, &Type::I32);
        match &value.kind {
          ExprKind::Literal(Lit::Integer { value, .. }) => assert_eq!(*value, 1),
          other => panic!("expected integer literal, got: {:?}", other),
        }
      },
      other => panic!("expected associated const item, got: {:?}", other),
    }

    match &block.items[2] {
      ImplItem::Method(func) => assert_eq!(func.sig.name.as_str(), "f"),
      other => panic!("expected method item, got: {:?}", other),
    }

    match &block.items[3] {
      ImplItem::Macro { mac } => {
        assert_eq!(simplify_path(&mac.path), simple_path(["foo"]));
      },
      other => panic!("expected macro item, got: {:?}", other),
    }
  }


  #[test]
  fn inherent_impl_with_inner_attribute_only() {
    let src = r#"impl Foo {
  #![inner]
  fn f() {}
}"#;
    let vis = parse_impl_item(src).unwrap();
    let block = impl_block(&vis);
    assert_eq!(block.inner_attributes.len(), 1);
    assert_eq!(block.items.len(), 1);

    match &block.items[0] {
      ImplItem::Method(func) => assert_eq!(func.sig.name.as_str(), "f"),
      other => panic!("expected method item, got: {:?}", other),
    }
  }

  #[test]
  fn trait_impl_allows_associated_type() {
    let vis = parse_impl_item("impl Trait for Foo { type Assoc = i32; }").unwrap();
    let block = impl_block(&vis);
    assert_eq!(block.items.len(), 1);
    match &block.items[0] {
      ImplItem::Type { name, ty, .. } => {
        assert_eq!(name.as_str(), "Assoc");
        assert_eq!(ty, &Type::I32);
      },
      other => panic!("expected associated type item, got: {:?}", other),
    }
  }

  #[test]
  fn impl_decl_missing_impl_keyword_errors() {
    assert_impl_decl_err("fn f() {}");
  }

  #[test]
  fn impl_decl_invalid_generics_errors() {
    assert_impl_err("impl<,> Foo {}");
  }

  #[test]
  fn impl_decl_trait_self_type_errors() {
    assert_impl_err("impl Trait for {}");
  }

  #[test]
  fn impl_decl_inherent_self_type_errors() {
    assert_impl_err("impl {}");
  }

  #[test]
  fn impl_decl_where_clause_errors() {
    assert_impl_err("impl Foo where {}");
  }

  #[test]
  fn impl_decl_missing_body_errors() {
    assert_impl_err("impl Foo");
  }

  #[test]
  fn impl_decl_invalid_inner_attribute_errors() {
    assert_impl_err("impl Foo { # }");
  }

  #[test]
  fn inherent_impl_type_rejects_non_ident_name() {
    assert_impl_err("impl Foo { type self = i32; }");
  }

  #[test]
  fn inherent_impl_type_missing_eq_errors() {
    assert_impl_err("impl Foo { type Assoc: Trait where Self: Copy; }");
  }

  #[test]
  fn inherent_impl_type_name_parse_errors() {
    assert_impl_err("impl Foo { type 123 = i32; }");
  }

  #[test]
  fn inherent_impl_type_generic_params_errors() {
    assert_impl_err("impl Foo { type Assoc<,> = i32; }");
  }

  #[test]
  fn inherent_impl_type_bounds_type_errors() {
    assert_impl_err("impl Foo { type Assoc: = i32; }");
  }

  #[test]
  fn inherent_impl_type_where_clause_errors() {
    assert_impl_err("impl Foo { type Assoc where = i32; }");
  }

  #[test]
  fn inherent_impl_type_default_type_errors() {
    assert_impl_err("impl Foo { type Assoc = ; }");
  }

  #[test]
  fn inherent_impl_type_missing_semi_errors() {
    assert_impl_err("impl Foo { type Assoc = i32 }");
  }

  #[test]
  fn inherent_impl_const_name_parse_errors() {
    assert_impl_err("impl Foo { const 123: i32 = 1; }");
  }

  #[test]
  fn inherent_impl_const_type_errors() {
    assert_impl_err("impl Foo { const A: = 1; }");
  }

  #[test]
  fn inherent_impl_const_value_errors() {
    assert_impl_err("impl Foo { const A: i32 = ; }");
  }

  #[test]
  fn inherent_impl_const_missing_eq_errors() {
    assert_impl_err("impl Foo { const A: i32; }");
  }

  #[test]
  fn inherent_impl_const_missing_semi_errors() {
    assert_impl_err("impl Foo { const A: i32 = 1 }");
  }

  #[test]
  fn inherent_impl_const_missing_colon_errors() {
    assert_impl_err("impl Foo { const A i32 = 1; }");
  }

  #[test]
  fn inherent_impl_method_missing_body_errors() {
    assert_impl_err("impl Foo { fn f() }");
  }

  #[test]
  fn inherent_impl_invalid_visibility_errors() {
    assert_impl_err("impl Foo { pub(foo) fn f() {} }");
  }

  #[test]
  fn inherent_impl_outer_attribute_parse_errors() {
    assert_impl_err("impl Foo { fn f() {} # }");
  }

  #[test]
  fn inherent_impl_macro_missing_bang_errors() {
    assert_impl_err("impl Foo { foo() }");
  }

  #[test]
  fn inherent_impl_macro_path_errors() {
    assert_impl_err("impl Foo { foo::!(); }");
  }

  #[test]
  fn inherent_impl_macro_invocation_errors() {
    assert_impl_err("impl Foo { foo!; }");
  }

  #[test]
  fn inherent_impl_macro_trailing_semi_errors() {
    assert_impl_err("impl Foo { foo!(); }");
  }

  #[test]
  fn impl_body_unexpected_item_errors() {
    assert_impl_err("impl Foo { ; }");
  }

  #[test]
  fn impl_body_missing_closing_brace_errors() {
    assert_impl_err("impl Foo { fn f() {}");
  }

  #[test]
  fn unsafe_fn_is_not_impl_item() {
    let item = parse_item(
      "unsafe fn foo() {}",
      "impl_unsafe_fn_test_temp",
      ParserContext::Default,
    )
    .unwrap();
    match item {
      Item::Vis(vis) => match vis.kind {
        VisItemKind::Function(_) => {},
        other => panic!("expected function item, got: {:?}", other),
      },
      other => panic!("expected vis item, got: {:?}", other),
    }
  }
}
