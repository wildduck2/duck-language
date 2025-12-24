#[cfg(test)]
mod generics_tests {
  use crate::{
    ast::{
      expr::ExprKind,
      generic::{GenericArg, GenericArgs, GenericParam, TraitBoundModifier, TypeBound},
      path::{Path, PathSegmentKind},
      Item, Lit, Type, VisItemKind,
    },
    parser_utils::ParserContext,
    tests::support::{parse_expression, parse_item},
  };

  fn parse_fn_generics(input: &str) -> crate::ast::GenericParams {
    let item = parse_item(input, "generics_fn_test_temp", ParserContext::Default).unwrap();
    match item {
      Item::Vis(vis) => match vis.kind {
        VisItemKind::Function(decl) => decl.sig.generics.expect("expected generics"),
        other => panic!("expected function item, got: {:?}", other),
      },
      other => panic!("expected vis item, got: {:?}", other),
    }
  }

  fn parse_type_param_bounds(input: &str) -> Vec<TypeBound> {
    let generics = parse_fn_generics(input);
    match &generics.params[0] {
      GenericParam::Type { bounds, .. } => bounds.clone(),
      other => panic!("expected type param, got: {:?}", other),
    }
  }

  fn parse_path_args(input: &str) -> GenericArgs {
    let expr = parse_expression(input, "generics_expr_test_temp", ParserContext::Default).unwrap();
    match expr {
      ExprKind::Path { path, .. } => match path.segments[0].args.clone() {
        Some(args) => args,
        None => panic!("expected generic args on path"),
      },
      other => panic!("expected path expression, got: {:?}", other),
    }
  }

  fn parse_method_turbofish(input: &str) -> GenericArgs {
    let expr =
      parse_expression(input, "generics_method_test_temp", ParserContext::Default).unwrap();
    match expr {
      ExprKind::MethodCall { turbofish, .. } => turbofish.expect("expected turbofish args"),
      other => panic!("expected method call, got: {:?}", other),
    }
  }

  fn assert_expr_err(input: &str) {
    assert!(
      parse_expression(input, "generics_expr_err_temp", ParserContext::Default).is_err(),
      "expected error for {input:?}"
    );
  }

  fn assert_item_err(input: &str) {
    assert!(
      parse_item(input, "generics_item_err_temp", ParserContext::Default).is_err(),
      "expected error for {input:?}"
    );
  }

  fn assert_ident_path(path: &Path, expected: &str) {
    assert!(!path.leading_colon);
    assert_eq!(path.segments.len(), 1);
    match &path.segments[0].kind {
      PathSegmentKind::Ident(name) => assert_eq!(name, expected),
      other => panic!("expected ident path, got: {:?}", other),
    }
  }

  fn assert_type_path(ty: &Type, expected: &str) {
    match ty {
      Type::Path(path) => assert_ident_path(path, expected),
      other => panic!("expected path type, got: {:?}", other),
    }
  }

  #[test]
  fn parses_all_generic_param_kinds_and_defaults() {
    let generics =
      parse_fn_generics("fn foo<'a: 'b + 'c, T: Copy + 'a = i32, const N: usize = 3>() {}");
    assert_eq!(generics.params.len(), 3);

    match &generics.params[0] {
      GenericParam::Lifetime { name, bounds, .. } => {
        assert_eq!(name, "'a");
        assert_eq!(bounds, &vec!["'b".to_string(), "'c".to_string()]);
      },
      other => panic!("expected lifetime param, got: {:?}", other),
    }

    match &generics.params[1] {
      GenericParam::Type {
        name,
        bounds,
        default,
        ..
      } => {
        assert_eq!(name, "T");
        assert_eq!(default, &Some(Type::I32));
        assert_eq!(bounds.len(), 2);

        match &bounds[0] {
          TypeBound::Trait {
            modifier,
            for_lifetimes,
            path,
          } => {
            assert_eq!(modifier, &TraitBoundModifier::None);
            assert!(for_lifetimes.is_none());
            assert_ident_path(path, "Copy");
          },
          other => panic!("expected trait bound, got: {:?}", other),
        }

        match &bounds[1] {
          TypeBound::Lifetime { name } => assert_eq!(name, "'a"),
          other => panic!("expected lifetime bound, got: {:?}", other),
        }
      },
      other => panic!("expected type param, got: {:?}", other),
    }

    match &generics.params[2] {
      GenericParam::Const {
        name, ty, default, ..
      } => {
        assert_eq!(name, "N");
        assert_eq!(ty, &Type::Usize);
        match default {
          Some(expr) => match &expr.kind {
            ExprKind::Literal(Lit::Integer { value, .. }) => assert_eq!(*value, 3),
            other => panic!("expected integer default, got: {:?}", other),
          },
          None => panic!("expected const default"),
        }
      },
      other => panic!("expected const param, got: {:?}", other),
    }
  }

  #[test]
  fn allows_trailing_comma_in_generic_params() {
    let generics = parse_fn_generics("fn foo<T,>() {}");
    assert_eq!(generics.params.len(), 1);
  }

  #[test]
  fn parses_const_param_without_default() {
    let generics = parse_fn_generics("fn foo<const N: usize>() {}");
    match &generics.params[0] {
      GenericParam::Const { default, .. } => assert!(default.is_none()),
      other => panic!("expected const param, got: {:?}", other),
    }
  }

  #[test]
  fn parses_trait_bounds_with_modifiers() {
    let bounds = parse_type_param_bounds("fn foo<T: ?Sized + ~const Copy + ?~const Clone>() {}");
    assert_eq!(bounds.len(), 3);

    match &bounds[0] {
      TypeBound::Trait { modifier, path, .. } => {
        assert_eq!(modifier, &TraitBoundModifier::Maybe);
        assert_ident_path(path, "Sized");
      },
      other => panic!("expected trait bound, got: {:?}", other),
    }

    match &bounds[1] {
      TypeBound::Trait { modifier, path, .. } => {
        assert_eq!(modifier, &TraitBoundModifier::Const);
        assert_ident_path(path, "Copy");
      },
      other => panic!("expected trait bound, got: {:?}", other),
    }

    match &bounds[2] {
      TypeBound::Trait { modifier, path, .. } => {
        assert_eq!(modifier, &TraitBoundModifier::MaybeConst);
        assert_ident_path(path, "Clone");
      },
      other => panic!("expected trait bound, got: {:?}", other),
    }
  }

  #[test]
  fn parses_trait_bounds_with_for_lifetimes() {
    let bounds = parse_type_param_bounds("fn foo<T: for<'a> Trait>() {}");
    match &bounds[0] {
      TypeBound::Trait { for_lifetimes, .. } => {
        assert_eq!(for_lifetimes.as_ref().unwrap(), &vec!["'a".to_string()]);
      },
      other => panic!("expected trait bound, got: {:?}", other),
    }
  }

  #[test]
  fn rejects_invalid_trait_bound_modifiers() {
    assert_item_err("fn foo<T: ~Foo>() {}");
    assert_item_err("fn foo<T: ?~Foo>() {}");
  }

  #[test]
  fn rejects_trailing_plus_in_bounds() {
    assert_item_err("fn foo<T: Copy +>() {}");
  }

  #[test]
  fn rejects_invalid_bound_after_plus() {
    assert_item_err("fn foo<T: Copy + *>() {}");
  }

  #[test]
  fn rejects_invalid_generic_param_token() {
    assert_item_err("fn foo<,>() {}");
  }

  #[test]
  fn parses_generic_args_types_lifetimes_and_consts() {
    let args = parse_path_args("Foo::<T>");
    match args {
      GenericArgs::AngleBracketed { args } => {
        assert_eq!(args.len(), 1);
        match &args[0] {
          GenericArg::Type(ty) => assert_type_path(ty, "T"),
          other => panic!("expected type arg, got: {:?}", other),
        }
      },
      other => panic!("expected angle bracketed args, got: {:?}", other),
    }

    let args = parse_path_args("Foo::<'a>");
    match args {
      GenericArgs::AngleBracketed { args } => match &args[0] {
        GenericArg::Lifetime(name) => assert_eq!(name, "'a"),
        other => panic!("expected lifetime arg, got: {:?}", other),
      },
      other => panic!("expected angle bracketed args, got: {:?}", other),
    }

    let args = parse_path_args("Foo::<3>");
    match args {
      GenericArgs::AngleBracketed { args } => match &args[0] {
        GenericArg::Const(expr) => match &expr.kind {
          ExprKind::Literal(Lit::Integer { value, .. }) => assert_eq!(*value, 3),
          other => panic!("expected integer const arg, got: {:?}", other),
        },
        other => panic!("expected const arg, got: {:?}", other),
      },
      other => panic!("expected angle bracketed args, got: {:?}", other),
    }
  }

  #[test]
  fn parses_const_block_generic_arg() {
    let args = parse_path_args("Foo::<{ 1 }>");
    match args {
      GenericArgs::AngleBracketed { args } => match &args[0] {
        GenericArg::Const(expr) => match &expr.kind {
          ExprKind::Block { .. } => {},
          other => panic!("expected block const arg, got: {:?}", other),
        },
        other => panic!("expected const arg, got: {:?}", other),
      },
      other => panic!("expected angle bracketed args, got: {:?}", other),
    }
  }

  #[test]
  fn parses_binding_and_constraint_args() {
    let args = parse_path_args("Foo::<Item = i32, Assoc: Copy>");
    match args {
      GenericArgs::AngleBracketed { args } => {
        assert_eq!(args.len(), 2);
        match &args[0] {
          GenericArg::Binding { name, args, ty } => {
            assert_eq!(name, "Item");
            assert!(args.is_none());
            assert_eq!(ty, &Type::I32);
          },
          other => panic!("expected binding arg, got: {:?}", other),
        }

        match &args[1] {
          GenericArg::Constraint { name, args, bounds } => {
            assert_eq!(name, "Assoc");
            assert!(args.is_none());
            assert_eq!(bounds.len(), 1);
            match &bounds[0] {
              TypeBound::Trait { path, .. } => assert_ident_path(path, "Copy"),
              other => panic!("expected trait bound, got: {:?}", other),
            }
          },
          other => panic!("expected constraint arg, got: {:?}", other),
        }
      },
      other => panic!("expected angle bracketed args, got: {:?}", other),
    }
  }

  #[test]
  fn parses_binding_and_constraint_with_args() {
    let args = parse_path_args("Foo::<Item<T> = i32, Assoc<U>: Copy>");
    match args {
      GenericArgs::AngleBracketed { args } => {
        assert_eq!(args.len(), 2);
        match &args[0] {
          GenericArg::Binding { name, args, .. } => {
            assert_eq!(name, "Item");
            match args {
              Some(GenericArgs::AngleBracketed { args }) => match &args[0] {
                GenericArg::Type(ty) => assert_type_path(ty, "T"),
                other => panic!("expected type arg, got: {:?}", other),
              },
              other => panic!("expected angle bracketed args, got: {:?}", other),
            }
          },
          other => panic!("expected binding arg, got: {:?}", other),
        }

        match &args[1] {
          GenericArg::Constraint { name, args, .. } => {
            assert_eq!(name, "Assoc");
            match args {
              Some(GenericArgs::AngleBracketed { args }) => match &args[0] {
                GenericArg::Type(ty) => assert_type_path(ty, "U"),
                other => panic!("expected type arg, got: {:?}", other),
              },
              other => panic!("expected angle bracketed args, got: {:?}", other),
            }
          },
          other => panic!("expected constraint arg, got: {:?}", other),
        }
      },
      other => panic!("expected angle bracketed args, got: {:?}", other),
    }
  }

  #[test]
  fn rejects_invalid_angle_bracketed_generic_args() {
    for src in ["Foo::<>", "Foo::<T U>", "Foo::<T,>", "Foo::<*>"] {
      assert_expr_err(src);
    }
  }

  #[test]
  fn rejects_unit_and_slice_generic_args() {
    assert_expr_err("Foo::<()>");
    assert_expr_err("Foo::<[u8]>");
  }

  #[test]
  fn parses_parenthesized_generic_args_on_method_call() {
    let args = parse_method_turbofish("foo.bar::(i32, u8) -> bool(1, 2)");
    match args {
      GenericArgs::Parenthesized { inputs, output } => {
        assert_eq!(inputs, vec![Type::I32, Type::U8]);
        assert_eq!(output, Some(Box::new(Type::Bool)));
      },
      other => panic!("expected parenthesized args, got: {:?}", other),
    }
  }

  #[test]
  fn rejects_parenthesized_generic_args_errors() {
    for src in [
      "foo.bar::() -> i32()",
      "foo.bar::(i32,) -> i32()",
      "foo.bar::(i32, -> i32)()",
    ] {
      assert_expr_err(src);
    }
  }
}
