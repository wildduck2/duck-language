#[cfg(test)]
mod struct_tests {
  use lexer::token::TokenKind;

  use crate::{
    ast::{
      expr::ExprKind,
      r#struct::{StructDecl, StructKind},
      Item, VisItem, VisItemKind, Visibility,
    },
    parser_utils::ParserContext,
    tests::support::{
      call, field, group, int, path_expr, simple_path, simple_path_leading_colon, simplify_expr,
      struct_expr, SimpleExpr, SimpleFieldName, parse_expression, run_parser,
    },
  };

  // Decl parsing

  fn parse_struct_item(input: &str) -> Result<VisItem, ()> {
    run_parser(input, "struct_decl_test_temp", |parser| {
      let attributes = if matches!(parser.current_token().kind, TokenKind::Pound) {
        parser.parse_outer_attributes(ParserContext::Default)?
      } else {
        vec![]
      };

      let visibility = parser.parse_visibility(ParserContext::Default)?;
      let item = parser.parse_struct_decl(attributes, visibility, ParserContext::Default)?;

      match item {
        Item::Vis(vis) => Ok(vis),
        _ => Err(()),
      }
    })
  }

  fn assert_decl_err(input: &str) {
    assert!(
      parse_struct_item(input).is_err(),
      "expected error for: {:?}",
      input
    );
  }

  fn assert_decl_ok(input: &str) -> VisItem {
    parse_struct_item(input).unwrap()
  }

  fn get_decl(vis: &VisItem) -> &StructDecl {
    match &vis.kind {
      VisItemKind::Struct(decl) => decl,
      other => panic!("expected struct decl, got: {:?}", other),
    }
  }

  // Expr parsing
  fn parse_expr(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "struct_expr_test_temp", ParserContext::Struct)
  }

  fn assert_expr_err(input: &str) {
    assert!(parse_expr(input).is_err(), "expected error for: {input:?}");
  }

  // Decl tests

  #[test]
  fn decl_named_empty() {
    let vis = assert_decl_ok("struct Foo {}");
    let decl = get_decl(&vis);

    assert_eq!(decl.name, "Foo");
    assert!(decl.generics.is_none());
    assert!(decl.where_clause.is_none());

    match &decl.kind {
      StructKind::Named { fields } => assert_eq!(fields.len(), 0),
      _ => panic!("expected named struct"),
    }
  }

  #[test]
  fn decl_named_one_field() {
    let vis = assert_decl_ok("struct Foo { x: i32 }");
    let decl = get_decl(&vis);

    match &decl.kind {
      StructKind::Named { fields } => {
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].name, "x");
      },
      _ => panic!("expected named struct"),
    }
  }

  #[test]
  fn decl_named_trailing_comma() {
    let vis = assert_decl_ok("struct Foo { x: i32, }");
    let decl = get_decl(&vis);

    match &decl.kind {
      StructKind::Named { fields } => assert_eq!(fields.len(), 1),
      _ => panic!("expected named struct"),
    }
  }

  #[test]
  fn decl_named_multiple_fields() {
    let vis = assert_decl_ok("struct Foo { x: i32, y: u8 }");
    let decl = get_decl(&vis);

    match &decl.kind {
      StructKind::Named { fields } => {
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].name, "x");
        assert_eq!(fields[1].name, "y");
      },
      _ => panic!("expected named struct"),
    }
  }

  #[test]
  fn decl_named_field_attr_and_vis() {
    let vis = assert_decl_ok("struct Foo { #[a] pub x: i32 }");
    let decl = get_decl(&vis);

    match &decl.kind {
      StructKind::Named { fields } => {
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].name, "x");
        assert!(!fields[0].attributes.is_empty());
        assert_eq!(fields[0].visibility, Visibility::Public);
      },
      _ => panic!("expected named struct"),
    }
  }

  #[test]
  fn decl_outer_attr_and_pub_vis() {
    let vis = assert_decl_ok("#[a] pub struct Foo { x: i32 }");
    assert!(!vis.attributes.is_empty());
    assert_eq!(vis.visibility, Visibility::Public);

    let decl = get_decl(&vis);
    assert_eq!(decl.name, "Foo");
  }

  #[test]
  fn decl_named_with_generics() {
    let vis = assert_decl_ok("struct Foo<T> { x: T }");
    let decl = get_decl(&vis);

    assert!(decl.generics.is_some());
    match &decl.kind {
      StructKind::Named { fields } => assert_eq!(fields.len(), 1),
      _ => panic!("expected named struct"),
    }
  }

  #[test]
  fn decl_named_with_lifetimes_and_bounds() {
    let vis = assert_decl_ok("struct Foo<'a, T: Copy> { x: &'a T }");
    let decl = get_decl(&vis);

    assert!(decl.generics.is_some());
    match &decl.kind {
      StructKind::Named { fields } => assert_eq!(fields.len(), 1),
      _ => panic!("expected named struct"),
    }
  }

  #[test]
  fn decl_named_with_where_clause_before_brace() {
    let vis = assert_decl_ok("struct Foo<T> where T: Copy { x: T }");
    let decl = get_decl(&vis);

    assert!(decl.generics.is_some());
    assert!(decl.where_clause.is_some());
    match &decl.kind {
      StructKind::Named { fields } => assert_eq!(fields.len(), 1),
      _ => panic!("expected named struct"),
    }
  }

  #[test]
  fn decl_named_trailing_semi_is_error() {
    assert_decl_err("struct Foo {};");
  }

  #[test]
  fn decl_named_missing_name_errors() {
    assert_decl_err("struct { x: i32 }");
  }

  #[test]
  fn decl_named_missing_close_brace_errors() {
    assert_decl_err("struct Foo { x: i32");
  }

  #[test]
  fn decl_named_missing_colon_errors() {
    assert_decl_err("struct Foo { x i32 }");
  }

  #[test]
  fn decl_named_missing_type_errors() {
    assert_decl_err("struct Foo { x: }");
  }

  #[test]
  fn decl_named_invalid_field_name_digit_errors() {
    assert_decl_err("struct Foo { 0: i32 }");
  }

  #[test]
  fn decl_named_stray_comma_errors() {
    assert_decl_err("struct Foo { , }");
  }

  #[test]
  fn decl_tuple_empty() {
    let vis = assert_decl_ok("struct Foo();");
    let decl = get_decl(&vis);

    match &decl.kind {
      StructKind::Tuple { fields } => assert_eq!(fields.len(), 0),
      _ => panic!("expected tuple struct"),
    }
  }

  #[test]
  fn decl_tuple_one_field() {
    let vis = assert_decl_ok("struct Foo(i32);");
    let decl = get_decl(&vis);

    match &decl.kind {
      StructKind::Tuple { fields } => assert_eq!(fields.len(), 1),
      _ => panic!("expected tuple struct"),
    }
  }

  #[test]
  fn decl_tuple_one_field_trailing_comma() {
    let vis = assert_decl_ok("struct Foo(i32,);");
    let decl = get_decl(&vis);

    match &decl.kind {
      StructKind::Tuple { fields } => assert_eq!(fields.len(), 1),
      _ => panic!("expected tuple struct"),
    }
  }

  #[test]
  fn decl_tuple_multiple_fields() {
    let vis = assert_decl_ok("struct Foo(i32, u8);");
    let decl = get_decl(&vis);

    match &decl.kind {
      StructKind::Tuple { fields } => assert_eq!(fields.len(), 2),
      _ => panic!("expected tuple struct"),
    }
  }

  #[test]
  fn decl_tuple_fields_attr_and_vis() {
    let vis = assert_decl_ok("struct Foo(pub i32, #[a] u8);");
    let decl = get_decl(&vis);

    match &decl.kind {
      StructKind::Tuple { fields } => {
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].visibility, Visibility::Public);
        assert!(!fields[1].attributes.is_empty());
      },
      _ => panic!("expected tuple struct"),
    }
  }

  #[test]
  fn decl_tuple_with_generics_and_where_after_fields() {
    let vis = assert_decl_ok("struct Foo<T>(T) where T: Copy;");
    let decl = get_decl(&vis);

    assert!(decl.generics.is_some());
    assert!(decl.where_clause.is_some());
    match &decl.kind {
      StructKind::Tuple { fields } => assert_eq!(fields.len(), 1),
      _ => panic!("expected tuple struct"),
    }
  }

  #[test]
  fn decl_tuple_where_before_fields_is_error() {
    assert_decl_err("struct Foo<T> where T: Copy (T);");
  }

  #[test]
  fn decl_tuple_missing_semi_errors() {
    assert_decl_err("struct Foo(i32)");
  }

  #[test]
  fn decl_tuple_missing_close_paren_errors() {
    assert_decl_err("struct Foo(i32;");
  }

  #[test]
  fn decl_tuple_leading_comma_errors() {
    assert_decl_err("struct Foo(, i32);");
  }

  #[test]
  fn decl_tuple_missing_comma_between_fields_errors() {
    assert_decl_err("struct Foo(i32 u8);");
  }

  #[test]
  fn decl_unit_basic() {
    let vis = assert_decl_ok("struct Foo;");
    let decl = get_decl(&vis);

    match &decl.kind {
      StructKind::Unit => {},
      _ => panic!("expected unit struct"),
    }
  }

  #[test]
  fn decl_unit_pub() {
    let vis = assert_decl_ok("pub struct Foo;");
    assert_eq!(vis.visibility, Visibility::Public);

    let decl = get_decl(&vis);
    match &decl.kind {
      StructKind::Unit => {},
      _ => panic!("expected unit struct"),
    }
  }

  #[test]
  fn decl_unit_with_generics_and_where() {
    let vis = assert_decl_ok("struct Foo<'a, T> where T: Copy;");
    let decl = get_decl(&vis);

    assert!(decl.generics.is_some());
    assert!(decl.where_clause.is_some());
    match &decl.kind {
      StructKind::Unit => {},
      _ => panic!("expected unit struct"),
    }
  }

  #[test]
  fn decl_unit_missing_semi_errors() {
    assert_decl_err("struct Foo");
  }

  // Expr model and helpers

  fn assert_expr(input: &str, expected: SimpleExpr) {
    let expr = parse_expr(input).unwrap();
    assert_eq!(simplify_expr(&expr), expected);
  }

  // Expr tests

  #[test]
  fn expr_struct_empty_braces() {
    assert_expr("Foo {}", struct_expr(simple_path(["Foo"]), vec![], None));
  }

  #[test]
  fn expr_struct_named_fields() {
    assert_expr(
      "Foo { x: 1, y: 2 }",
      struct_expr(
        simple_path(["Foo"]),
        vec![
          field(SimpleFieldName::Ident("x".into()), int(1)),
          field(SimpleFieldName::Ident("y".into()), int(2)),
        ],
        None,
      ),
    );
  }

  #[test]
  fn expr_struct_shorthand_and_explicit() {
    assert_expr(
      "Foo { x, y: 2 }",
      struct_expr(
        simple_path(["Foo"]),
        vec![
          field(
            SimpleFieldName::Ident("x".into()),
            path_expr(simple_path(["x"])),
          ),
          field(SimpleFieldName::Ident("y".into()), int(2)),
        ],
        None,
      ),
    );
  }

  #[test]
  fn expr_struct_tuple_index_fields() {
    assert_expr(
      "Foo { 0: 10, 1: 20 }",
      struct_expr(
        simple_path(["Foo"]),
        vec![
          field(SimpleFieldName::TupleIndex(0), int(10)),
          field(SimpleFieldName::TupleIndex(1), int(20)),
        ],
        None,
      ),
    );
  }

  #[test]
  fn expr_struct_with_base() {
    assert_expr(
      "Foo { x: 1, ..base }",
      struct_expr(
        simple_path(["Foo"]),
        vec![field(SimpleFieldName::Ident("x".into()), int(1))],
        Some(path_expr(simple_path(["base"]))),
      ),
    );
  }

  #[test]
  fn expr_struct_base_only() {
    assert_expr(
      "Foo { ..other }",
      struct_expr(
        simple_path(["Foo"]),
        vec![],
        Some(path_expr(simple_path(["other"]))),
      ),
    );
  }

  #[test]
  fn expr_struct_base_is_call() {
    assert_expr(
      "Foo { ..Bar(1) }",
      struct_expr(
        simple_path(["Foo"]),
        vec![],
        Some(call(path_expr(simple_path(["Bar"])), vec![int(1)])),
      ),
    );
  }

  #[test]
  fn expr_struct_with_trailing_comma() {
    assert_expr(
      "Foo { x: 1, y: 2, }",
      struct_expr(
        simple_path(["Foo"]),
        vec![
          field(SimpleFieldName::Ident("x".into()), int(1)),
          field(SimpleFieldName::Ident("y".into()), int(2)),
        ],
        None,
      ),
    );
  }

  #[test]
  fn expr_unit_struct_is_path() {
    assert_expr("Foo", path_expr(simple_path(["Foo"])));
  }

  #[test]
  fn expr_tuple_struct_constructor_is_call() {
    assert_expr(
      "Foo(1, 2)",
      call(path_expr(simple_path(["Foo"])), vec![int(1), int(2)]),
    );
  }

  #[test]
  fn expr_tuple_struct_empty_is_call() {
    assert_expr("Foo()", call(path_expr(simple_path(["Foo"])), vec![]));
  }

  #[test]
  fn expr_tuple_struct_trailing_comma_is_call() {
    assert_expr(
      "Foo(1, 2,)",
      call(path_expr(simple_path(["Foo"])), vec![int(1), int(2)]),
    );
  }

  #[test]
  fn expr_call_with_grouped_callee() {
    assert_expr(
      "(Foo)(1)",
      call(group(path_expr(simple_path(["Foo"]))), vec![int(1)]),
    );
  }

  #[test]
  fn expr_call_with_struct_arg() {
    assert_expr(
      "Foo(Bar { x: 1 })",
      call(
        path_expr(simple_path(["Foo"])),
        vec![struct_expr(
          simple_path(["Bar"]),
          vec![field(SimpleFieldName::Ident("x".into()), int(1))],
          None,
        )],
      ),
    );
  }

  #[test]
  fn expr_struct_path_with_leading_colon() {
    assert_expr(
      "::Foo { x: 1 }",
      struct_expr(
        simple_path_leading_colon(["Foo"]),
        vec![field(SimpleFieldName::Ident("x".into()), int(1))],
        None,
      ),
    );
  }

  #[test]
  fn expr_struct_path_with_module_segments() {
    assert_expr(
      "a::b::Foo { x: 1 }",
      struct_expr(
        simple_path(["a", "b", "Foo"]),
        vec![field(SimpleFieldName::Ident("x".into()), int(1))],
        None,
      ),
    );
  }

  #[test]
  fn expr_struct_path_segments_self_super_crate() {
    assert_expr(
      "self::super::crate::Foo { x: 1 }",
      struct_expr(
        simple_path(["self", "super", "crate", "Foo"]),
        vec![field(SimpleFieldName::Ident("x".into()), int(1))],
        None,
      ),
    );
  }

  #[test]
  fn expr_struct_path_dollar_crate_first_segment() {
    assert_expr(
      "$crate::Foo { x: 1 }",
      struct_expr(
        simple_path(["$crate", "Foo"]),
        vec![field(SimpleFieldName::Ident("x".into()), int(1))],
        None,
      ),
    );
  }

  #[test]
  fn expr_struct_path_dollar_crate_non_first_errors() {
    for src in [
      "self::$crate::Foo { x: 1 }",
      "self::super::crate::$crate::Foo { x: 1 }",
    ] {
      assert_expr_err(src);
    }
  }

  // Expr error tests

  #[test]
  fn expr_invalid_field_name_errors() {
    assert_expr_err("Foo { \"x\": 1 }");
  }

  #[test]
  fn expr_missing_closing_brace_errors() {
    assert_expr_err("Foo { x: 1");
  }

  #[test]
  fn expr_missing_base_expression_errors() {
    assert_expr_err("Foo { .. }");
  }

  #[test]
  fn expr_struct_base_not_last_errors() {
    assert_expr_err("Foo { ..base, x: 1 }");
  }

  #[test]
  fn expr_struct_base_with_trailing_comma_errors() {
    assert_expr_err("Foo { x: 1, ..base, }");
  }

  #[test]
  fn expr_struct_leading_comma_errors() {
    assert_expr_err("Foo { , x: 1 }");
  }

  #[test]
  fn expr_struct_double_comma_errors() {
    assert_expr_err("Foo { x: 1,, y: 2 }");
  }

  #[test]
  fn expr_struct_missing_comma_between_fields_errors() {
    assert_expr_err("Foo { x: 1 y: 2 }");
  }

  #[test]
  fn expr_call_missing_paren_errors() {
    assert_expr_err("Foo(1, 2");
  }

  #[test]
  fn expr_call_leading_comma_errors() {
    assert_expr_err("Foo(,1)");
  }

  #[test]
  fn expr_call_double_comma_errors() {
    assert_expr_err("Foo(1,,2)");
  }

  #[test]
  fn expr_call_missing_comma_between_elements_errors() {
    assert_expr_err("Foo(1 2)");
  }

  #[test]
  fn expr_call_only_comma_errors() {
    assert_expr_err("Foo(,)");
  }

  #[test]
  fn expr_struct_stray_trailing_tokens_errors() {
    assert_expr_err("Foo {} 1");
  }

  #[test]
  fn expr_call_stray_trailing_tokens_errors() {
    assert_expr_err("Foo(1) 2");
  }

  #[test]
  fn tuple_index_shorthand_panics_with_current_code() {
    assert_expr_err("Foo { 0 }");
  }

  #[test]
  fn decl_pub_crate_struct() {
    assert_decl_ok("pub(crate) struct Foo {}");
  }

  #[test]
  fn decl_pub_super_struct() {
    assert_decl_ok("pub(super) struct Foo {}");
  }

  #[test]
  fn decl_pub_in_path_struct() {
    assert_decl_ok("pub(in crate::a) struct Foo {}");
  }

  #[test]
  fn field_pub_crate_visibility() {
    let vis = assert_decl_ok("struct Foo { pub(crate) x: i32 }");
    let decl = get_decl(&vis);
    match &decl.kind {
      StructKind::Named { fields } => assert_eq!(fields[0].visibility, Visibility::PublicCrate),
      _ => panic!(),
    }
  }

  #[test]
  fn multiple_outer_attributes() {
    let vis = assert_decl_ok("#[a] #[b] struct Foo {}");
    assert_eq!(vis.attributes.len(), 2);
  }

  #[test]
  fn multiple_field_attributes() {
    let vis = assert_decl_ok("struct Foo { #[a] #[b] x: i32 }");
    let decl = get_decl(&vis);
    match &decl.kind {
      StructKind::Named { fields } => assert_eq!(fields[0].attributes.len(), 2),
      _ => panic!(),
    }
  }

  #[test]
  fn generics_trailing_comma() {
    assert_decl_ok("struct Foo<T,> {}");
  }

  #[test]
  fn where_multiple_predicates() {
    assert_decl_ok("struct Foo<T, U> where T: Copy, U: Clone {}");
  }

  #[test]
  fn where_lifetime_and_type_bounds() {
    assert_decl_ok("struct Foo<'a, T> where T: 'a + Copy {}");
  }

  #[test]
  fn empty_where_clause_errors() {
    assert_decl_err("struct Foo<T> where {}");
  }

  #[test]
  fn tuple_struct_field_attrs() {
    assert_decl_ok("struct Foo(#[a] i32, #[b] u8);");
  }

  #[test]
  fn tuple_struct_pub_fields() {
    let vis = assert_decl_ok("struct Foo(pub i32, pub u8);");
    let decl = get_decl(&vis);
    match &decl.kind {
      StructKind::Tuple { fields } => assert_eq!(fields.len(), 2),
      _ => panic!(),
    }
  }

  #[test]
  fn tuple_struct_double_trailing_comma_errors() {
    assert_decl_err("struct Foo(i32,,);");
  }

  #[test]
  fn nested_calls() {
    assert_expr(
      "Foo()(1)",
      call(call(path_expr(simple_path(["Foo"])), vec![]), vec![int(1)]),
    );
  }

  #[test]
  fn chained_calls() {
    assert_expr(
      "Foo(1)(2)",
      call(
        call(path_expr(simple_path(["Foo"])), vec![int(1)]),
        vec![int(2)],
      ),
    );
  }

  #[test]
  fn call_with_struct_and_call_args() {
    assert_expr(
      "Foo(Bar { x: 1 }, Baz(2))",
      call(
        path_expr(simple_path(["Foo"])),
        vec![
          struct_expr(
            simple_path(["Bar"]),
            vec![field(SimpleFieldName::Ident("x".into()), int(1))],
            None,
          ),
          call(path_expr(simple_path(["Baz"])), vec![int(2)]),
        ],
      ),
    );
  }
}
