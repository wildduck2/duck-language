#[cfg(test)]
mod struct_decl_tests {
  use lexer::token::TokenKind;

  use crate::{
    ast::{
      expr::ExprKind,
      r#struct::{StructDecl, StructKind},
      Item, Lit, VisItem, VisItemKind, Visibility,
    },
    parser_utils::ExprContext,
    tests::support::{parse_primary_expr, run_parser},
  };

  fn parse_struct_item(input: &str) -> Result<VisItem, ()> {
    run_parser(input, "struct_decl_test_temp", |parser| {
      let attributes = if matches!(parser.current_token().kind, TokenKind::Pound) {
        parser.parse_outer_attributes()?
      } else {
        vec![]
      };

      let visibility = parser.parse_visibility()?;
      let item = parser.parse_struct_decl(attributes, visibility)?;

      match item {
        Item::Vis(vis) => Ok(vis),
        _ => Err(()),
      }
    })
  }

  fn parse_single(input: &str) -> Result<ExprKind, ()> {
    parse_primary_expr(input, "struct_expr_test_temp", ExprContext::Struct)
  }

  fn assert_err(input: &str) {
    assert!(
      parse_struct_item(input).is_err(),
      "expected error for: {:?}",
      input
    );
  }

  fn assert_ok(input: &str) -> VisItem {
    parse_struct_item(input).unwrap()
  }

  fn get_decl(vis: &VisItem) -> &StructDecl {
    match &vis.kind {
      VisItemKind::Struct(decl) => decl,
      other => panic!("expected struct decl, got: {:?}", other),
    }
  }

  #[test]
  fn decl_named_empty() {
    let vis = assert_ok("struct Foo {}");
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
    let vis = assert_ok("struct Foo { x: i32 }");
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
    let vis = assert_ok("struct Foo { x: i32, }");
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
  fn decl_named_multiple_fields() {
    let vis = assert_ok("struct Foo { x: i32, y: u8 }");
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
    let vis = assert_ok("struct Foo { #[a] pub x: i32 }");
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
    let vis = assert_ok("#[a] pub struct Foo { x: i32 }");
    assert!(!vis.attributes.is_empty());
    assert_eq!(vis.visibility, Visibility::Public);
    let decl = get_decl(&vis);
    assert_eq!(decl.name, "Foo");
  }

  #[test]
  fn decl_named_with_generics() {
    let vis = assert_ok("struct Foo<T> { x: T }");
    let decl = get_decl(&vis);
    assert!(decl.generics.is_some());
    match &decl.kind {
      StructKind::Named { fields } => assert_eq!(fields.len(), 1),
      _ => panic!("expected named struct"),
    }
  }

  #[test]
  fn decl_named_with_lifetimes_and_bounds() {
    let vis = assert_ok("struct Foo<'a, T: Copy> { x: &'a T }");
    let decl = get_decl(&vis);
    assert!(decl.generics.is_some());
    match &decl.kind {
      StructKind::Named { fields } => assert_eq!(fields.len(), 1),
      _ => panic!("expected named struct"),
    }
  }

  #[test]
  fn decl_named_with_where_clause_before_brace() {
    let vis = assert_ok("struct Foo<T> where T: Copy { x: T }");
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
    assert_err("struct Foo {};");
  }

  #[test]
  fn decl_named_missing_name_errors() {
    assert_err("struct { x: i32 }");
  }

  #[test]
  fn decl_named_missing_close_brace_errors() {
    assert_err("struct Foo { x: i32");
  }

  #[test]
  fn decl_named_missing_colon_errors() {
    assert_err("struct Foo { x i32 }");
  }

  #[test]
  fn decl_named_missing_type_errors() {
    assert_err("struct Foo { x: }");
  }

  #[test]
  fn decl_named_invalid_field_name_digit_errors() {
    assert_err("struct Foo { 0: i32 }");
  }

  #[test]
  fn decl_named_stray_comma_errors() {
    assert_err("struct Foo { , }");
  }

  #[test]
  fn decl_tuple_empty() {
    let vis = assert_ok("struct Foo();");
    let decl = get_decl(&vis);
    match &decl.kind {
      StructKind::Tuple { fields } => assert_eq!(fields.len(), 0),
      _ => panic!("expected tuple struct"),
    }
  }

  #[test]
  fn decl_tuple_one_field() {
    let vis = assert_ok("struct Foo(i32);");
    let decl = get_decl(&vis);
    match &decl.kind {
      StructKind::Tuple { fields } => assert_eq!(fields.len(), 1),
      _ => panic!("expected tuple struct"),
    }
  }

  #[test]
  fn decl_tuple_one_field_trailing_comma() {
    let vis = assert_ok("struct Foo(i32,);");
    let decl = get_decl(&vis);
    match &decl.kind {
      StructKind::Tuple { fields } => assert_eq!(fields.len(), 1),
      _ => panic!("expected tuple struct"),
    }
  }

  #[test]
  fn decl_tuple_multiple_fields() {
    let vis = assert_ok("struct Foo(i32, u8);");
    let decl = get_decl(&vis);
    match &decl.kind {
      StructKind::Tuple { fields } => assert_eq!(fields.len(), 2),
      _ => panic!("expected tuple struct"),
    }
  }

  #[test]
  fn decl_tuple_fields_attr_and_vis() {
    let vis = assert_ok("struct Foo(pub i32, #[a] u8);");
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
    let vis = assert_ok("struct Foo<T>(T) where T: Copy;");
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
    assert_err("struct Foo<T> where T: Copy (T);");
  }

  #[test]
  fn decl_tuple_missing_semi_errors() {
    assert_err("struct Foo(i32)");
  }

  #[test]
  fn decl_tuple_missing_close_paren_errors() {
    assert_err("struct Foo(i32;");
  }

  #[test]
  fn decl_tuple_leading_comma_errors() {
    assert_err("struct Foo(, i32);");
  }

  #[test]
  fn decl_tuple_missing_comma_between_fields_errors() {
    assert_err("struct Foo(i32 u8);");
  }

  #[test]
  fn decl_unit_basic() {
    let vis = assert_ok("struct Foo;");
    let decl = get_decl(&vis);
    match &decl.kind {
      StructKind::Unit => {},
      _ => panic!("expected unit struct"),
    }
  }

  #[test]
  fn decl_unit_pub() {
    let vis = assert_ok("pub struct Foo;");
    assert_eq!(vis.visibility, Visibility::Public);
    let decl = get_decl(&vis);
    match &decl.kind {
      StructKind::Unit => {},
      _ => panic!("expected unit struct"),
    }
  }

  #[test]
  fn decl_unit_with_generics_and_where() {
    let vis = assert_ok("struct Foo<'a, T> where T: Copy;");
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
    assert_err("struct Foo");
  }

  #[derive(Debug, PartialEq)]
  enum SimpleExpr {
    Int(i128),
    Path(SimplePath),
    Struct {
      path: SimplePath,
      fields: Vec<SimpleField>,
      base: Option<Box<SimpleExpr>>,
    },
    TupleStruct {
      path: SimplePath,
      elements: Vec<SimpleExpr>,
    },
  }

  #[derive(Debug, PartialEq, Clone)]
  struct SimplePath {
    leading_colon: bool,
    segments: Vec<String>,
  }

  #[derive(Debug, PartialEq)]
  struct SimpleField {
    name: SimpleFieldName,
    value: SimpleExpr,
  }

  #[derive(Debug, PartialEq)]
  enum SimpleFieldName {
    Ident(String),
    TupleIndex(usize),
  }

  fn simple_path<I: Into<String>>(segments: impl IntoIterator<Item = I>) -> SimplePath {
    SimplePath {
      leading_colon: false,
      segments: segments.into_iter().map(Into::into).collect(),
    }
  }

  fn int(value: i128) -> SimpleExpr {
    SimpleExpr::Int(value)
  }

  fn struct_expr(
    path: SimplePath,
    fields: Vec<SimpleField>,
    base: Option<SimpleExpr>,
  ) -> SimpleExpr {
    SimpleExpr::Struct {
      path,
      fields,
      base: base.map(Box::new),
    }
  }

  fn tuple_struct(path: SimplePath, elements: Vec<SimpleExpr>) -> SimpleExpr {
    SimpleExpr::TupleStruct { path, elements }
  }

  fn field(name: SimpleFieldName, value: SimpleExpr) -> SimpleField {
    SimpleField { name, value }
  }

  fn simplify_path(path: &crate::ast::path::Path) -> SimplePath {
    use crate::ast::path::PathSegmentKind;
    let segments = path
      .segments
      .iter()
      .map(|segment| match &segment.kind {
        PathSegmentKind::Ident(name) => name.clone(),
        PathSegmentKind::Self_ => "self".to_string(),
        PathSegmentKind::SelfType => "Self".to_string(),
        PathSegmentKind::Super => "super".to_string(),
        PathSegmentKind::Crate => "crate".to_string(),
        PathSegmentKind::DollarCrate => "$crate".to_string(),
      })
      .collect();
    SimplePath {
      leading_colon: path.leading_colon,
      segments,
    }
  }

  fn simplify(expr: &ExprKind) -> SimpleExpr {
    match expr {
      ExprKind::Literal(Lit::Integer { value, .. }) => int(*value),
      ExprKind::Path { path, .. } => SimpleExpr::Path(simplify_path(path)),
      ExprKind::Struct { path, fields, base } => SimpleExpr::Struct {
        path: simplify_path(path),
        fields: fields
          .iter()
          .map(|field| SimpleField {
            name: match &field.name {
              crate::ast::expr::FieldName::Ident(name) => SimpleFieldName::Ident(name.clone()),
              crate::ast::expr::FieldName::TupleIndex(idx) => SimpleFieldName::TupleIndex(*idx),
            },
            value: field
              .value
              .as_ref()
              .map(|expr| simplify(&expr.kind))
              .unwrap_or_else(|| panic!("field values always present in struct tests")),
          })
          .collect(),
        base: base.as_ref().map(|expr| Box::new(simplify(&expr.kind))),
      },
      ExprKind::TupleStruct { path, elements } => SimpleExpr::TupleStruct {
        path: simplify_path(path),
        elements: elements.iter().map(|expr| simplify(&expr.kind)).collect(),
      },
      other => panic!("unsupported expression in struct tests: {:?}", other),
    }
  }

  fn assert_expr(input: &str, expected: SimpleExpr) {
    let expr = parse_single(input).unwrap();
    assert_eq!(simplify(&expr), expected);
  }

  #[test]
  fn struct_empty_braces() {
    assert_expr("Foo {}", struct_expr(simple_path(["Foo"]), vec![], None));
  }

  #[test]
  fn struct_named_fields() {
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
  fn struct_shorthand_and_explicit() {
    assert_expr(
      "Foo { x, y: 2 }",
      struct_expr(
        simple_path(["Foo"]),
        vec![
          field(
            SimpleFieldName::Ident("x".into()),
            SimpleExpr::Path(simple_path(["x"])),
          ),
          field(SimpleFieldName::Ident("y".into()), int(2)),
        ],
        None,
      ),
    );
  }

  #[test]
  fn struct_tuple_index_fields() {
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
  fn struct_with_base_expression() {
    assert_expr(
      "Foo { x: 1, ..base }",
      struct_expr(
        simple_path(["Foo"]),
        vec![field(SimpleFieldName::Ident("x".into()), int(1))],
        Some(SimpleExpr::Path(simple_path(["base"]))),
      ),
    );
  }

  #[test]
  fn struct_base_only() {
    assert_expr(
      "Foo { ..other }",
      struct_expr(
        simple_path(["Foo"]),
        vec![],
        Some(SimpleExpr::Path(simple_path(["other"]))),
      ),
    );
  }

  #[test]
  fn struct_with_trailing_comma() {
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
  fn struct_single_field_no_comma() {
    assert_expr(
      "Foo { x: 1 }",
      struct_expr(
        simple_path(["Foo"]),
        vec![field(SimpleFieldName::Ident("x".into()), int(1))],
        None,
      ),
    );
  }

  #[test]
  fn struct_multiple_shorthand_fields() {
    assert_expr(
      "Foo { x, y, z }",
      struct_expr(
        simple_path(["Foo"]),
        vec![
          field(
            SimpleFieldName::Ident("x".into()),
            SimpleExpr::Path(simple_path(["x"])),
          ),
          field(
            SimpleFieldName::Ident("y".into()),
            SimpleExpr::Path(simple_path(["y"])),
          ),
          field(
            SimpleFieldName::Ident("z".into()),
            SimpleExpr::Path(simple_path(["z"])),
          ),
        ],
        None,
      ),
    );
  }

  #[test]
  fn struct_nested_struct_field_value() {
    assert_expr(
      "Foo { x: Bar { y: 1 } }",
      struct_expr(
        simple_path(["Foo"]),
        vec![field(
          SimpleFieldName::Ident("x".into()),
          struct_expr(
            simple_path(["Bar"]),
            vec![field(SimpleFieldName::Ident("y".into()), int(1))],
            None,
          ),
        )],
        None,
      ),
    );
  }

  #[test]
  fn struct_nested_tuple_struct_field_value() {
    assert_expr(
      "Foo { x: Bar(1, 2) }",
      struct_expr(
        simple_path(["Foo"]),
        vec![field(
          SimpleFieldName::Ident("x".into()),
          tuple_struct(simple_path(["Bar"]), vec![int(1), int(2)]),
        )],
        None,
      ),
    );
  }

  #[test]
  fn struct_base_is_struct_expr() {
    assert_expr(
      "Foo { ..Bar { x: 1 } }",
      struct_expr(
        simple_path(["Foo"]),
        vec![],
        Some(struct_expr(
          simple_path(["Bar"]),
          vec![field(SimpleFieldName::Ident("x".into()), int(1))],
          None,
        )),
      ),
    );
  }

  #[test]
  fn struct_base_is_tuple_struct_expr() {
    assert_expr(
      "Foo { ..Bar(1) }",
      struct_expr(
        simple_path(["Foo"]),
        vec![],
        Some(tuple_struct(simple_path(["Bar"]), vec![int(1)])),
      ),
    );
  }

  #[test]
  fn struct_path_with_leading_colon() {
    assert_expr(
      "::Foo { x: 1 }",
      struct_expr(
        SimplePath {
          leading_colon: true,
          segments: vec!["Foo".into()],
        },
        vec![field(SimpleFieldName::Ident("x".into()), int(1))],
        None,
      ),
    );
  }

  #[test]
  fn struct_path_with_module_segments() {
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
  fn struct_path_segments_self_super_crate() {
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
  fn struct_path_dollar_crate_first_segment() {
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
  fn struct_path_dollar_crate_non_first_errors() {
    let cases = [
      "self::$crate::Foo { x: 1 }",
      "self::super::crate::$crate::Foo { x: 1 }",
    ];
    for src in cases {
      assert_err(src);
    }
  }

  #[test]
  fn tuple_struct_basic() {
    assert_expr(
      "Foo(1, 2)",
      tuple_struct(simple_path(["Foo"]), vec![int(1), int(2)]),
    );
  }

  #[test]
  fn tuple_struct_empty() {
    assert_expr("Foo()", tuple_struct(simple_path(["Foo"]), vec![]));
  }

  #[test]
  fn tuple_struct_trailing_comma() {
    assert_expr(
      "Foo(1, 2,)",
      tuple_struct(simple_path(["Foo"]), vec![int(1), int(2)]),
    );
  }

  #[test]
  fn tuple_struct_single_element() {
    assert_expr("Foo(1)", tuple_struct(simple_path(["Foo"]), vec![int(1)]));
  }

  #[test]
  fn tuple_struct_nested_struct_element() {
    assert_expr(
      "Foo(Bar { x: 1 })",
      tuple_struct(
        simple_path(["Foo"]),
        vec![struct_expr(
          simple_path(["Bar"]),
          vec![field(SimpleFieldName::Ident("x".into()), int(1))],
          None,
        )],
      ),
    );
  }

  #[test]
  fn tuple_struct_nested_tuple_struct_element() {
    assert_expr(
      "Foo(Bar(1, 2))",
      tuple_struct(
        simple_path(["Foo"]),
        vec![tuple_struct(simple_path(["Bar"]), vec![int(1), int(2)])],
      ),
    );
  }

  #[test]
  fn unit_struct_is_path_expression() {
    assert_expr("Foo", SimpleExpr::Path(simple_path(["Foo"])));
  }

  #[test]
  fn invalid_field_name_errors() {
    assert_err("Foo { \"x\": 1 }");
  }

  #[test]
  fn missing_closing_brace_errors() {
    assert_err("Foo { x: 1");
  }

  #[test]
  fn missing_base_expression_errors() {
    assert_err("Foo { .. }");
  }

  #[test]
  fn tuple_struct_missing_paren_errors() {
    assert_err("Foo(1, 2");
  }

  #[test]
  fn struct_base_not_last_errors() {
    assert_err("Foo { ..base, x: 1 }");
  }

  #[test]
  fn struct_base_with_trailing_comma_errors() {
    assert_err("Foo { x: 1, ..base, }");
  }

  #[test]
  fn struct_leading_comma_errors() {
    assert_err("Foo { , x: 1 }");
  }

  #[test]
  fn struct_double_comma_errors() {
    assert_err("Foo { x: 1,, y: 2 }");
  }

  #[test]
  fn struct_missing_comma_between_fields_errors() {
    assert_err("Foo { x: 1 y: 2 }");
  }

  #[test]
  fn tuple_struct_leading_comma_errors() {
    assert_err("Foo(,1)");
  }

  #[test]
  fn tuple_struct_double_comma_errors() {
    assert_err("Foo(1,,2)");
  }

  #[test]
  fn tuple_struct_missing_comma_between_elements_errors() {
    assert_err("Foo(1 2)");
  }

  #[test]
  fn tuple_struct_only_comma_errors() {
    assert_err("Foo(,)");
  }

  #[test]
  fn struct_stray_trailing_tokens_errors() {
    assert_err("Foo {} 1");
  }

  #[test]
  fn tuple_struct_stray_trailing_tokens_errors() {
    assert_err("Foo(1) 2");
  }

  #[test]
  #[should_panic]
  fn tuple_index_shorthand_panics_with_current_code() {
    let _ = parse_single("Foo { 0 }");
  }
}
