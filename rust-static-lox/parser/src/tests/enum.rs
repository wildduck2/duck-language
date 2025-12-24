#[cfg(test)]
mod enum_tests {

  use crate::{
    ast::{Item, VisItemKind},
    parser_utils::ParserContext,
    tests::support::parse_item,
  };

  fn parse_enum(input: &str) -> Result<(), ()> {
    let item = parse_item(input, "enum_test_temp", ParserContext::Default)?;

    match item {
      Item::Vis(vis) => match vis.kind {
        VisItemKind::Enum(_) => Ok(()),
        _ => Err(()),
      },
      _ => Err(()),
    }
  }

  fn assert_ok(input: &str) {
    assert!(parse_enum(input).is_ok(), "expected ok for {input:?}");
  }

  fn assert_err(input: &str) {
    assert!(parse_enum(input).is_err(), "expected error for {input:?}");
  }

  // ------------------------------------------------------------
  // Basic enum forms
  // ------------------------------------------------------------

  #[test]
  fn enum_empty() {
    assert_ok("enum E {}");
  }

  #[test]
  fn enum_single_unit_variant() {
    assert_ok("enum E { A }");
  }

  #[test]
  fn enum_multiple_unit_variants() {
    assert_ok("enum E { A, B, C }");
  }

  #[test]
  fn enum_trailing_comma() {
    assert_ok("enum E { A, B, }");
  }

  // ------------------------------------------------------------
  // Tuple variants
  // ------------------------------------------------------------

  #[test]
  fn enum_tuple_variant_single() {
    assert_ok("enum E { A(i32) }");
  }

  #[test]
  fn enum_tuple_variant_multiple_fields() {
    assert_ok("enum E { A(i32, u32) }");
  }

  #[test]
  fn enum_tuple_variant_trailing_comma() {
    assert_ok("enum E { A(i32, u32,), }");
  }

  // ------------------------------------------------------------
  // Struct variants
  // ------------------------------------------------------------

  #[test]
  fn enum_struct_variant() {
    assert_ok("enum E { A { x: i32 } }");
  }

  #[test]
  fn enum_struct_variant_multiple_fields() {
    assert_ok("enum E { A { x: i32, y: u32 } }");
  }

  #[test]
  fn enum_struct_variant_trailing_comma() {
    assert_ok("enum E { A { x: i32, }, }");
  }

  // ------------------------------------------------------------
  // Mixed variant kinds
  // ------------------------------------------------------------

  #[test]
  fn enum_mixed_variants() {
    assert_ok("enum E { A, B(i32), C { x: i32 } }");
  }

  // ------------------------------------------------------------
  // Discriminants
  // ------------------------------------------------------------

  #[test]
  fn enum_discriminant_simple() {
    assert_ok("enum E { A = 1 }");
  }

  #[test]
  fn enum_discriminant_expression() {
    assert_ok("enum E { A = 1 + 2 }");
  }

  #[test]
  fn enum_discriminant_multiple() {
    assert_ok("enum E { A = 1, B = 2 }");
  }

  #[test]
  fn enum_discriminant_with_trailing_comma() {
    assert_ok("enum E { A = 1, B = 2, }");
  }

  // ------------------------------------------------------------
  // Visibility and attributes on variants
  // ------------------------------------------------------------

  #[test]
  fn enum_variant_with_visibility() {
    assert_ok("enum E { pub A }");
  }

  #[test]
  fn enum_variant_with_attributes() {
    assert_ok("enum E { #[attr] A }");
  }

  #[test]
  fn enum_variant_with_attr_and_visibility() {
    assert_ok("enum E { #[attr] pub A }");
  }

  // ------------------------------------------------------------
  // Generics and where clause
  // ------------------------------------------------------------

  #[test]
  fn enum_with_generics() {
    assert_ok("enum E<T> { A(T) }");
  }

  #[test]
  fn enum_with_where_clause() {
    assert_ok("enum E<T> where T: Copy { A(T) }");
  }

  #[test]
  fn enum_with_generics_and_where() {
    assert_ok("enum E<T, U> where T: Copy, U: Clone { A(T), B(U) }");
  }

  // ------------------------------------------------------------
  // Error cases
  // ------------------------------------------------------------

  #[test]
  fn enum_missing_name_errors() {
    assert_err("enum { A }");
  }

  #[test]
  fn enum_missing_braces_errors() {
    assert_err("enum E");
  }

  #[test]
  fn enum_variant_missing_name_errors() {
    assert_err("enum E { = 1 }");
  }

  #[test]
  fn enum_variant_invalid_discriminant_errors() {
    assert_err("enum E { A = }");
  }

  #[test]
  fn enum_duplicate_commas_errors() {
    assert_err("enum E { A,, B }");
  }

  #[test]
  fn enum_invalid_variant_syntax_errors() {
    assert_err("enum E { A(i32 }");
    assert_err("enum E { A { x i32 } }");
  }
}
