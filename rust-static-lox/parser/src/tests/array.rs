#[cfg(test)]
mod array_tests {

  use crate::{
    ast::expr::ExprKind,
    parser_utils::ParserContext,
    tests::support::{array, int, repeat_array, simplify_expr, SimpleExpr, parse_primary_expr},
  };

  fn parse_single(input: &str) -> Result<ExprKind, ()> {
    parse_primary_expr(input, "array_test_temp", ParserContext::Default)
  }

  fn assert_err(input: &str) {
    assert!(
      parse_single(input).is_err(),
      "expected error for: {:?}",
      input
    );
  }

  fn assert_expr(input: &str, expected: SimpleExpr) {
    let expr = parse_single(input).unwrap();
    assert_eq!(simplify_expr(&expr), expected);
  }

  #[test]
  fn empty_array() {
    assert_expr("[]", array(vec![]));
  }

  #[test]
  fn single_element_array() {
    assert_expr("[1]", array(vec![int(1)]));
  }

  #[test]
  fn multiple_elements_array() {
    assert_expr("[1, 2, 3]", array(vec![int(1), int(2), int(3)]));
  }

  #[test]
  fn trailing_comma_array() {
    assert_expr("[1, 2, 3,]", array(vec![int(1), int(2), int(3)]));
  }

  #[test]
  fn repeated_commas_error() {
    assert_err("[1,,2]");
  }

  #[test]
  fn leading_comma_errors() {
    assert_err("[,1]");
    assert_err("[,]");
  }

  #[test]
  fn only_commas_errors() {
    assert_err("[,,]");
  }

  #[test]
  fn missing_first_element_errors() {
    assert_err("[; 1]");
  }

  #[test]
  fn nested_arrays() {
    assert_expr(
      "[[1], [2, 3]]",
      array(vec![array(vec![int(1)]), array(vec![int(2), int(3)])]),
    );
  }

  #[test]
  fn repeat_form_array() {
    assert_expr("[1; 4]", repeat_array(int(1), int(4)));
  }

  #[test]
  fn repeat_form_requires_literal_count() {
    assert_err("[1; foo]");
  }

  #[test]
  fn repeat_form_missing_count() {
    assert_err("[1;]");
  }

  #[test]
  fn repeat_form_trailing_comma_errors() {
    assert_err("[1; 2,]");
  }

  #[test]
  fn repeat_form_missing_rhs_errors() {
    assert_err("[1; 1 + ]");
  }

  #[test]
  fn repeat_form_missing_closing_bracket_errors() {
    assert_err("[1; 2");
  }

  #[test]
  fn missing_closing_bracket_errors() {
    assert_err("[1, 2");
  }

  #[test]
  fn missing_separator_is_allowed() {
    assert_expr("[1 2]", array(vec![int(1), int(2)]));
  }
}
