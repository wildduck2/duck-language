#[cfg(test)]
mod tuple_tests {

  use crate::{
    ast::ExprKind,
    parser_utils::ParserContext,
    tests::support::{group, int, simplify_expr, tuple, SimpleExpr, parse_primary_expr},
  };

  fn parse_single(input: &str) -> Result<ExprKind, ()> {
    parse_primary_expr(input, "tuple_test_temp", ParserContext::Default)
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
  fn grouped_single_expression() {
    assert_expr("(1)", group(int(1)));
  }

  #[test]
  fn grouped_nested() {
    assert_expr("((1))", group(group(int(1))));
  }

  #[test]
  fn unit_expression() {
    assert_expr("()", tuple(vec![]));
  }

  #[test]
  fn single_element_tuple() {
    assert_expr("(1,)", tuple(vec![int(1)]));
  }

  #[test]
  fn two_element_tuple() {
    assert_expr("(1, 2)", tuple(vec![int(1), int(2)]));
  }

  #[test]
  fn three_element_tuple() {
    assert_expr("(1, 2, 3)", tuple(vec![int(1), int(2), int(3)]));
  }

  #[test]
  fn trailing_comma_tuple() {
    assert_expr("(1, 2, 3,)", tuple(vec![int(1), int(2), int(3)]));
  }

  #[test]
  fn nested_tuple() {
    assert_expr(
      "(1, (2, 3))",
      tuple(vec![int(1), tuple(vec![int(2), int(3)])]),
    );
  }

  #[test]
  fn grouped_inside_tuple() {
    assert_expr("((1), 2)", tuple(vec![group(int(1)), int(2)]));
  }

  #[test]
  fn tuple_inside_group() {
    assert_expr("((1, 2))", group(tuple(vec![int(1), int(2)])));
  }

  #[test]
  fn empty_group_inside_tuple() {
    assert_expr("((), 1)", tuple(vec![tuple(vec![]), int(1)]));
  }

  #[test]
  fn deeply_nested_group_and_tuple() {
    assert_expr(
      "(((1,), (2)),)",
      tuple(vec![tuple(vec![tuple(vec![int(1)]), group(int(2))])]),
    );
  }

  #[test]
  fn missing_closing_paren_errors() {
    assert_err("(1, 2");
  }

  #[test]
  fn missing_comma_between_elements_errors() {
    assert_err("(1 2)");
  }

  #[test]
  fn dangling_open_paren_errors() {
    assert_err("(");
  }

  #[test]
  fn leading_comma_errors() {
    assert_err("(,1)");
  }

  #[test]
  fn double_comma_errors() {
    assert_err("(1,,2)");
  }

  #[test]
  fn only_commas_errors() {
    assert_err("(,,)");
  }

  #[test]
  fn trailing_double_comma_errors() {
    assert_err("(1,,)");
  }

  #[test]
  fn tuple_followed_by_extra_tokens_errors() {
    assert_err("(1, 2),");
  }

  #[test]
  fn grouped_unit_tuple() {
    // (()) is a group containing the unit tuple
    assert_expr("(())", group(tuple(vec![])));
  }

  #[test]
  fn unit_tuple_inside_group_inside_group() {
    // Nested grouping around unit tuple
    assert_expr("((()))", group(group(tuple(vec![]))));
  }
}
