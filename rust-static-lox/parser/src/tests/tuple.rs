#[cfg(test)]
mod tuple_tests {

  use crate::{
    ast::{ExprKind, Lit},
    parser_utils::ExprContext,
    tests::support::parse_primary_expr,
  };

  fn parse_single(input: &str) -> Result<ExprKind, ()> {
    parse_primary_expr(input, "tuple_test_temp", ExprContext::Default)
  }

  fn assert_err(input: &str) {
    assert!(
      parse_single(input).is_err(),
      "expected error for: {:?}",
      input
    );
  }

  #[derive(Debug, PartialEq)]
  enum SimpleExpr {
    Int(i128),
    Tuple(Vec<SimpleExpr>),
    Group(Box<SimpleExpr>),
  }

  fn int(value: i128) -> SimpleExpr {
    SimpleExpr::Int(value)
  }

  fn tuple(elements: Vec<SimpleExpr>) -> SimpleExpr {
    SimpleExpr::Tuple(elements)
  }

  fn group(inner: SimpleExpr) -> SimpleExpr {
    SimpleExpr::Group(Box::new(inner))
  }

  fn simplify(expr: &ExprKind) -> SimpleExpr {
    match expr {
      ExprKind::Literal(Lit::Integer { value, .. }) => int(*value),
      ExprKind::Tuple { elements } => tuple(
        elements
          .iter()
          .map(|element| simplify(&element.kind))
          .collect(),
      ),
      ExprKind::Group { expr } => group(simplify(&expr.kind)),
      _ => panic!("unsupported expression in tuple tests: {:?}", expr),
    }
  }

  fn assert_expr(input: &str, expected: SimpleExpr) {
    let expr = parse_single(input).unwrap();
    assert_eq!(simplify(&expr), expected);
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
  fn multiple_trailing_commas_allowed() {
    assert_expr("(1,,)", tuple(vec![int(1)]));
  }

  #[test]
  fn leading_comma_allowed() {
    assert_expr("(,1)", group(int(1)));
  }

  #[test]
  fn many_leading_and_trailing_commas() {
    assert_expr("(,,1,,)", tuple(vec![int(1)]));
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
  fn empty_but_with_comma_is_unit_tuple() {
    assert_expr("(,)", tuple(vec![]));
  }

  #[test]
  fn deeply_nested_group_and_tuple() {
    assert_expr(
      "(((1,), (2)),)",
      tuple(vec![tuple(vec![tuple(vec![int(1)]), group(int(2))])]),
    );
  }
}
