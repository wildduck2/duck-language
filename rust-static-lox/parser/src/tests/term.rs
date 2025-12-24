#[cfg(test)]
mod term_tests {

  use crate::{
    ast::expr::{BinaryOp, ExprKind, UnaryOp},
    parser_utils::ParserContext,
    tests::support::{bin, int, simplify_expr_ungrouped, unary, SimpleExpr, parse_expression},
  };

  fn parse(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "term_expr_test_temp", ParserContext::Default)
  }

  fn assert_expr(input: &str, expected: SimpleExpr) {
    let expr = parse(input).unwrap();
    assert_eq!(simplify_expr_ungrouped(&expr), expected);
  }

  fn assert_err(input: &str) {
    assert!(parse(input).is_err(), "expected error for {input:?}");
  }

  // Basic term operators

  #[test]
  fn parses_addition() {
    assert_expr("1 + 2", bin(BinaryOp::Add, int(1), int(2)));
  }

  #[test]
  fn parses_subtraction() {
    assert_expr("3 - 1", bin(BinaryOp::Sub, int(3), int(1)));
  }

  // Left associativity

  #[test]
  fn addition_is_left_associative() {
    assert_expr(
      "1 + 2 + 3",
      bin(BinaryOp::Add, bin(BinaryOp::Add, int(1), int(2)), int(3)),
    );
  }

  #[test]
  fn subtraction_is_left_associative() {
    assert_expr(
      "5 - 2 - 1",
      bin(BinaryOp::Sub, bin(BinaryOp::Sub, int(5), int(2)), int(1)),
    );
  }

  #[test]
  fn mixed_term_ops_are_left_associative() {
    assert_expr(
      "1 + 2 - 3",
      bin(BinaryOp::Sub, bin(BinaryOp::Add, int(1), int(2)), int(3)),
    );
  }

  // Term vs factor precedence

  #[test]
  fn factor_binds_tighter_than_addition() {
    assert_expr(
      "1 + 2 * 3",
      bin(BinaryOp::Add, int(1), bin(BinaryOp::Mul, int(2), int(3))),
    );
  }

  // Unary precedence

  #[test]
  fn unary_binds_tighter_than_term() {
    assert_expr(
      "1 + -2",
      bin(BinaryOp::Add, int(1), unary(UnaryOp::Neg, int(2))),
    );
  }

  // Grouping

  #[test]
  fn parenthesized_term_changes_binding() {
    assert_expr(
      "(1 + 2) * 3",
      bin(BinaryOp::Mul, bin(BinaryOp::Add, int(1), int(2)), int(3)),
    );
  }

  // Error cases

  #[test]
  fn missing_rhs_errors() {
    assert_err("1 +");
    assert_err("1 -");
  }

  #[test]
  fn invalid_rhs_errors() {
    assert_err("1 + +");
    assert_err("1 - / 2");
  }
}
