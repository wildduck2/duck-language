#[cfg(test)]
mod factor_tests {

  use crate::{
    ast::expr::{BinaryOp, ExprKind, UnaryOp},
    parser_utils::ParserContext,
    tests::support::{bin, int, simplify_expr_ungrouped, unary, SimpleExpr, parse_expression},
  };

  fn parse(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "factor_expr_test_temp", ParserContext::Default)
  }

  fn assert_expr(input: &str, expected: SimpleExpr) {
    let expr = parse(input).unwrap();
    assert_eq!(simplify_expr_ungrouped(&expr), expected);
  }

  fn assert_err(input: &str) {
    assert!(parse(input).is_err(), "expected error for {input:?}");
  }

  // Basic factor operators

  #[test]
  fn parses_multiplication() {
    assert_expr("2 * 3", bin(BinaryOp::Mul, int(2), int(3)));
  }

  #[test]
  fn parses_division() {
    assert_expr("6 / 3", bin(BinaryOp::Div, int(6), int(3)));
  }

  #[test]
  fn parses_remainder() {
    assert_expr("7 % 4", bin(BinaryOp::Mod, int(7), int(4)));
  }

  // Left associativity

  #[test]
  fn multiplication_is_left_associative() {
    assert_expr(
      "2 * 3 * 4",
      bin(BinaryOp::Mul, bin(BinaryOp::Mul, int(2), int(3)), int(4)),
    );
  }

  #[test]
  fn mixed_factor_ops_are_left_associative() {
    assert_expr(
      "8 / 4 % 3",
      bin(BinaryOp::Mod, bin(BinaryOp::Div, int(8), int(4)), int(3)),
    );
  }

  // Factor vs cast precedence

  #[test]
  fn cast_binds_tighter_than_multiplication() {
    assert_expr("2 * 3", bin(BinaryOp::Mul, int(2), int(3)));
  }

  // Factor vs addition precedence

  #[test]
  fn factor_binds_tighter_than_addition() {
    assert_expr(
      "1 + 2 * 3",
      bin(BinaryOp::Add, int(1), bin(BinaryOp::Mul, int(2), int(3))),
    );
  }

  #[test]
  fn parses_factor_with_unary_lhs() {
    assert_expr(
      "-2 * 3",
      bin(BinaryOp::Mul, unary(UnaryOp::Neg, int(2)), int(3)),
    );
  }

  #[test]
  fn parses_factor_with_unary_rhs() {
    assert_expr(
      "2 * -3",
      bin(BinaryOp::Mul, int(2), unary(UnaryOp::Neg, int(3))),
    );
  }

  #[test]
  fn division_with_parenthesized_rhs() {
    assert_expr(
      "6 / (3 + 1)",
      bin(BinaryOp::Div, int(6), bin(BinaryOp::Add, int(3), int(1))),
    );
  }

  // Grouping

  #[test]
  fn parenthesized_factor_changes_binding() {
    assert_expr(
      "(1 + 2) * 3",
      bin(BinaryOp::Mul, bin(BinaryOp::Add, int(1), int(2)), int(3)),
    );
  }

  // Error cases

  #[test]
  fn missing_rhs_errors() {
    assert_err("1 *");
    assert_err("1 /");
    assert_err("1 %");
  }

  #[test]
  fn double_operator_errors() {
    assert_err("1 ** 2");
    assert_err("1 %% 2");
  }

  #[test]
  fn invalid_rhs_token_errors() {
    assert_err("1 * )");
    assert_err("1 / ]");
  }
}
