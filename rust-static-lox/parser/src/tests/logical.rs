#[cfg(test)]
mod logical_tests {

  use crate::{
    ast::expr::{BinaryOp, ExprKind},
    parser_utils::ParserContext,
    tests::support::{bin, int, simplify_expr_ungrouped, SimpleExpr, parse_expression},
  };

  fn parse(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "logical_expr_test_temp", ParserContext::Default)
  }

  fn assert_expr(input: &str, expected: SimpleExpr) {
    let expr = parse(input).unwrap();
    assert_eq!(simplify_expr_ungrouped(&expr), expected);
  }

  fn assert_err(input: &str) {
    assert!(parse(input).is_err(), "expected error for: {input:?}");
  }

  #[test]
  fn parses_logical_or() {
    assert_expr(
      "1 || 0",
      bin(BinaryOp::Or, int(1), int(0)),
    );
  }

  #[test]
  fn parses_logical_and() {
    assert_expr(
      "1 && 0",
      bin(BinaryOp::And, int(1), int(0)),
    );
  }

  #[test]
  fn logical_and_has_higher_precedence_than_or() {
    assert_expr(
      "1 || 0 && 2",
      bin(
        BinaryOp::Or,
        int(1),
        bin(BinaryOp::And, int(0), int(2)),
      ),
    );
  }

  #[test]
  fn chains_logical_and_left_associatively() {
    assert_expr(
      "1 && 2 && 3",
      bin(
        BinaryOp::And,
        bin(BinaryOp::And, int(1), int(2)),
        int(3),
      ),
    );
  }

  #[test]
  fn errors_when_or_missing_rhs() {
    assert_err("true ||");
  }

  #[test]
  fn errors_when_and_missing_rhs() {
    assert_err("true &&");
  }

  #[test]
  fn parentheses_override_precedence() {
    assert_expr(
      "(1 || 0) && 2",
      bin(
        BinaryOp::And,
        bin(BinaryOp::Or, int(1), int(0)),
        int(2),
      ),
    );
  }

  #[test]
  fn single_and_is_not_logical_and() {
    assert_err("true & false");
  }

  #[test]
  fn single_or_is_not_logical_or() {
    assert_err("true | false");
  }
}
