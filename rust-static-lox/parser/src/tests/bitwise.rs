#[cfg(test)]
mod bitwise_tests {

  use crate::{
    ast::expr::{BinaryOp, ExprKind},
    parser_utils::ParserContext,
    tests::support::{bin, int, simplify_expr_ungrouped, SimpleExpr, parse_expression},
  };

  fn parse(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "bitwise_expr_test_temp", ParserContext::Default)
  }

  fn assert_expr(input: &str, expected: SimpleExpr) {
    let expr = parse(input).unwrap();
    assert_eq!(simplify_expr_ungrouped(&expr), expected);
  }

  fn assert_err(input: &str) {
    assert!(parse(input).is_err(), "expected error for: {input:?}");
  }

  #[test]
  fn parses_bitwise_or_chain() {
    assert_expr(
      "1 | 2 | 3",
      bin(
        BinaryOp::BitOr,
        bin(BinaryOp::BitOr, int(1), int(2)),
        int(3),
      ),
    );
  }

  #[test]
  fn parses_bitwise_xor_chain() {
    assert_expr(
      "5 ^ 6 ^ 7",
      bin(
        BinaryOp::BitXor,
        bin(BinaryOp::BitXor, int(5), int(6)),
        int(7),
      ),
    );
  }

  #[test]
  fn parses_bitwise_and_chain() {
    assert_expr(
      "8 & 4 & 2",
      bin(
        BinaryOp::BitAnd,
        bin(BinaryOp::BitAnd, int(8), int(4)),
        int(2),
      ),
    );
  }

  #[test]
  fn respects_bitwise_precedence() {
    assert_expr(
      "1 | 2 & 3 ^ 4",
      bin(
        BinaryOp::BitOr,
        int(1),
        bin(
          BinaryOp::BitXor,
          bin(BinaryOp::BitAnd, int(2), int(3)),
          int(4),
        ),
      ),
    );
  }

  #[test]
  fn bitwise_or_stops_before_comparison() {
    assert_expr("1 == 2", bin(BinaryOp::Eq, int(1), int(2)));
  }

  #[test]
  fn errors_when_or_missing_rhs() {
    assert_err("1 |");
  }

  #[test]
  fn errors_when_xor_missing_rhs() {
    assert_err("1 ^");
  }

  #[test]
  fn errors_when_and_missing_rhs() {
    assert_err("1 &");
  }

  #[test]
  fn errors_on_double_operators() {
    assert_err("1 | )");
    assert_err("1 ^ )");
    assert_err("1 & )");
  }

  #[test]
  fn if_condition_rejects_bitwise_or() {
    let result =
      parse_expression("1 | 2", "bitwise_if_condition", ParserContext::IfCondition);
    assert!(result.is_err(), "expected error in if condition context");
  }

  #[test]
  fn match_context_short_circuits_bitwise_or() {
    let expr = parse_expression("1", "bitwise_match_context", ParserContext::Match).unwrap();
    assert_eq!(simplify_expr_ungrouped(&expr), int(1));
  }

  #[test]
  fn errors_when_or_rhs_parse_fails() {
    assert_err("1 | 2 ^");
  }

  #[test]
  fn errors_when_xor_rhs_parse_fails() {
    assert_err("1 ^ 2 &");
  }

  #[test]
  fn errors_when_and_rhs_parse_fails() {
    assert_err("1 & 2 <<");
  }
}
