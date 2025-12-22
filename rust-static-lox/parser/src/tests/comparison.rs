#[cfg(test)]
mod comparison_tests {

  use crate::{ast::expr::ExprKind, parser_utils::ExprContext, tests::support::parse_expression};

  fn parse_single(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "comparison_expr_test_temp", ExprContext::Default)
  }

  fn assert_ok(input: &str) {
    assert!(parse_single(input).is_ok(), "expected ok for {input:?}");
  }

  fn assert_err(input: &str) {
    assert!(parse_single(input).is_err(), "expected error for {input:?}");
  }

  #[test]
  fn parses_simple_comparison() {
    assert_ok("1 < 2");
    assert_ok("1 == 2");
  }

  #[test]
  fn rejects_chained_comparisons() {
    for src in ["1 < 2 < 3", "1 == 2 == 3", "1 < 2 <= 3", "1 != 2 > 3"] {
      assert_err(src);
    }
  }

  #[test]
  fn allows_parenthesized_comparisons() {
    assert_ok("(1 < 2) < 3");
    assert_ok("1 < (2 < 3)");
  }
}
