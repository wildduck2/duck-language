#[cfg(test)]
mod comparison_tests {

  use crate::{ast::expr::ExprKind, parser_utils::ParserContext, tests::support::parse_expression};

  fn parse_single(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "comparison_expr_test_temp", ParserContext::Default)
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

  #[test]
  fn parses_all_comparison_operators() {
    assert_ok("1 != 2");
    assert_ok("1 <= 2");
    assert_ok("1 >= 2");
    assert_ok("1 > 2");
  }

  #[test]
  fn comparison_has_lower_precedence_than_arithmetic() {
    assert_ok("1 + 2 < 3 * 4");
  }

  #[test]
  fn comparison_binds_tighter_than_logical() {
    assert_ok("1 < 2 && 3 < 4");
    assert_ok("1 < 2 || 3 < 4");
  }

  #[test]
  fn allows_nested_parenthesized_comparisons() {
    assert_ok("(1 < 2) == (3 < 4)");
  }

  #[test]
  fn comparison_inside_expression() {
    assert_ok("1 + (2 < 3)");
  }

  #[test]
  fn errors_on_missing_comparison_operands() {
    assert_err("1 <");
    assert_err("< 2");
    assert_err("1 ==");
    assert_err("== 2");
  }

  #[test]
  fn errors_on_invalid_comparison_operators() {
    assert_err("1 <> 2");
    assert_err("1 === 2");
  }

  #[test]
  fn comparison_in_if_condition() {
    assert_ok("if 1 < 2 { }");
  }

  #[test]
  fn comparison_in_while_condition() {
    assert_ok("while 1 < 2 { }");
  }

  #[test]
  fn comparison_with_cast() {
    assert_ok("1 as i32 < 2");
    assert_ok("1 < 2 as i32");
  }
}
