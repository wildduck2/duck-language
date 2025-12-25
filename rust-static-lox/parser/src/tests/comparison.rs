#[cfg(test)]
mod comparison_tests {
  use crate::{
    ast::expr::ExprKind,
    parser_utils::ParserContext,
    tests::support::{parse_expression, run_parser},
  };
  use lexer::token::TokenKind;

  fn parse_single(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "comparison_expr_test_temp", ParserContext::Default)
  }

  fn parse_direct(input: &str, context: ParserContext) -> Result<ExprKind, ()> {
    run_parser(input, "comparison_direct_test_temp", |parser| {
      let expr = parser.parse_comparison(context)?;
      parser.advance_till_match(TokenKind::Eof);
      Ok(expr.kind)
    })
  }

  fn assert_ok(input: &str) {
    assert!(parse_single(input).is_ok(), "expected ok for {input:?}");
  }

  fn assert_err(input: &str) {
    assert!(parse_single(input).is_err(), "expected error for {input:?}");
  }

  fn assert_direct_ok(input: &str, context: ParserContext) {
    assert!(
      parse_direct(input, context).is_ok(),
      "expected ok for {input:?}"
    );
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
  fn errors_on_invalid_rhs_tokens() {
    assert_err("1 < )");
    assert_err("1 > ]");
  }

  #[test]
  fn errors_on_invalid_tokens_after_rhs_expression() {
    assert_err("1 < 2 let");
  }

  #[test]
  fn errors_on_invalid_comparison_operators() {
    assert_err("1 <> 2");
    assert_err("1 === 2");
  }

  #[test]
  fn comparison_is_ignored_in_function_context() {
    assert_direct_ok("1 < 2", ParserContext::Function);
  }

  #[test]
  fn comparison_is_ignored_when_followed_by_colon() {
    assert_direct_ok("1 < :", ParserContext::Default);
  }

  #[test]
  fn chained_comparisons_allowed_in_type_context() {
    assert_direct_ok("1 < 2 < 3", ParserContext::Type);
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
