#[cfg(test)]
mod loop_tests {

  use crate::{ast::expr::ExprKind, parser_utils::ExprContext, tests::support::parse_expression};

  fn parse_single(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "loop_expr_test_temp", ExprContext::LoopCondition)
  }

  fn assert_ok(input: &str) {
    assert!(parse_single(input).is_ok(), "expected ok for {input:?}");
  }

  fn assert_err(input: &str) {
    assert!(parse_single(input).is_err(), "expected error for {input:?}");
  }

  // Loop expressions

  #[test]
  fn loop_empty_block() {
    assert_ok("loop { }");
  }

  #[test]
  fn loop_with_statements() {
    assert_ok("loop { 1; 2; }");
  }

  #[test]
  fn loop_nested() {
    assert_ok("loop { loop { } }");
  }

  #[test]
  fn loop_with_label() {
    assert_ok("'search: loop { }");
  }

  #[test]
  fn loop_inside_block() {
    assert_ok("{ loop { } }");
  }

  // While expressions

  #[test]
  fn while_true_literal() {
    assert_ok("while true { }");
  }

  #[test]
  fn while_nested() {
    assert_ok("while true { while false { } }");
  }

  #[test]
  fn while_with_label() {
    assert_ok("'guard: while true { }");
  }

  #[test]
  fn while_inside_loop() {
    assert_ok("loop { while true { } }");
  }

  // Labeled block expressions

  #[test]
  fn labeled_block_basic() {
    assert_ok("'a: { }");
  }

  #[test]
  fn labeled_block_nested() {
    assert_ok("'a: { 'b: { } }");
  }

  #[test]
  fn loop_inside_labeled_block() {
    assert_ok("'a: { loop { } }");
  }

  // Unsupported constructs (documented for future work)

  #[test]
  fn loop_outer_attributes_not_supported() {
    assert_err("#[attr] loop { }");
    assert_err("#[first] #[second] loop { }");
  }

  #[test]
  fn while_outer_attributes_not_supported() {
    assert_err("#[attr] while true { }");
  }

  #[test]
  fn for_loops_not_supported_yet() {
    assert_err("for x in y { }");
    assert_err("'label: for x in y { }");
  }

  #[test]
  fn while_comparison_condition_not_supported_yet() {
    assert_err("while x < 10 { x = x + 1; }");
  }

  #[test]
  fn while_identifier_condition_not_supported_yet() {
    assert_err("while ready { }");
  }

  #[test]
  fn while_unary_condition_not_supported_yet() {
    assert_err("while !done { done; }");
  }

  #[test]
  fn while_let_not_supported_yet() {
    assert_err("while let Some(x) = y { }");
    assert_err("'label: while let Some(x) = y { }");
  }

  // Error cases for supported constructs

  #[test]
  fn loop_missing_block_errors() {
    assert_err("loop");
  }

  #[test]
  fn while_missing_condition_errors() {
    assert_err("while { }");
  }

  #[test]
  fn while_missing_block_errors() {
    assert_err("while true");
  }

  #[test]
  fn label_without_colon_errors() {
    assert_err("'a loop { }");
  }

  #[test]
  fn colon_without_label_errors() {
    assert_err(": loop { }");
  }

  #[test]
  fn labeled_block_missing_block_errors() {
    assert_err("'a:");
  }

  #[test]
  fn stray_tokens_after_loop_errors() {
    assert_err("loop { } 1");
  }

  #[test]
  fn stray_tokens_after_while_errors() {
    assert_err("while true { } 1");
  }

  #[test]
  fn infinite_loop_with_break() {
    assert_ok("loop { break; }");
  }

  #[test]
  fn infinite_loop_with_continue() {
    assert_ok("loop { continue; }");
  }

  #[test]
  fn loop_with_break_value() {
    assert_ok("loop { break 1; }");
  }

  #[test]
  fn labeled_break() {
    assert_ok("'a: loop { break 'a; }");
  }

  #[test]
  fn labeled_continue() {
    assert_ok("'a: loop { continue 'a; }");
  }

  #[test]
  fn while_grouped_condition() {
    assert_ok("while (true) { }");
  }

  #[test]
  fn loop_block_with_expression_tail() {
    assert_ok("loop { 1 }");
  }

  #[test]
  fn while_block_with_expression_tail() {
    assert_ok("while true { 1 }");
  }

  #[test]
  fn nested_labeled_loops() {
    assert_ok("'a: loop { 'b: loop { break 'a; } }");
  }

  #[test]
  fn break_outside_loop_errors() {
    assert_err("break;");
  }

  #[test]
  fn continue_outside_loop_errors() {
    assert_err("continue;");
  }

  #[test]
  fn label_on_non_loop_block_errors() {
    assert_err("'a: 1");
  }
}
