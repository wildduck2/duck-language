#[cfg(test)]
mod if_tests {

  use crate::{ast::expr::ExprKind, parser_utils::ParserContext, tests::support::parse_expression};

  fn parse_single(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "if_expr_test_temp", ParserContext::IfCondition)
  }

  fn assert_ok(input: &str) {
    assert!(parse_single(input).is_ok(), "expected ok for {input:?}");
  }

  fn assert_err(input: &str) {
    assert!(parse_single(input).is_err(), "expected error for {input:?}");
  }

  // Basic if expressions
  #[test]
  fn if_basic() {
    assert_ok("if true { }");
  }

  #[test]
  fn if_with_body() {
    assert_ok("if true { 1 }");
  }

  #[test]
  fn if_nested() {
    assert_ok("if true { if false { } }");
  }

  #[test]
  fn if_inside_block() {
    assert_ok("{ if true { } }");
  }

  // If else expressions
  #[test]
  fn if_else_basic() {
    assert_ok("if true { } else { }");
  }

  #[test]
  fn if_else_if() {
    assert_ok("if true { } else if false { }");
  }

  #[test]
  fn if_else_if_chain() {
    assert_ok("if true { } else if false { } else { }");
  }

  #[test]
  fn if_else_nested_block() {
    assert_ok("if true { } else { if false { } }");
  }

  // If let expressions
  #[test]
  fn if_let_basic() {
    assert_ok("if let x = y { }");
  }

  #[test]
  fn if_let_with_body() {
    assert_ok("if let x = y { x }");
  }

  #[test]
  fn if_let_nested() {
    assert_ok("if let x = y { if let y = z { } }");
  }

  // If let with else
  #[test]
  fn if_let_else_block() {
    assert_ok("if let x = y { } else { }");
  }

  #[test]
  fn if_let_else_if() {
    assert_ok("if let x = y { } else if true { }");
  }

  #[test]
  fn if_let_else_if_let() {
    assert_ok("if let x = y { } else if let y = z { }");
  }

  #[test]
  fn if_let_else_chain() {
    assert_ok("if let x = y { } else if let y = z { } else { }");
  }

  #[test]
  fn if_else_if_let() {
    assert_ok("if true { } else if let x = y { }");
  }

  // Error cases
  #[test]
  fn if_missing_condition_errors() {
    assert_err("if { }");
  }

  #[test]
  fn if_missing_block_errors() {
    assert_err("if true");
  }

  #[test]
  fn else_without_if_errors() {
    assert_err("else { }");
  }

  #[test]
  fn if_else_missing_block_errors() {
    assert_err("if true { } else");
  }

  #[test]
  fn if_missing_then_before_else_errors() {
    assert_err("if true else { }");
  }

  #[test]
  fn if_else_if_missing_block_errors() {
    assert_err("if true { } else if false");
  }

  #[test]
  fn if_let_missing_pattern_errors() {
    assert_err("if let = y { }");
  }

  #[test]
  fn if_let_missing_equals_errors() {
    assert_err("if let x y { }");
  }

  #[test]
  fn if_let_missing_scrutinee_errors() {
    assert_err("if let x = { }");
  }

  #[test]
  fn if_let_missing_block_errors() {
    assert_err("if let x = y");
  }

  #[test]
  fn if_let_missing_then_before_else_errors() {
    assert_err("if let x = y else { }");
  }

  #[test]
  fn if_let_else_if_missing_block_errors() {
    assert_err("if true { } else if let x = y");
  }

  #[test]
  fn stray_tokens_after_if_errors() {
    assert_err("if true { } 1");
  }

  #[test]
  fn stray_tokens_after_if_else_errors() {
    assert_err("if true { } else { } 1");
  }

  #[test]
  fn stray_tokens_after_if_let_errors() {
    assert_err("if let x = y { } 1");
  }

  // continue expressions
  #[test]
  fn continue_basic_errors() {
    assert_err("continue");
  }

  #[test]
  fn continue_with_label_errors() {
    assert_err("continue 'a");
  }

  #[test]
  fn continue_inside_loop() {
    assert_ok("loop { continue }");
  }

  #[test]
  fn continue_inside_labeled_loop() {
    assert_ok("'a: loop { continue 'a }");
  }

  #[test]
  fn continue_with_expression_errors() {
    assert_err("loop { continue 1 }");
  }

  #[test]
  fn continue_double_label_errors() {
    assert_err("loop { continue 'a 'b }");
  }

  #[test]
  fn continue_stray_tokens_errors() {
    assert_err("loop { continue 1 + 2 }");
  }

  // break expressions
  #[test]
  fn break_basic_errors() {
    assert_err("break");
  }

  #[test]
  fn break_with_label_errors() {
    assert_err("break 'a");
  }

  #[test]
  fn break_inside_loop() {
    assert_ok("loop { break }");
  }

  #[test]
  fn break_inside_labeled_loop() {
    assert_ok("'a: loop { break 'a }");
  }

  #[test]
  fn break_with_expression() {
    assert_ok("loop { break 1 }");
  }

  #[test]
  fn break_with_label_and_expression() {
    assert_ok("'a: loop { break 'a }");
  }

  #[test]
  fn break_double_label_errors() {
    assert_err("loop { break 'a 'b }");
  }

  #[test]
  fn break_stray_tokens_errors() {
    assert_err("loop { break 1 2 }");
  }

  // return expressions
  #[test]
  fn return_basic() {
    assert_ok("{ return }");
  }

  #[test]
  fn return_with_expression() {
    assert_ok("{ return 1 }");
  }

  #[test]
  fn return_with_complex_expression() {
    assert_ok("{ return x + y }");
  }

  #[test]
  fn return_inside_nested_block() {
    assert_ok("{ { return } }");
  }

  #[test]
  fn return_with_label_errors() {
    assert_err("{ return 'a }");
  }

  #[test]
  fn return_double_expression_errors() {
    assert_err("{ return 1 2 }");
  }

  #[test]
  fn return_stray_tokens_errors() {
    assert_err("{ return x y }");
  }

  #[test]
  fn if_block_condition_errors() {
    assert_err("if { true } { }");
  }

  #[test]
  fn if_assignment_condition_errors() {
    assert_err("if x = y { }");
  }

  #[test]
  fn if_compound_assignment_condition_errors() {
    assert_err("if x += y { }");
  }

  #[test]
  fn if_range_condition_errors() {
    assert_err("if x..y { }");
  }

  #[test]
  fn if_if_condition_errors() {
    assert_err("if if true { } { }");
  }

  #[test]
  fn if_match_condition_errors() {
    assert_err("if match x { _ => true } { }");
  }

  #[test]
  fn if_loop_condition_errors() {
    assert_err("if loop { } { }");
  }

  #[test]
  fn if_while_condition_errors() {
    assert_err("if while true { } { }");
  }

  #[test]
  fn if_closure_condition_errors() {
    assert_err("if |x| x { }");
  }

  #[test]
  fn if_macro_condition_errors() {
    assert_err("if foo!() { }");
  }

  #[test]
  fn if_await_condition_errors() {
    assert_err("if foo.await { }");
  }

  #[test]
  fn if_try_condition_errors() {
    assert_err("if foo? { }");
  }

  #[test]
  fn if_let_or_pattern_errors() {
    assert_err("if let x | y = z { }");
  }

  #[test]
  fn dangling_else_binds_to_outer_if() {
    assert_ok("if true { if false { } } else { }");
  }

  #[test]
  fn else_if_stray_tokens_errors() {
    assert_err("if true { } else if false 1 { }");
  }

  #[test]
  fn if_let_invalid_scrutinee_errors() {
    assert_err("if let x = y z { }");
  }

  #[test]
  fn continue_inside_block_errors() {
    assert_err("{ continue }");
  }

  #[test]
  fn continue_to_block_label_errors() {
    assert_err("'a: { continue 'a }");
  }

  #[test]
  fn break_with_label_and_expression_errors() {
    assert_err("'a: loop { break 'a 1 }");
  }

  #[test]
  fn return_outside_block_errors() {
    assert_err("return");
  }

  #[test]
  #[ignore = "break values from labeled blocks are not supported yet"]
  fn break_from_labeled_block_with_value() {
    assert_ok("let x = 'a: { break 'a 1 };");
  }

  #[test]
  // #[ignore = "Not yet implemented"]
  fn multiple_control_keywords_errors() {
    assert_err("if true loop { }");
  }

  #[test]
  fn if_let_grouped_pattern() {
    assert_ok("if let (x) = y { }");
  }
}
