#[cfg(test)]
mod match_tests {

  use crate::{ast::expr::ExprKind, parser_utils::ParserContext, tests::support::parse_expression};

  fn parse_single(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "match_expr_test_temp", ParserContext::Match)
  }

  fn assert_ok(input: &str) {
    assert!(parse_single(input).is_ok(), "expected ok for {input:?}");
  }

  fn assert_err(input: &str) {
    assert!(parse_single(input).is_err(), "expected error for {input:?}");
  }

  // Basic match expressions
  #[test]
  fn match_single_arm_expression_body() {
    assert_ok("match x { y => 1 }");
  }

  #[test]
  fn match_single_arm_block_body() {
    assert_ok("match x { y => { 1 } }");
  }

  #[test]
  fn match_multiple_arms() {
    assert_ok("match x { a => 1, b => 2 }");
  }

  #[test]
  fn match_trailing_comma_allowed() {
    assert_ok("match x { a => 1, }");
    assert_ok("match x { a => 1, b => 2, }");
  }

  #[test]
  fn match_nested_in_arm() {
    assert_ok("match x { y => match z { _ => 0 } }");
  }

  #[test]
  fn match_inside_block_expression() {
    assert_ok("{ match x { _ => 0 } }");
  }

  // Guards on match arms
  #[test]
  fn match_arm_with_identifier_guard() {
    assert_ok("match x { y if cond => 1 }");
  }

  #[test]
  fn match_arm_with_guard_block_body() {
    assert_ok("match x { y if cond => { 2 } }");
  }

  #[test]
  fn match_multiple_guards() {
    assert_ok("match x { a if cond1 => 1, b if cond2 => 2 }");
  }

  // Arm attributes
  #[test]
  fn match_arm_with_outer_attribute() {
    assert_ok("match x { #[attr] y => 1 }");
  }

  #[test]
  fn match_arm_with_multiple_attributes() {
    assert_ok("match x { #[a] #[b] y => 1 }");
  }

  #[test]
  fn match_arm_with_attribute_and_guard() {
    assert_ok("match x { #[attr] y if cond => 1 }");
  }

  // Pattern coverage
  #[test]
  fn match_tuple_pattern() {
    assert_ok("match x { (a, b) => a }");
  }

  #[test]
  fn match_wildcard_pattern() {
    assert_ok("match x { _ => 1 }");
  }

  // Unsupported / future work
  #[test]
  #[ignore = "struct literal scrutinee parsing is not implemented yet"]
  fn match_record_struct_literal_scrutinee_not_supported_yet() {
    assert_ok("match (Foo { bar: 1 }) { _ => 0 }");
  }

  #[test]
  #[ignore = "struct pattern parsing is not implemented yet"]
  fn match_struct_pattern_not_supported_yet() {
    assert_ok("match x { Foo { a, b } => a }");
  }

  #[test]
  #[ignore = "tuple struct pattern parsing is not implemented yet"]
  fn match_tuple_struct_pattern_not_supported_yet() {
    assert_ok("match x { Foo(a, b) => a }");
  }

  // Error cases
  #[test]
  fn match_missing_scrutinee_errors() {
    assert_err("match { }");
  }

  #[test]
  fn match_missing_open_brace_errors() {
    assert_err("match x }");
  }

  #[test]
  fn match_missing_close_brace_errors() {
    assert_err("match x { a => 1");
  }

  #[test]
  fn match_arm_missing_arrow_errors() {
    assert_err("match x { a 1 }");
  }

  #[test]
  fn match_arm_missing_body_errors() {
    assert_err("match x { a => }");
  }

  #[test]
  fn match_guard_missing_expression_errors() {
    assert_err("match x { a if => 1 }");
  }

  #[test]
  fn match_guard_without_pattern_errors() {
    assert_err("match x { if cond => 1 }");
  }

  #[test]
  fn match_double_comma_errors() {
    assert_err("match x { a => 1,, b => 2 }");
  }

  #[test]
  fn match_leading_comma_errors() {
    assert_err("match x { , a => 1 }");
  }

  #[test]
  fn stray_tokens_after_match_errors() {
    assert_err("match x { a => 1 } 2");
  }

  #[test]
  fn match_empty_arms_allowed() {
    assert_ok("match x { }");
  }

  #[test]
  fn match_arm_with_parenthesized_pattern() {
    assert_ok("match x { (y) => 1 }");
  }

  #[test]
  fn match_arm_with_or_pattern_simple() {
    assert_ok("match x { a | b => 1 }");
  }

  #[test]
  fn match_arm_with_or_pattern_multiple() {
    assert_ok("match x { a | b | c => 1 }");
  }

  #[test]
  fn match_arm_with_or_pattern_and_guard() {
    assert_ok("match x { a | b if cond => 1 }");
  }

  #[test]
  fn match_range_pattern() {
    assert_ok("match x { 1..=5 => 1 }");
  }

  #[test]
  fn match_literal_pattern() {
    assert_ok("match x { 42 => 1 }");
  }

  #[test]
  fn match_boolean_literal_pattern() {
    assert_ok("match x { true => 1, false => 0 }");
  }

  #[test]
  fn match_nested_match_in_scrutinee() {
    assert_ok("match match x { _ => y } { z => 1 }");
  }

  #[test]
  fn match_arm_missing_pattern_errors() {
    assert_err("match x { => 1 }");
  }

  #[test]
  fn match_arm_missing_fat_arrow_errors() {
    assert_err("match x { a = 1 }");
  }

  #[test]
  fn match_arm_extra_arrow_errors() {
    assert_err("match x { a => => 1 }");
  }

  #[test]
  fn match_guard_after_arrow_errors() {
    assert_err("match x { a => if cond 1 }");
  }

  #[test]
  fn match_or_pattern_without_rhs_errors() {
    assert_err("match x { a | => 1 }");
  }

  #[test]
  fn match_or_pattern_leading_pipe_errors() {
    assert_err("match x { | a => 1 }");
  }

  #[test]
  fn match_guard_before_pattern_errors() {
    assert_err("match x { if cond a => 1 }");
  }

  #[test]
  fn match_arm_with_multiple_guards_errors() {
    assert_err("match x { a if c1 if c2 => 1 }");
  }

  #[test]
  fn match_comma_outside_arm_errors() {
    assert_err("match x , { a => 1 }");
  }

  #[test]
  fn match_arm_trailing_tokens_errors() {
    assert_err("match x { a => 1 b }");
  }
}
