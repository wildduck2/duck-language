#[cfg(test)]
mod closure_tests {

  use crate::{
    ast::expr::ExprKind, parser_utils::ExprContext, tests::support::parse_expression_expr,
  };

  fn parse_single(input: &str) -> Result<ExprKind, ()> {
    parse_expression_expr(input, "closure_expr_test_temp", ExprContext::Closure)
  }

  fn assert_ok(input: &str) {
    assert!(parse_single(input).is_ok(), "expected ok for {input:?}");
  }

  fn assert_err(input: &str) {
    assert!(parse_single(input).is_err(), "expected error for {input:?}");
  }

  // Basic closures
  #[test]
  fn closure_no_params_expr_body() {
    assert_ok("|| 1");
  }

  #[test]
  fn closure_no_params_block_body() {
    assert_ok("|| { 1 }");
  }

  #[test]
  fn closure_single_param() {
    assert_ok("|x| x");
  }

  #[test]
  fn closure_multiple_params() {
    assert_ok("|x, y| x + y");
  }

  #[test]
  fn closure_trailing_comma() {
    assert_ok("|x, y,| x + y");
  }

  // move and async
  #[test]
  fn closure_move() {
    assert_ok("move |x| x");
  }

  #[test]
  fn closure_async() {
    assert_ok("async |x| x");
  }

  #[test]
  fn closure_async_move() {
    assert_ok("async move |x| x");
  }

  #[test]
  fn closure_move_async() {
    assert_ok("move async |x| x");
  }

  #[test]
  fn closure_async_no_params() {
    assert_ok("async || 1");
  }

  // Typed parameters
  #[test]
  fn closure_typed_param() {
    assert_ok("|x: i32| x");
  }

  #[test]
  fn closure_multiple_typed_params() {
    assert_ok("|x: i32, y: i32| x + y");
  }

  #[test]
  fn closure_mixed_typed_and_untyped() {
    assert_ok("|x, y: i32| y");
  }

  #[test]
  fn closure_pattern_param() {
    assert_ok("|(x, y)| x");
  }

  #[test]
  fn closure_ref_pattern_param() {
    assert_ok("|&x| x");
  }

  #[test]
  fn closure_mut_pattern_param() {
    assert_ok("|mut x| x");
  }

  // Attributes on params
  #[test]
  fn closure_param_with_attribute() {
    assert_ok("|#[attr] x| x");
  }

  #[test]
  fn closure_multiple_params_with_attributes() {
    assert_ok("|#[a] x, #[b] y| x");
  }

  // Explicit return type
  #[test]
  fn closure_return_type_block() {
    assert_ok("|x| -> i32 { x }");
  }

  #[test]
  fn closure_no_params_return_type() {
    assert_ok("|| -> i32 { 1 }");
  }

  #[test]
  fn closure_async_return_type() {
    assert_ok("async |x| -> i32 { x }");
  }

  // Nested closures
  #[test]
  fn closure_inside_closure() {
    assert_ok("|x| || x");
  }

  #[test]
  fn closure_returning_closure() {
    assert_ok("|| |x| x");
  }

  // Error cases
  #[test]
  fn closure_missing_body_errors() {
    assert_err("|x|");
  }

  #[test]
  fn closure_missing_pipe_errors() {
    assert_err("|x x");
  }

  #[test]
  fn closure_double_comma_errors() {
    assert_err("|x,, y| x");
  }

  #[test]
  fn closure_leading_comma_errors() {
    assert_err("|,x| x");
  }

  #[test]
  fn closure_missing_closing_pipe_errors() {
    assert_err("|x y| x");
  }

  #[test]
  fn closure_return_type_without_block_errors() {
    assert_err("|x| -> i32 x");
  }

  #[test]
  fn closure_async_without_closure_errors() {
    assert_err("async x");
  }

  #[test]
  fn closure_move_without_closure_errors() {
    assert_err("move x");
  }

  #[test]
  fn closure_arrow_without_type_errors() {
    assert_err("|x| -> { x }");
  }

  #[test]
  fn closure_invalid_param_syntax_errors() {
    assert_err("|x:| x");
  }

  #[test]
  fn closure_stray_tokens_after_errors() {
    assert_err("|x| x 1");
  }
}
