#[cfg(test)]
mod block_tests {

  use crate::{ast::expr::ExprKind, parser_utils::ExprContext, tests::support::parse_expression};

  fn parse_single(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "block_expr_test_temp", ExprContext::Block)
  }

  fn assert_ok(input: &str) {
    assert!(parse_single(input).is_ok(), "expected ok for {input:?}");
  }

  fn assert_err(input: &str) {
    assert!(parse_single(input).is_err(), "expected error for {input:?}");
  }

  // ============================
  // Basic block expressions
  // ============================

  #[test]
  fn empty_block() {
    assert_ok("{}");
  }

  #[test]
  fn block_with_single_expression() {
    assert_ok("{ 1 }");
  }

  #[test]
  fn block_with_statements_and_trailing_expr() {
    assert_ok("{ let x = 1; let y = 2; x + y }");
  }

  #[test]
  fn block_with_multiple_expressions() {
    assert_ok("{ 1; 2; 3 }");
  }

  #[test]
  fn nested_blocks() {
    assert_ok("{ { 1 } { 2 } }");
  }

  // ============================
  // Block attributes
  // ============================

  #[test]
  fn block_outer_attribute_not_supported() {
    assert_err("#[attr] { 1 }");
  }

  #[test]
  fn block_multiple_outer_attributes_not_supported() {
    assert_err("#[a] #[b] { 1 }");
  }

  #[test]
  fn block_rejects_inner_attribute() {
    assert_err("{ #![inner] 1 }");
  }

  // ============================
  // Unsafe block expressions
  // ============================

  #[test]
  fn unsafe_block_basic() {
    assert_ok("unsafe { 1 }");
  }

  #[test]
  fn unsafe_empty_block() {
    assert_ok("unsafe {}");
  }

  #[test]
  fn unsafe_block_with_attributes_unsupported() {
    assert_err("#[attr] unsafe { 1 }");
  }

  #[test]
  fn nested_unsafe_blocks() {
    assert_ok("unsafe { unsafe { 1 } }");
  }

  // ============================
  // Async block expressions
  // ============================

  #[test]
  fn async_block_basic() {
    assert_ok("async { 1 }");
  }

  #[test]
  fn async_move_block() {
    assert_ok("async move { 1 }");
  }

  #[test]
  fn async_block_with_attributes_unsupported() {
    assert_err("#[attr] async { 1 }");
  }

  #[test]
  fn async_move_block_with_attributes_unsupported() {
    assert_err("#[attr] async move { 1 }");
  }

  #[test]
  fn move_async_order_errors() {
    assert_err("move async { 1 }");
  }

  #[test]
  fn double_move_async_errors() {
    assert_err("async move move { 1 }");
  }

  #[test]
  fn nested_async_blocks() {
    assert_ok("async { async { 1 } }");
  }

  // ============================
  // Try block expressions
  // ============================

  #[test]
  fn try_block_basic() {
    assert_ok("try { 1 }");
  }

  #[test]
  fn try_empty_block() {
    assert_ok("try {}");
  }

  #[test]
  fn try_block_with_attributes_unsupported() {
    assert_err("#[attr] try { 1 }");
  }

  #[test]
  fn try_move_not_allowed() {
    assert_err("try move { 1 }");
  }

  #[test]
  fn nested_try_blocks() {
    assert_ok("try { try { 1 } }");
  }

  // ============================
  // Mixed combinations
  // ============================

  #[test]
  fn async_inside_unsafe_block() {
    assert_ok("unsafe { async { 1 } }");
  }

  #[test]
  fn unsafe_inside_async_block() {
    assert_ok("async { unsafe { 1 } }");
  }

  #[test]
  fn try_inside_async_block() {
    assert_ok("async { try { 1 } }");
  }

  #[test]
  fn async_inside_try_block() {
    assert_ok("try { async { 1 } }");
  }

  // ============================
  // Error cases
  // ============================

  #[test]
  fn block_missing_closing_brace_errors() {
    assert_err("{ 1");
  }

  #[test]
  fn unsafe_without_block_errors() {
    assert_err("unsafe 1");
  }

  #[test]
  fn async_without_block_errors() {
    assert_err("async 1");
  }

  #[test]
  fn async_move_without_block_errors() {
    assert_err("async move 1");
  }

  #[test]
  fn block_missing_open_brace_errors() {
    assert_err("1 }");
  }

  #[test]
  fn try_without_block_errors() {
    assert_err("try 1");
  }

  #[test]
  fn attributes_without_block_errors() {
    assert_err("#[attr] 1");
  }

  #[test]
  fn stray_tokens_after_block_errors() {
    assert_err("{ 1 } 2");
  }

  #[test]
  fn unsafe_stray_tokens_after_block_errors() {
    assert_err("unsafe { 1 } 2");
  }

  #[test]
  fn async_stray_tokens_after_block_errors() {
    assert_err("async { 1 } 2");
  }

  #[test]
  fn try_stray_tokens_after_block_errors() {
    assert_err("try { 1 } 2");
  }
}
