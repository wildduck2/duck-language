#[cfg(test)]
mod block_tests {

  use crate::{
    ast::{expr::ExprKind, Stmt},
    parser_utils::ParserContext,
    tests::support::{parse_expression, run_parser},
  };
  use lexer::token::TokenKind;

  fn parse_single(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "block_expr_test_temp", ParserContext::Block)
  }

  fn assert_ok(input: &str) {
    assert!(parse_single(input).is_ok(), "expected ok for {input:?}");
  }

  fn assert_err(input: &str) {
    assert!(parse_single(input).is_err(), "expected error for {input:?}");
  }

  fn parse_stmt_expr(input: &str) -> Result<ExprKind, ()> {
    run_parser(input, "block_stmt_test_temp", |parser| {
      match parser.parse_stmt(ParserContext::Default)? {
        Stmt::Expr { expr, .. } => Ok(expr.kind),
        other => panic!("expected expr stmt, got: {:?}", other),
      }
    })
  }

  fn parse_flavor(input: &str, context: ParserContext) -> Result<(), ()> {
    run_parser(input, "block_flavor_test_temp", |parser| {
      parser.parse_block_expression_flavors(context)?;
      parser.advance_till_match(TokenKind::Eof);
      Ok(())
    })
  }

  fn can_start_block_expr(input: &str) -> Result<bool, ()> {
    run_parser(input, "block_start_test_temp", |parser| {
      let can_start = parser.can_start_block_expression();
      parser.advance_till_match(TokenKind::Eof);
      Ok(can_start)
    })
  }

  // Basic block expressions
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

  #[test]
  #[ignore = "block tail expressions are not stored yet"]
  fn block_tail_expression_is_recorded() {
    match parse_single("{ 1 }").unwrap() {
      ExprKind::Block { tail, .. } => {
        assert!(tail.is_some(), "expected tail expression in block");
      },
      other => panic!("expected block expr, got: {:?}", other),
    }
  }

  // Block attributes
  #[test]
  fn block_outer_attribute_on_stmt() {
    match parse_stmt_expr("#[attr] { 1 }").unwrap() {
      ExprKind::Block { outer_attributes, .. } => {
        assert_eq!(outer_attributes.len(), 1);
      },
      other => panic!("expected block expr, got: {:?}", other),
    }
  }

  #[test]
  fn block_multiple_outer_attributes_on_stmt() {
    match parse_stmt_expr("#[a] #[b] { 1 }").unwrap() {
      ExprKind::Block { outer_attributes, .. } => {
        assert_eq!(outer_attributes.len(), 2);
      },
      other => panic!("expected block expr, got: {:?}", other),
    }
  }

  #[test]
  fn block_rejects_inner_attribute() {
    assert_err("{ #![inner] 1 }");
  }

  // Unsafe block expressions

  #[test]
  fn unsafe_block_basic() {
    assert_ok("unsafe { 1 }");
  }

  #[test]
  fn unsafe_empty_block() {
    assert_ok("unsafe {}");
  }

  #[test]
  fn unsafe_block_with_attributes_on_stmt() {
    match parse_stmt_expr("#[attr] unsafe { 1 }").unwrap() {
      ExprKind::Block { outer_attributes, .. } => {
        assert_eq!(outer_attributes.len(), 1);
      },
      other => panic!("expected unsafe block expr, got: {:?}", other),
    }
  }

  #[test]
  fn nested_unsafe_blocks() {
    assert_ok("unsafe { unsafe { 1 } }");
  }

  // Async block expressions

  #[test]
  fn async_block_basic() {
    assert_ok("async { 1 }");
  }

  #[test]
  fn async_move_block() {
    assert_ok("async move { 1 }");
  }

  #[test]
  fn async_block_with_attributes_on_stmt() {
    match parse_stmt_expr("#[attr] async { 1 }").unwrap() {
      ExprKind::Block { outer_attributes, .. } => {
        assert_eq!(outer_attributes.len(), 1);
      },
      other => panic!("expected async block expr, got: {:?}", other),
    }
  }

  #[test]
  fn async_move_block_with_attributes_on_stmt() {
    match parse_stmt_expr("#[attr] async move { 1 }").unwrap() {
      ExprKind::Block { outer_attributes, .. } => {
        assert_eq!(outer_attributes.len(), 1);
      },
      other => panic!("expected async move block expr, got: {:?}", other),
    }
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
  fn async_followed_by_other_flavors_errors() {
    assert_err("async async { 1 }");
    assert_err("async unsafe { 1 }");
    assert_err("async try { 1 }");
  }

  #[test]
  fn async_move_followed_by_other_flavors_errors() {
    assert_err("async move async { 1 }");
    assert_err("async move unsafe { 1 }");
    assert_err("async move try { 1 }");
  }

  #[test]
  fn unsafe_followed_by_other_flavors_errors() {
    assert_err("unsafe async { 1 }");
    assert_err("unsafe try { 1 }");
    assert_err("unsafe move { 1 }");
  }

  #[test]
  fn try_followed_by_other_flavors_errors() {
    assert_err("try async { 1 }");
    assert_err("try unsafe { 1 }");
    assert_err("try move { 1 }");
  }

  #[test]
  fn nested_async_blocks() {
    assert_ok("async { async { 1 } }");
  }

  // Try block expressions

  #[test]
  fn try_block_basic() {
    assert_ok("try { 1 }");
  }

  #[test]
  fn try_empty_block() {
    assert_ok("try {}");
  }

  #[test]
  fn try_block_with_attributes_on_stmt() {
    match parse_stmt_expr("#[attr] try { 1 }").unwrap() {
      ExprKind::Block { outer_attributes, .. } => {
        assert_eq!(outer_attributes.len(), 1);
      },
      other => panic!("expected try block expr, got: {:?}", other),
    }
  }

  #[test]
  fn try_move_not_allowed() {
    assert_err("try move { 1 }");
  }

  #[test]
  fn nested_try_blocks() {
    assert_ok("try { try { 1 } }");
  }

  // Mixed combinations
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

  // Error cases

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

  #[test]
  fn block_flavor_allows_non_default_context() {
    assert!(parse_flavor("{ }", ParserContext::Block).is_ok());
  }

  #[test]
  fn block_flavor_requires_block_after_flavor() {
    assert!(parse_flavor("async 1", ParserContext::Default).is_err());
  }

  #[test]
  fn block_flavor_requires_block_after_unsafe() {
    assert!(parse_flavor("unsafe 1", ParserContext::Default).is_err());
  }

  #[test]
  fn block_flavor_requires_block_after_try() {
    assert!(parse_flavor("try 1", ParserContext::Default).is_err());
  }

  #[test]
  fn block_flavor_rejects_async_followed_by_other_flavor() {
    assert!(parse_flavor("async unsafe { 1 }", ParserContext::Default).is_err());
  }

  #[test]
  fn block_flavor_rejects_unsafe_followed_by_other_flavor() {
    assert!(parse_flavor("unsafe async { 1 }", ParserContext::Default).is_err());
  }

  #[test]
  fn block_flavor_rejects_try_followed_by_other_flavor() {
    assert!(parse_flavor("try move { 1 }", ParserContext::Default).is_err());
  }

  #[test]
  fn block_flavor_rejects_move_only() {
    assert!(parse_flavor("move { 1 }", ParserContext::Default).is_err());
  }

  #[test]
  fn can_start_block_expression_reports_plain_block() {
    assert_eq!(can_start_block_expr("{ }").unwrap(), true);
  }

  #[test]
  fn can_start_block_expression_rejects_async_without_brace() {
    assert_eq!(can_start_block_expr("async 1").unwrap(), false);
  }
}
