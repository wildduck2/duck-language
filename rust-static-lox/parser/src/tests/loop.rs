#[cfg(test)]
mod loop_tests {

  use crate::{
    ast::{expr::ExprKind, Stmt},
    parser_utils::ParserContext,
    tests::support::{parse_expression, run_parser},
  };

  fn parse_single(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "loop_expr_test_temp", ParserContext::LoopCondition)
  }

  fn assert_ok(input: &str) {
    assert!(parse_single(input).is_ok(), "expected ok for {input:?}");
  }

  fn assert_err(input: &str) {
    assert!(parse_single(input).is_err(), "expected error for {input:?}");
  }

  fn parse_stmt_expr(input: &str) -> Result<ExprKind, ()> {
    run_parser(input, "loop_stmt_test_temp", |parser| {
      match parser.parse_stmt(ParserContext::Default)? {
        Stmt::Expr { expr, .. } => Ok(expr.kind),
        other => panic!("expected expr stmt, got: {:?}", other),
      }
    })
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
  fn loop_outer_attributes_on_stmt() {
    match parse_stmt_expr("#[attr] loop { }").unwrap() {
      ExprKind::Loop { .. } => {},
      other => panic!("expected loop expr, got: {:?}", other),
    }
    match parse_stmt_expr("#[first] #[second] loop { }").unwrap() {
      ExprKind::Loop { .. } => {},
      other => panic!("expected loop expr, got: {:?}", other),
    }
  }

  #[test]
  fn while_outer_attributes_on_stmt() {
    match parse_stmt_expr("#[attr] while true { }").unwrap() {
      ExprKind::While { .. } => {},
      other => panic!("expected while expr, got: {:?}", other),
    }
  }

  #[test]
  fn for_outer_attributes_on_stmt() {
    match parse_stmt_expr("#[attr] for x in y { }").unwrap() {
      ExprKind::For { .. } => {},
      other => panic!("expected for expr, got: {:?}", other),
    }
    match parse_stmt_expr("#[first] #[second] for x in y { }").unwrap() {
      ExprKind::For { .. } => {},
      other => panic!("expected for expr, got: {:?}", other),
    }
  }

  #[test]
  fn parses_for_loops() {
    assert_ok("for x in y { }");
    assert_ok("'label: for x in y { }");
  }

  #[test]
  fn while_comparison_condition() {
    assert_ok("while x < 10 { x = x + 1; }");
  }

  #[test]
  fn while_identifier_condition() {
    assert_ok("while ready { }");
  }

  #[test]
  fn while_unary_condition() {
    assert_ok("while !done { done; }");
  }

  #[test]
  fn parses_while_let() {
    assert_ok("while let x = y { }");
    assert_ok("'label: while let x = y { }");
  }

  #[test]
  fn while_let_error_cases() {
    assert_err("while let < = y { }");
    assert_err("while let x y { }");
    assert_err("while let x = ; { }");
    assert_err("while let x = y");
  }

  #[test]
  fn for_error_cases() {
    assert_err("for < = y { }");
    assert_err("for x y { }");
    assert_err("for x in ; { }");
    assert_err("for x in y");
  }

  // Error cases for supported constructs

  #[test]
  fn loop_missing_block_errors() {
    assert_err("loop");
  }

  #[test]
  fn while_missing_condition_errors() {
    assert_err("while { }");
    assert_err("while , { }");
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
  fn while_with_break_and_continue() {
    assert_ok("while true { break; }");
    assert_ok("while true { continue; }");
  }

  #[test]
  fn for_with_break_and_continue() {
    assert_ok("for x in y { break; }");
    assert_ok("for x in y { continue; }");
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
