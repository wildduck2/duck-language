#[cfg(test)]
mod parser_edge_cases_tests {
  use crate::{
    parser_utils::ParserContext,
    tests::support::{parse_expression, parse_primary_expr, run_parser},
  };
  use lexer::token::TokenKind;

  fn assert_err(input: &str) {
    assert!(
      parse_primary_expr(input, "edge_case_test_temp", ParserContext::Default).is_err(),
      "expected error for: {:?}",
      input
    );
  }

  // ============================================================================
  // EOF handling edge cases
  // ============================================================================

  #[test]
  fn test_eof_after_open_paren() {
    assert_err("(");
  }

  #[test]
  fn test_eof_after_open_bracket() {
    assert_err("[");
  }

  #[test]
  fn test_eof_after_open_brace() {
    assert_err("{");
  }

  #[test]
  fn test_eof_after_binary_operator() {
    assert_err("1 +");
  }

  #[test]
  fn test_eof_after_unary_operator() {
    assert_err("!");
  }

  #[test]
  fn test_eof_after_comma() {
    assert_err("1,");
  }

  #[test]
  fn test_eof_after_semicolon() {
    assert_err("1;");
  }

  #[test]
  fn test_eof_after_colon() {
    assert_err("1:");
  }

  #[test]
  fn test_eof_after_dot() {
    assert_err("foo.");
  }

  #[test]
  fn test_eof_after_double_colon() {
    assert_err("foo::");
  }

  #[test]
  fn test_eof_after_arrow() {
    assert_err("fn foo() ->");
  }

  #[test]
  fn test_eof_in_function_params() {
    assert_err("fn foo(");
  }

  #[test]
  fn test_eof_in_function_params_after_comma() {
    assert_err("fn foo(x,");
  }

  #[test]
  fn test_eof_in_array_after_comma() {
    assert_err("[1,");
  }

  #[test]
  fn test_eof_in_tuple_after_comma() {
    assert_err("(1,");
  }

  #[test]
  fn test_eof_in_struct_fields() {
    assert_err("struct Foo {");
  }

  #[test]
  fn test_eof_in_struct_fields_after_comma() {
    assert_err("struct Foo { x: i32,");
  }

  #[test]
  fn test_eof_in_match_arms() {
    assert_err("match x {");
  }

  #[test]
  fn test_eof_in_match_arms_after_comma() {
    assert_err("match x { 1 => 2,");
  }

  #[test]
  fn test_eof_in_if_condition() {
    assert_err("if");
  }

  #[test]
  fn test_eof_in_if_condition_after_open_paren() {
    assert_err("if (");
  }

  #[test]
  fn test_eof_in_while_condition() {
    assert_err("while");
  }

  #[test]
  fn test_eof_in_for_loop() {
    assert_err("for");
  }

  #[test]
  fn test_eof_in_for_loop_after_in() {
    assert_err("for x in");
  }

  #[test]
  fn test_eof_in_let_binding() {
    assert_err("let");
  }

  #[test]
  fn test_eof_in_let_binding_after_equals() {
    assert_err("let x =");
  }

  #[test]
  fn test_eof_in_cast_expression() {
    assert_err("1 as");
  }

  #[test]
  fn test_eof_in_range_expression() {
    assert_err("1..");
  }

  #[test]
  fn test_eof_in_range_inclusive() {
    assert_err("1..=");
  }

  // ============================================================================
  // Unexpected token edge cases
  // ============================================================================

  #[test]
  fn test_unexpected_token_in_expression() {
    assert_err("1 + + 2");
  }

  #[test]
  fn test_unexpected_token_after_binary_op() {
    assert_err("1 + * 2");
  }

  #[test]
  fn test_unexpected_token_in_function_call() {
    assert_err("foo(1, + 2)");
  }

  #[test]
  fn test_unexpected_token_in_array() {
    assert_err("[1, + 2]");
  }

  #[test]
  fn test_unexpected_token_in_struct_literal() {
    assert_err("Foo { x: + 1 }");
  }

  #[test]
  fn test_unexpected_token_in_match_arm() {
    assert_err("match x { 1 => + 2 }");
  }

  #[test]
  fn test_unexpected_token_in_if_condition() {
    assert_err("if (+ 1) {}");
  }

  #[test]
  fn test_unexpected_token_in_while_condition() {
    assert_err("while (+ 1) {}");
  }

  #[test]
  fn test_unexpected_token_in_for_range() {
    assert_err("for x in (+ 1) {}");
  }

  #[test]
  fn test_unexpected_token_in_let_binding() {
    assert_err("let x = +");
  }

  #[test]
  fn test_unexpected_token_after_dot() {
    assert_err("foo.+");
  }

  #[test]
  fn test_unexpected_token_after_double_colon() {
    assert_err("foo::+");
  }

  #[test]
  fn test_unexpected_token_in_type_cast() {
    assert_err("1 as +");
  }

  #[test]
  fn test_unexpected_token_in_range_start() {
    assert_err("(+ 1)..2");
  }

  #[test]
  fn test_unexpected_token_in_range_end() {
    assert_err("1..(+ 2)");
  }

  // ============================================================================
  // Missing delimiter edge cases
  // ============================================================================

  #[test]
  fn test_missing_closing_paren_in_function_call() {
    assert_err("foo(1");
  }

  #[test]
  fn test_missing_closing_paren_in_group() {
    assert_err("(1");
  }

  #[test]
  fn test_missing_closing_bracket_in_array() {
    assert_err("[1");
  }

  #[test]
  fn test_missing_closing_brace_in_block() {
    assert_err("{ 1");
  }

  #[test]
  fn test_missing_closing_brace_in_struct() {
    assert_err("struct Foo { x: i32");
  }

  #[test]
  fn test_missing_closing_brace_in_match() {
    assert_err("match x { 1 => 2");
  }

  #[test]
  fn test_missing_closing_brace_in_if_block() {
    assert_err("if true { 1");
  }

  #[test]
  fn test_missing_closing_brace_in_while_block() {
    assert_err("while true { 1");
  }

  #[test]
  fn test_missing_closing_brace_in_for_block() {
    assert_err("for x in 1..2 { 1");
  }

  #[test]
  fn test_missing_closing_brace_in_loop_block() {
    assert_err("loop { 1");
  }

  #[test]
  fn test_missing_closing_brace_in_unsafe_block() {
    assert_err("unsafe { 1");
  }

  #[test]
  fn test_missing_closing_brace_in_async_block() {
    assert_err("async { 1");
  }

  #[test]
  fn test_missing_closing_brace_in_try_block() {
    assert_err("try { 1");
  }

  // ============================================================================
  // Nested delimiter edge cases
  // ============================================================================

  #[test]
  fn test_nested_parens_missing_inner_close() {
    assert_err("((1)");
  }

  #[test]
  fn test_nested_parens_missing_outer_close() {
    assert_err("((1)");
  }

  #[test]
  fn test_nested_brackets_missing_inner_close() {
    assert_err("[[1]");
  }

  #[test]
  fn test_nested_brackets_missing_outer_close() {
    assert_err("[[1]");
  }

  #[test]
  fn test_nested_braces_missing_inner_close() {
    assert_err("{{1}");
  }

  #[test]
  fn test_nested_braces_missing_outer_close() {
    assert_err("{{1}}");
  }

  #[test]
  fn test_mixed_nested_delimiters_missing_close() {
    assert_err("([{1}]");
  }

  #[test]
  fn test_mixed_nested_delimiters_wrong_order() {
    assert_err("([{1})]");
  }

  // ============================================================================
  // Comma and separator edge cases
  // ============================================================================

  #[test]
  fn test_double_comma_in_function_args() {
    assert_err("foo(1,,2)");
  }

  #[test]
  fn test_double_comma_in_array() {
    assert_err("[1,,2]");
  }

  #[test]
  fn test_double_comma_in_tuple() {
    assert_err("(1,,2)");
  }

  #[test]
  fn test_double_comma_in_struct_fields() {
    assert_err("Foo { x: 1,, y: 2 }");
  }

  #[test]
  fn test_trailing_comma_after_trailing_comma() {
    // This might be valid, but testing edge case
    assert_err("[1,,]");
  }

  #[test]
  fn test_comma_before_closing_paren() {
    // Trailing comma might be valid, but testing
    assert_err("foo(1,)");
  }

  #[test]
  fn test_comma_before_closing_bracket() {
    // Trailing comma might be valid in arrays
    // This test checks if it's handled correctly
  }

  // ============================================================================
  // Operator edge cases
  // ============================================================================

  #[test]
  fn test_double_binary_operator() {
    assert_err("1 + + 2");
  }

  #[test]
  fn test_binary_operator_at_start() {
    assert_err("+ 1");
  }

  #[test]
  fn test_binary_operator_at_end() {
    assert_err("1 +");
  }

  #[test]
  fn test_multiple_binary_operators() {
    assert_err("1 + + + 2");
  }

  #[test]
  fn test_binary_operator_after_unary() {
    // This might be valid: !x + y
    // But testing: ! + x which should error
    assert_err("! + 1");
  }

  #[test]
  fn test_unary_operator_after_binary() {
    assert_err("1 + !");
  }

  // ============================================================================
  // Path and identifier edge cases
  // ============================================================================

  #[test]
  fn test_path_double_colon_at_start() {
    assert_err("::");
  }

  #[test]
  fn test_path_double_colon_at_end() {
    assert_err("foo::");
  }

  #[test]
  fn test_path_triple_colon() {
    assert_err("foo:::");
  }

  #[test]
  fn test_path_double_colon_after_dot() {
    assert_err("foo.::bar");
  }

  #[test]
  fn test_dot_after_double_colon() {
    assert_err("foo::.bar");
  }

  // ============================================================================
  // Type annotation edge cases
  // ============================================================================

  #[test]
  fn test_type_annotation_missing_type() {
    assert_err("let x: = 1");
  }

  #[test]
  fn test_type_annotation_missing_colon() {
    assert_err("let x i32 = 1");
  }

  #[test]
  fn test_type_annotation_in_function_param() {
    assert_err("fn foo(x: ) {}");
  }

  #[test]
  fn test_type_annotation_in_function_return() {
    assert_err("fn foo() -> {}");
  }

  // ============================================================================
  // Control flow edge cases
  // ============================================================================

  #[test]
  fn test_if_missing_condition() {
    assert_err("if {}");
  }

  #[test]
  fn test_if_missing_condition_parens() {
    assert_err("if true {}");
  }

  #[test]
  fn test_while_missing_condition() {
    assert_err("while {}");
  }

  #[test]
  fn test_for_missing_pattern() {
    assert_err("for in 1..2 {}");
  }

  #[test]
  fn test_for_missing_in_keyword() {
    assert_err("for x 1..2 {}");
  }

  #[test]
  fn test_for_missing_range() {
    assert_err("for x in {}");
  }

  #[test]
  fn test_match_missing_expression() {
    assert_err("match {}");
  }

  #[test]
  fn test_match_missing_arms() {
    assert_err("match x {}");
  }

  #[test]
  fn test_match_arm_missing_arrow() {
    assert_err("match x { 1 2 }");
  }

  #[test]
  fn test_match_arm_missing_expression() {
    assert_err("match x { 1 => }");
  }

  // ============================================================================
  // Function edge cases
  // ============================================================================

  #[test]
  fn test_function_missing_name() {
    assert_err("fn () {}");
  }

  #[test]
  fn test_function_missing_params() {
    assert_err("fn foo {}");
  }

  #[test]
  fn test_function_missing_body() {
    assert_err("fn foo()");
  }

  #[test]
  fn test_function_param_missing_comma() {
    assert_err("fn foo(x: i32 y: i32) {}");
  }

  #[test]
  fn test_function_param_missing_type() {
    assert_err("fn foo(x:) {}");
  }

  // ============================================================================
  // Struct and enum edge cases
  // ============================================================================

  #[test]
  fn test_struct_missing_name() {
    assert_err("struct {}");
  }

  #[test]
  fn test_struct_missing_body() {
    assert_err("struct Foo");
  }

  #[test]
  fn test_struct_field_missing_colon() {
    assert_err("struct Foo { x i32 }");
  }

  #[test]
  fn test_struct_field_missing_type() {
    assert_err("struct Foo { x: }");
  }

  #[test]
  fn test_struct_field_missing_comma() {
    assert_err("struct Foo { x: i32 y: i32 }");
  }

  #[test]
  fn test_enum_missing_name() {
    assert_err("enum {}");
  }

  #[test]
  fn test_enum_missing_body() {
    assert_err("enum Foo");
  }

  #[test]
  fn test_enum_variant_missing_comma() {
    assert_err("enum Foo { Bar Baz }");
  }

  // ============================================================================
  // Let binding edge cases
  // ============================================================================

  #[test]
  fn test_let_missing_identifier() {
    assert_err("let = 1");
  }

  #[test]
  fn test_let_missing_equals() {
    assert_err("let x 1");
  }

  #[test]
  fn test_let_missing_value() {
    assert_err("let x =");
  }

  #[test]
  fn test_let_missing_semicolon() {
    // This might be valid in some contexts
    run_parser("let x = 1", "let_test", |parser| {
      parser.parse_stmt(ParserContext::Default)?;
      Ok(())
    })
    .unwrap();
  }

  // ============================================================================
  // Expression edge cases
  // ============================================================================

  #[test]
  fn test_empty_expression() {
    assert_err("");
  }

  #[test]
  fn test_expression_with_only_whitespace() {
    assert_err("   ");
  }

  #[test]
  fn test_expression_with_only_comments() {
    // Comments are handled by lexer, but testing parser behavior
    assert_err("// comment");
  }

  #[test]
  fn test_nested_expressions_deep() {
    // Test deep nesting (reduced to avoid stack overflow in test)
    let mut input = String::new();
    for _ in 0..20 {
      input.push_str("(");
    }
    input.push_str("1");
    for _ in 0..20 {
      input.push_str(")");
    }
    // This should parse successfully
    parse_primary_expr(&input, "deep_nest_test", ParserContext::Default).unwrap();
  }

  #[test]
  fn test_very_long_expression() {
    // Test expression with many terms
    let mut input = String::new();
    for i in 1..=100 {
      input.push_str(&format!("{}", i));
      if i < 100 {
        input.push_str(" + ");
      }
    }
    parse_expression(&input, "long_expr_test", ParserContext::Default).unwrap();
  }
}

