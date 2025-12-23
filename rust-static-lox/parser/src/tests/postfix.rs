#[cfg(test)]
mod postfix_tests {

  use crate::{
    ast::{
      expr::{ExprKind, FieldAccess},
      path::{PathSegment, PathSegmentKind},
      Lit,
    },
    parser_utils::ExprContext,
    tests::support::parse_expression,
  };

  fn parse(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "postfix_expr_test_temp", ExprContext::Default)
  }

  fn assert_ident_expr(expr: &ExprKind, expected: &str) {
    match expr {
      ExprKind::Path { qself, path } => {
        assert!(qself.is_none());
        assert!(!path.leading_colon);
        assert_eq!(path.segments.len(), 1);
        assert_eq!(
          path.segments[0],
          PathSegment::new(PathSegmentKind::Ident(expected.to_string()), None)
        );
      },
      ExprKind::Group { expr } => assert_ident_expr(&expr.kind, expected),
      other => panic!("expected path `{expected}`, got: {:?}", other),
    }
  }

  fn assert_int(expr: &ExprKind, expected: i128) {
    match expr {
      ExprKind::Literal(Lit::Integer { value, .. }) => assert_eq!(*value, expected),
      other => panic!("expected integer literal {expected}, got: {:?}", other),
    }
  }

  #[test]
  fn parses_function_call_without_args() {
    let expr = parse("(foo)()").unwrap();
    match expr {
      ExprKind::Call { callee, args } => {
        assert_ident_expr(&callee.kind, "foo");
        assert!(args.is_empty());
      },
      other => panic!("expected call expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_function_call_with_args_and_trailing_comma() {
    let expr = parse("(foo)(1, 2,)").unwrap();
    match expr {
      ExprKind::Call { callee, args } => {
        assert_ident_expr(&callee.kind, "foo");
        assert_eq!(args.len(), 2);
        assert_int(&args[0].kind, 1);
        assert_int(&args[1].kind, 2);
      },
      other => panic!("expected call expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_method_call() {
    let expr = parse("foo.bar(1)").unwrap();
    match expr {
      ExprKind::MethodCall {
        receiver,
        method,
        turbofish,
        args,
      } => {
        assert_ident_expr(&receiver.kind, "foo");
        assert_eq!(method, "bar");
        assert!(turbofish.is_none());
        assert_eq!(args.len(), 1);
        assert_int(&args[0].kind, 1);
      },
      other => panic!("expected method call, got: {:?}", other),
    }
  }

  #[test]
  fn parses_named_field_access() {
    let expr = parse("foo.bar").unwrap();
    match expr {
      ExprKind::Field { object, field } => {
        assert_ident_expr(&object.kind, "foo");
        assert_eq!(field, FieldAccess::Named("bar".to_string()));
      },
      other => panic!("expected field access, got: {:?}", other),
    }
  }

  #[test]
  fn parses_tuple_index_field_access() {
    let expr = parse("foo.0").unwrap();
    match expr {
      ExprKind::Field { object, field } => {
        assert_ident_expr(&object.kind, "foo");
        assert_eq!(field, FieldAccess::Unnamed(0));
      },
      other => panic!("expected tuple index field access, got: {:?}", other),
    }
  }

  #[test]
  fn parses_chained_tuple_index_access() {
    let expr = parse("foo.0.1").unwrap();
    match expr {
      ExprKind::Field { object, field } => {
        assert_eq!(field, FieldAccess::Unnamed(1));
        match object.kind {
          ExprKind::Field { object, field } => {
            assert_eq!(field, FieldAccess::Unnamed(0));
            assert_ident_expr(&object.kind, "foo");
          },
          other => panic!("expected first tuple index access, got: {:?}", other),
        }
      },
      other => panic!("expected tuple index field access, got: {:?}", other),
    }
  }

  #[test]
  fn parses_index_expression() {
    let expr = parse("(foo)[bar]").unwrap();
    match expr {
      ExprKind::Index { object, index } => {
        assert_ident_expr(&object.kind, "foo");
        assert_ident_expr(&index.kind, "bar");
      },
      other => panic!("expected index expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_await_expression() {
    let expr = parse("foo.await").unwrap();
    match expr {
      ExprKind::Await { expr } => assert_ident_expr(&expr.kind, "foo"),
      other => panic!("expected await expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_try_expression() {
    let expr = parse("(foo)?").unwrap();
    match expr {
      ExprKind::Try { expr } => assert_ident_expr(&expr.kind, "foo"),
      other => panic!("expected try expression, got: {:?}", other),
    }
  }

  #[test]
  fn chains_postfix_operations() {
    let expr = parse("(foo)().bar[0]?").unwrap();
    match expr {
      ExprKind::Try { expr } => match expr.kind {
        ExprKind::Index { object, index } => {
          assert_int(&index.kind, 0);
          match object.kind {
            ExprKind::Field { object, field } => {
              assert_eq!(field, FieldAccess::Named("bar".to_string()));
              match object.kind {
                ExprKind::Call { callee, args } => {
                  assert_ident_expr(&callee.kind, "foo");
                  assert!(args.is_empty());
                },
                other => panic!("expected call as receiver, got: {:?}", other),
              }
            },
            other => panic!("expected field access before index, got: {:?}", other),
          }
        },
        other => panic!("expected index expression before try, got: {:?}", other),
      },
      other => panic!("expected try expression, got: {:?}", other),
    }
  }

  #[test]
  fn errors_on_invalid_token_after_dot() {
    assert!(parse("foo.?").is_err());
  }

  #[test]
  fn try_does_not_bind_tighter_than_call() {
    assert!(parse("foo?()").is_err());
  }

  #[test]
  fn try_can_be_followed_by_await() {
    let expr = parse("foo?.await").unwrap();
    match expr {
      ExprKind::Await { expr } => match expr.kind {
        ExprKind::Try { expr } => assert_ident_expr(&expr.kind, "foo"),
        other => panic!("expected try before await, got: {:?}", other),
      },
      other => panic!("expected await expression, got: {:?}", other),
    }
  }

  #[test]
  fn chains_multiple_method_calls() {
    let expr = parse("foo.bar().baz(1)").unwrap();
    match expr {
      ExprKind::MethodCall {
        receiver,
        method,
        args,
        ..
      } => {
        assert_eq!(method, "baz");
        assert_eq!(args.len(), 1);
        match receiver.kind {
          ExprKind::MethodCall { method: ref m, .. } => assert_eq!(m, "bar"),
          other => panic!("expected chained method call, got: {:?}", other),
        }
      },
      other => panic!("expected method call, got: {:?}", other),
    }
  }

  #[test]
  fn parses_call_then_await() {
    let expr = parse("foo().await").unwrap();
    match expr {
      ExprKind::Await { expr } => match expr.kind {
        ExprKind::Call { callee, .. } => assert_ident_expr(&callee.kind, "foo"),
        other => panic!("expected call before await, got: {:?}", other),
      },
      other => panic!("expected await expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_await_then_call() {
    let expr = parse("(foo.await)()").unwrap();
    match expr {
      ExprKind::Call { callee, .. } => match callee.kind {
        ExprKind::Group { expr } => match expr.kind {
          ExprKind::Await { expr } => assert_ident_expr(&expr.kind, "foo"),
          other => panic!("expected await inside group, got: {:?}", other),
        },
        other => panic!("expected grouped await as callee, got: {:?}", other),
      },
      other => panic!("expected call expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_method_call_with_turbofish() {
    let expr = parse("foo.bar::<i32>(1)").unwrap();
    match expr {
      ExprKind::MethodCall { .. } => {},
      other => panic!("expected method call with turbofish, got: {:?}", other),
    }
  }

  #[test]
  fn parses_index_then_call() {
    let expr = parse("((foo)[0])(1)").unwrap();
    match expr {
      ExprKind::Call { callee, args } => {
        assert_eq!(args.len(), 1);
        match callee.kind {
          ExprKind::Group { expr } => match expr.kind {
            ExprKind::Index { object, index } => {
              assert_ident_expr(&object.kind, "foo");
              assert_int(&index.kind, 0);
            },
            other => panic!("expected index inside group, got: {:?}", other),
          },
          other => panic!("expected grouped index as callee, got: {:?}", other),
        }
      },
      other => panic!("expected call expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_repeated_try_chain() {
    let expr = parse("(foo?)?").unwrap();
    match expr {
      ExprKind::Try { expr } => match expr.kind {
        ExprKind::Group { expr } => match expr.kind {
          ExprKind::Try { expr } => assert_ident_expr(&expr.kind, "foo"),
          other => panic!("expected inner try, got: {:?}", other),
        },
        other => panic!("expected grouped try, got: {:?}", other),
      },
      other => panic!("expected try expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_try_after_postfix_chain() {
    let expr = parse("(foo().bar())?").unwrap();
    match expr {
      ExprKind::Try { expr } => match expr.kind {
        ExprKind::Group { expr } => match expr.kind {
          ExprKind::MethodCall { receiver, .. } => match receiver.kind {
            ExprKind::Call { .. } => {},
            other => panic!("expected call receiver, got: {:?}", other),
          },
          other => panic!("expected method call inside group, got: {:?}", other),
        },
        other => panic!("expected grouped expression before try, got: {:?}", other),
      },
      other => panic!("expected try expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro_invocation_in_postfix_chain() {
    let expr = parse("foo!().bar").unwrap();
    match expr {
      ExprKind::Field { object, .. } => match object.kind {
        ExprKind::Macro { .. } => {},
        other => panic!("expected macro invocation, got: {:?}", other),
      },
      other => panic!("expected field access, got: {:?}", other),
    }
  }

  #[test]
  fn parses_postfix_after_macro_invocation() {
    let expr = parse("foo.bar!()[0]").unwrap();
    match expr {
      ExprKind::Index { object, index } => {
        assert_int(&index.kind, 0);
        match object.kind {
          ExprKind::Macro { .. } => {},
          other => panic!("expected macro invocation, got: {:?}", other),
        }
      },
      other => panic!("expected index expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro_then_await() {
    let expr = parse("foo!().await").unwrap();
    match expr {
      ExprKind::Await { expr } => match expr.kind {
        ExprKind::Macro { .. } => {},
        other => panic!("expected macro before await, got: {:?}", other),
      },
      other => panic!("expected await expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_grouped_macro_then_try() {
    let expr = parse("(foo!())?").unwrap();
    match expr {
      ExprKind::Try { expr } => match expr.kind {
        ExprKind::Group { expr } => match expr.kind {
          ExprKind::Macro { .. } => {},
          other => panic!("expected macro inside group, got: {:?}", other),
        },
        other => panic!("expected grouped macro, got: {:?}", other),
      },
      other => panic!("expected try expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_qualified_macro_index() {
    let expr = parse("crate::foo!()[0]").unwrap();
    match expr {
      ExprKind::Index { object, index } => {
        assert_int(&index.kind, 0);
        match object.kind {
          ExprKind::Macro { .. } => {},
          other => panic!("expected macro as index object, got: {:?}", other),
        }
      },
      other => panic!("expected index expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro_with_args_then_field() {
    let expr = parse("foo!(1).bar").unwrap();
    match expr {
      ExprKind::Field { object, .. } => match object.kind {
        ExprKind::Macro { .. } => {},
        other => panic!("expected macro before field, got: {:?}", other),
      },
      other => panic!("expected field access, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro_then_method_then_index() {
    let expr = parse("foo!().bar()[0]").unwrap();
    match expr {
      ExprKind::Index { object, .. } => match object.kind {
        ExprKind::MethodCall { .. } => {},
        other => panic!("expected method call before index, got: {:?}", other),
      },
      other => panic!("expected index expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_grouped_try_then_field() {
    let expr = parse("((foo.bar)?).baz").unwrap();
    match expr {
      ExprKind::Field { object, field } => {
        assert_eq!(field, FieldAccess::Named("baz".to_string()));
        match object.kind {
          ExprKind::Group { expr } => match expr.kind {
            ExprKind::Try { .. } => {},
            other => panic!("expected try inside group, got: {:?}", other),
          },
          other => panic!("expected grouped try, got: {:?}", other),
        }
      },
      other => panic!("expected field access, got: {:?}", other),
    }
  }

  #[test]
  fn parses_index_then_await() {
    let expr = parse("foo[0].await").unwrap();
    match expr {
      ExprKind::Await { expr } => match expr.kind {
        ExprKind::Index { .. } => {},
        other => panic!("expected index before await, got: {:?}", other),
      },
      other => panic!("expected await expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_tuple_index_chain() {
    let expr = parse("foo.0.1").unwrap();
    match expr {
      ExprKind::Field { object, field } => {
        assert_eq!(field, FieldAccess::Unnamed(1));
        match object.kind {
          ExprKind::Field {
            field: FieldAccess::Unnamed(0),
            ..
          } => {},
          other => panic!("expected first tuple index, got: {:?}", other),
        }
      },
      other => panic!("expected tuple index chain, got: {:?}", other),
    }
  }

  #[test]
  fn errors_on_invalid_postfix_sequence() {
    assert!(parse("foo.!()").is_err());
    assert!(parse("foo.await()").is_err());
    assert!(parse("foo?.bar").is_err());
  }

  #[test]
  fn parses_field_then_method_call() {
    let expr = parse("foo.bar.baz(1)").unwrap();
    match expr {
      ExprKind::MethodCall {
        receiver,
        method,
        args,
        ..
      } => {
        assert_eq!(method, "baz");
        assert_eq!(args.len(), 1);
        match receiver.kind {
          ExprKind::Field {
            field: FieldAccess::Named(ref name),
            ..
          } => {
            assert_eq!(name, "bar");
          },
          other => panic!("expected field access as receiver, got: {:?}", other),
        }
      },
      other => panic!("expected method call, got: {:?}", other),
    }
  }

  #[test]
  fn parses_method_call_then_field_access() {
    let expr = parse("foo.bar().baz").unwrap();
    match expr {
      ExprKind::Field { object, field } => {
        assert_eq!(field, FieldAccess::Named("baz".to_string()));
        match object.kind {
          ExprKind::MethodCall { method, .. } => assert_eq!(method, "bar"),
          other => panic!("expected method call before field, got: {:?}", other),
        }
      },
      other => panic!("expected field access, got: {:?}", other),
    }
  }

  #[test]
  fn parses_double_index_chain() {
    let expr = parse("foo[0][1]").unwrap();
    match expr {
      ExprKind::Index { object, index } => {
        assert_int(&index.kind, 1);
        match object.kind {
          ExprKind::Index { index, .. } => assert_int(&index.kind, 0),
          other => panic!("expected first index, got: {:?}", other),
        }
      },
      other => panic!("expected index chain, got: {:?}", other),
    }
  }

  #[test]
  fn parses_index_then_method_call() {
    let expr = parse("foo[0].bar()").unwrap();
    match expr {
      ExprKind::MethodCall {
        receiver, method, ..
      } => {
        assert_eq!(method, "bar");
        match receiver.kind {
          ExprKind::Index { .. } => {},
          other => panic!("expected index as receiver, got: {:?}", other),
        }
      },
      other => panic!("expected method call, got: {:?}", other),
    }
  }

  #[test]
  fn parses_call_then_index() {
    let expr = parse("foo()[0]").unwrap();
    match expr {
      ExprKind::Index { object, .. } => match object.kind {
        ExprKind::Call { .. } => {},
        other => panic!("expected call before index, got: {:?}", other),
      },
      other => panic!("expected index expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_nested_grouped_postfix_chain() {
    let expr = parse("(((foo))).bar").unwrap();
    match expr {
      ExprKind::Field { object, field } => {
        assert_eq!(field, FieldAccess::Named("bar".to_string()));
        assert_ident_expr(&object.kind, "foo");
      },
      other => panic!("expected field access, got: {:?}", other),
    }
  }

  #[test]
  fn parses_grouped_call_then_method() {
    let expr = parse("(foo()) .bar()").unwrap();
    match expr {
      ExprKind::MethodCall {
        receiver, method, ..
      } => {
        assert_eq!(method, "bar");
        match receiver.kind {
          ExprKind::Group { expr } => match expr.kind {
            ExprKind::Call { .. } => {},
            other => panic!("expected call inside group, got: {:?}", other),
          },
          other => panic!("expected grouped call, got: {:?}", other),
        }
      },
      other => panic!("expected method call, got: {:?}", other),
    }
  }

  #[test]
  fn parses_multiple_try_chain_without_grouping() {
    let expr = parse("foo???").unwrap();
    match expr {
      ExprKind::Try { expr } => match expr.kind {
        ExprKind::Try { expr } => match expr.kind {
          ExprKind::Try { expr } => assert_ident_expr(&expr.kind, "foo"),
          other => panic!("expected innermost try, got: {:?}", other),
        },
        other => panic!("expected nested try, got: {:?}", other),
      },
      other => panic!("expected try expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_method_call_then_try() {
    let expr = parse("(foo.bar())?").unwrap();
    match expr {
      ExprKind::Try { expr } => match expr.kind {
        ExprKind::Group { expr } => match expr.kind {
          ExprKind::MethodCall { .. } => {},
          other => panic!("expected method call inside group, got: {:?}", other),
        },
        other => panic!("expected grouped expression, got: {:?}", other),
      },
      other => panic!("expected try expression, got: {:?}", other),
    }
  }

  #[test]
  fn errors_on_postfix_after_try_without_grouping() {
    assert!(parse("foo?.bar()").is_err());
    assert!(parse("foo?[0]").is_err());
  }

  #[test]
  fn allows_await_after_try_without_grouping() {
    let expr = parse("foo?.await").unwrap();
    match expr {
      ExprKind::Await { expr } => match expr.kind {
        ExprKind::Try { expr } => assert_ident_expr(&expr.kind, "foo"),
        other => panic!("expected try before await, got: {:?}", other),
      },
      other => panic!("expected await expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_method_call_with_multiple_generic_args() {
    let expr = parse("foo.bar::<i32, u64>(1)").unwrap();
    match expr {
      ExprKind::MethodCall { turbofish, .. } => {
        assert!(turbofish.is_some());
      },
      other => panic!(
        "expected method call with multiple generics, got: {:?}",
        other
      ),
    }
  }

  #[test]
  fn parses_chained_method_calls_with_turbofish_on_second() {
    let expr = parse("foo.bar().baz::<i32>(1)").unwrap();
    match expr {
      ExprKind::MethodCall { method, .. } => assert_eq!(method, "baz"),
      other => panic!("expected method call, got: {:?}", other),
    }
  }

  #[test]
  fn parses_index_then_field_then_call() {
    let expr = parse("foo[0].bar.baz()").unwrap();
    match expr {
      ExprKind::MethodCall { receiver, .. } => match receiver.kind {
        ExprKind::Field { .. } => {},
        other => panic!("expected field as receiver, got: {:?}", other),
      },
      other => panic!("expected method call, got: {:?}", other),
    }
  }

  #[test]
  fn parses_deep_postfix_chain_mixed() {
    let expr = parse("foo.bar()[0].baz::<u8>()?.await").unwrap();
    match expr {
      ExprKind::Await { expr } => match expr.kind {
        ExprKind::Try { .. } => {},
        other => panic!("expected try before await, got: {:?}", other),
      },
      other => panic!("expected await expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_postfix_on_path_expression() {
    let expr = parse("crate::foo.bar()").unwrap();
    match expr {
      ExprKind::MethodCall { receiver, .. } => match receiver.kind {
        ExprKind::Path { .. } => {},
        other => panic!("expected path as receiver, got: {:?}", other),
      },
      other => panic!("expected method call, got: {:?}", other),
    }
  }

  #[test]
  fn parses_qualified_path_then_index_then_call() {
    let expr = parse("crate::foo::bar[0]()(1)").unwrap();
    match expr {
      ExprKind::Call { callee, .. } => match callee.kind {
        ExprKind::Call { .. } => {},
        other => panic!("expected call as callee, got: {:?}", other),
      },
      other => panic!("expected call expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro_path_then_method_call() {
    let expr = parse("crate::foo!().bar()").unwrap();
    match expr {
      ExprKind::MethodCall { receiver, .. } => match receiver.kind {
        ExprKind::Macro { .. } => {},
        other => panic!("expected macro as receiver, got: {:?}", other),
      },
      other => panic!("expected method call, got: {:?}", other),
    }
  }

  #[test]
  fn errors_on_turbofish_without_method() {
    assert!(parse("foo::<i32>()").is_err());
  }

  #[test]
  fn errors_on_postfix_starting_with_dot() {
    assert!(parse(".foo").is_err());
  }

  #[test]
  fn errors_on_multiple_awaits() {
    assert!(parse("foo.await.await").is_err());
  }
}
