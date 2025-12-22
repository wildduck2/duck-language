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
        assert_eq!(path.leading_colon, false);
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
  fn try_does_not_bind_tighter_than_await() {
    assert!(parse("foo?.await").is_err());
  }
}
