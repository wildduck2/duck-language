#[cfg(test)]
mod unary_tests {

  use crate::{
    ast::{
      expr::{ExprKind, UnaryOp},
      path::PathSegmentKind,
      Lit, Mutability,
    },
    parser_utils::ParserContext,
    tests::support::parse_expression,
  };

  fn parse(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "unary_expr_test_temp", ParserContext::Default)
  }

  fn assert_int(expr: &ExprKind, expected: i128) {
    match expr {
      ExprKind::Literal(Lit::Integer { value, .. }) => {
        assert_eq!(*value, expected);
      },

      ExprKind::Group { expr } => match &expr.kind {
        ExprKind::Literal(Lit::Integer { value, .. }) => {
          assert_eq!(*value, expected);
        },
        other => {
          panic!(
            "expected integer literal {expected} inside group, got: {:?}",
            other
          );
        },
      },

      other => {
        panic!("expected integer literal {expected}, got: {:?}", other);
      },
    }
  }

  fn assert_bool(expr: &ExprKind, expected: bool) {
    match expr {
      ExprKind::Literal(Lit::Bool(value)) => assert_eq!(*value, expected),
      other => panic!("expected bool literal {expected}, got: {:?}", other),
    }
  }

  fn assert_path(expr: &ExprKind, expected: &str) {
    match expr {
      ExprKind::Path { path, .. } => {
        assert_eq!(path.segments.len(), 1);
        match &path.segments[0].kind {
          PathSegmentKind::Ident(name) => assert_eq!(name, expected),
          other => panic!("expected ident path, got: {:?}", other),
        }
      },
      ExprKind::Group { expr } => assert_path(&expr.kind, expected),
      other => panic!("expected path `{expected}`, got: {:?}", other),
    }
  }

  #[test]
  fn parses_negation() {
    let expr = parse("-1").unwrap();
    match expr {
      ExprKind::Unary { op, expr } => {
        assert_eq!(op, UnaryOp::Neg);
        assert_int(&expr.kind, 1);
      },
      other => panic!("expected unary negation, got: {:?}", other),
    }
  }

  #[test]
  fn parses_logical_not() {
    let expr = parse("!true").unwrap();
    match expr {
      ExprKind::Unary { op, expr } => {
        assert_eq!(op, UnaryOp::Not);
        assert_bool(&expr.kind, true);
      },
      other => panic!("expected unary not, got: {:?}", other),
    }
  }

  #[test]
  fn parses_dereference() {
    let expr = parse("*x").unwrap();
    match expr {
      ExprKind::Unary { op, expr } => {
        assert_eq!(op, UnaryOp::Deref);
        assert_path(&expr.kind, "x");
      },
      other => panic!("expected deref expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_reference_chains_with_depth() {
    let expr = parse("&&x").unwrap();
    match expr {
      ExprKind::Unary { op, expr } => {
        assert_eq!(
          op,
          UnaryOp::Ref {
            mutability: Mutability::Immutable,
            depth: 2,
          }
        );
        assert_path(&expr.kind, "x");
      },
      other => panic!("expected reference expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_mutable_reference_chains() {
    let expr = parse("&&mut x").unwrap();
    match expr {
      ExprKind::Unary { op, expr } => {
        assert_eq!(
          op,
          UnaryOp::Ref {
            mutability: Mutability::Mutable,
            depth: 2,
          }
        );
        assert_path(&expr.kind, "x");
      },
      other => panic!("expected mutable reference expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_const_reference() {
    let expr = parse("&const x").unwrap();
    match expr {
      ExprKind::Unary { op, expr } => {
        assert_eq!(
          op,
          UnaryOp::Ref {
            mutability: Mutability::Immutable,
            depth: 1,
          }
        );
        assert_path(&expr.kind, "x");
      },
      other => panic!("expected const reference expression, got: {:?}", other),
    }
  }

  #[test]
  fn falls_back_to_postfix_parsing() {
    let expr = parse("(foo)()").unwrap();
    match expr {
      ExprKind::Call { callee, args } => {
        assert_path(&callee.kind, "foo");
        assert!(args.is_empty());
      },
      other => panic!("expected call expression via postfix, got: {:?}", other),
    }
  }

  #[test]
  fn parses_unary_on_grouped_expression() {
    let expr = parse("-(1)").unwrap();
    match expr {
      ExprKind::Unary { op, expr } => {
        assert_eq!(op, UnaryOp::Neg);
        assert_int(&expr.kind, 1);
      },
      other => panic!("expected unary negation on group, got: {:?}", other),
    }
  }

  #[test]
  fn parses_logical_not_on_path() {
    let expr = parse("!x").unwrap();
    match expr {
      ExprKind::Unary { op, expr } => {
        assert_eq!(op, UnaryOp::Not);
        assert_path(&expr.kind, "x");
      },
      other => panic!("expected unary not on path, got: {:?}", other),
    }
  }

  #[test]
  fn parses_multiple_dereference_chain() {
    let expr = parse("***x").unwrap();
    match expr {
      ExprKind::Unary { op, expr } => {
        assert_eq!(op, UnaryOp::Deref);
        match expr.kind {
          ExprKind::Unary {
            op: UnaryOp::Deref,
            expr,
          } => match expr.kind {
            ExprKind::Unary {
              op: UnaryOp::Deref,
              expr,
            } => {
              assert_path(&expr.kind, "x");
            },
            other => panic!("expected third deref, got: {:?}", other),
          },
          other => panic!("expected second deref, got: {:?}", other),
        }
      },
      other => panic!("expected deref chain, got: {:?}", other),
    }
  }

  #[test]
  fn parses_mixed_unary_chain() {
    let expr = parse("!&x").unwrap();
    match expr {
      ExprKind::Unary {
        op: UnaryOp::Not,
        expr,
      } => match expr.kind {
        ExprKind::Unary {
          op:
            UnaryOp::Ref {
              mutability: Mutability::Immutable,
              depth: 1,
            },
          expr,
        } => {
          assert_path(&expr.kind, "x");
        },
        other => panic!("expected reference under not, got: {:?}", other),
      },
      other => panic!("expected unary not, got: {:?}", other),
    }
  }

  #[test]
  fn parses_negation_of_logical_not() {
    let expr = parse("-!true").unwrap();
    match expr {
      ExprKind::Unary {
        op: UnaryOp::Neg,
        expr,
      } => match expr.kind {
        ExprKind::Unary {
          op: UnaryOp::Not,
          expr,
        } => {
          assert_bool(&expr.kind, true);
        },
        other => panic!("expected logical not under negation, got: {:?}", other),
      },
      other => panic!("expected negation, got: {:?}", other),
    }
  }

  #[test]
  fn parses_reference_then_dereference() {
    let expr = parse("*&x").unwrap();
    match expr {
      ExprKind::Unary {
        op: UnaryOp::Deref,
        expr,
      } => match expr.kind {
        ExprKind::Unary {
          op:
            UnaryOp::Ref {
              mutability: Mutability::Immutable,
              depth: 1,
            },
          expr,
        } => {
          assert_path(&expr.kind, "x");
        },
        other => panic!("expected reference under deref, got: {:?}", other),
      },
      other => panic!("expected deref, got: {:?}", other),
    }
  }

  #[test]
  fn parses_unary_before_postfix_chain() {
    let expr = parse("!foo()").unwrap();
    match expr {
      ExprKind::Unary {
        op: UnaryOp::Not,
        expr,
      } => match expr.kind {
        ExprKind::Call { callee, args } => {
          assert_path(&callee.kind, "foo");
          assert!(args.is_empty());
        },
        other => panic!("expected call under unary, got: {:?}", other),
      },
      other => panic!("expected unary not, got: {:?}", other),
    }
  }

  #[test]
  fn parses_nested_grouped_unary() {
    let expr = parse("-(-x)").unwrap();
    match expr {
      ExprKind::Unary {
        op: UnaryOp::Neg,
        expr,
      } => match expr.kind {
        ExprKind::Group { expr } => match expr.kind {
          ExprKind::Unary {
            op: UnaryOp::Neg,
            expr,
          } => {
            assert_path(&expr.kind, "x");
          },
          other => panic!("expected inner negation, got: {:?}", other),
        },
        other => panic!("expected group, got: {:?}", other),
      },
      other => panic!("expected outer negation, got: {:?}", other),
    }
  }
}
