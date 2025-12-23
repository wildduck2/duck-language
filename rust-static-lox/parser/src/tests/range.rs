#[cfg(test)]
mod range_tests {

  use crate::{
    ast::{
      expr::{BinaryOp, ExprKind, RangeExprKind, UnaryOp},
      Lit,
    },
    parser_utils::ParserContext,
    tests::support::parse_expression,
  };

  #[derive(Debug, PartialEq)]
  enum SimpleExpr {
    Int(i128),
    Unary {
      op: UnaryOp,
      expr: Box<SimpleExpr>,
    },
    Binary {
      op: BinaryOp,
      left: Box<SimpleExpr>,
      right: Box<SimpleExpr>,
    },
    Group(Box<SimpleExpr>),
  }

  #[derive(Debug, PartialEq)]
  struct SimpleRange {
    start: Option<SimpleExpr>,
    end: Option<SimpleExpr>,
    kind: RangeExprKind,
  }

  fn int(value: i128) -> SimpleExpr {
    SimpleExpr::Int(value)
  }

  fn unary(op: UnaryOp, expr: SimpleExpr) -> SimpleExpr {
    SimpleExpr::Unary {
      op,
      expr: Box::new(expr),
    }
  }

  fn bin(op: BinaryOp, left: SimpleExpr, right: SimpleExpr) -> SimpleExpr {
    SimpleExpr::Binary {
      op,
      left: Box::new(left),
      right: Box::new(right),
    }
  }

  fn group(expr: SimpleExpr) -> SimpleExpr {
    SimpleExpr::Group(Box::new(expr))
  }

  fn simplify_expr(expr: &ExprKind) -> SimpleExpr {
    match expr {
      ExprKind::Literal(Lit::Integer { value, .. }) => int(*value),

      ExprKind::Unary { op, expr } => unary(op.clone(), simplify_expr(&expr.kind)),

      ExprKind::Binary { left, op, right } => {
        bin(*op, simplify_expr(&left.kind), simplify_expr(&right.kind))
      },

      ExprKind::Group { expr } => group(simplify_expr(&expr.kind)),

      other => panic!("unexpected bound expression in range tests: {:?}", other),
    }
  }

  fn simplify_range(expr: &ExprKind) -> SimpleRange {
    match expr {
      ExprKind::Range { start, end, kind } => SimpleRange {
        start: start.as_ref().map(|e| simplify_expr(&e.kind)),
        end: end.as_ref().map(|e| simplify_expr(&e.kind)),
        kind: kind.clone(),
      },
      other => panic!("unexpected expression in range tests: {:?}", other),
    }
  }

  fn parse(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "range_expr_test_temp", ParserContext::Default)
  }

  fn assert_range(input: &str, expected: SimpleRange) {
    let expr = parse(input).unwrap();
    assert_eq!(simplify_range(&expr), expected);
  }

  fn assert_err(input: &str) {
    assert!(parse(input).is_err(), "expected error for: {input:?}");
  }

  #[test]
  fn parses_full_range() {
    assert_range(
      "..",
      SimpleRange {
        start: None,
        end: None,
        kind: RangeExprKind::Full,
      },
    );
  }

  #[test]
  fn parses_from_range() {
    assert_range(
      "1..",
      SimpleRange {
        start: Some(int(1)),
        end: None,
        kind: RangeExprKind::From,
      },
    );
  }

  #[test]
  fn parses_to_range() {
    assert_range(
      "..5",
      SimpleRange {
        start: None,
        end: Some(int(5)),
        kind: RangeExprKind::To,
      },
    );
  }

  #[test]
  fn parses_inclusive_to_range() {
    assert_range(
      "..=5",
      SimpleRange {
        start: None,
        end: Some(int(5)),
        kind: RangeExprKind::ToInclusive,
      },
    );
  }

  #[test]
  fn parses_range_with_start_and_end() {
    assert_range(
      "1..2",
      SimpleRange {
        start: Some(int(1)),
        end: Some(int(2)),
        kind: RangeExprKind::To,
      },
    );
  }

  #[test]
  fn parses_inclusive_range_with_start_and_end() {
    assert_range(
      "1..=2",
      SimpleRange {
        start: Some(int(1)),
        end: Some(int(2)),
        kind: RangeExprKind::ToInclusive,
      },
    );
  }

  #[test]
  fn inclusive_from_range_errors() {
    assert_err("1..=");
  }

  #[test]
  fn dot_dot_eq_without_bounds_errors() {
    assert_err("..=");
  }

  #[test]
  fn rejects_chained_ranges() {
    assert_err("1..2..3");
  }

  #[test]
  fn unary_binds_tighter_than_range_start() {
    assert_range(
      "-1..2",
      SimpleRange {
        start: Some(unary(UnaryOp::Neg, int(1))),
        end: Some(int(2)),
        kind: RangeExprKind::To,
      },
    );
  }

  #[test]
  fn unary_binds_tighter_than_range_end() {
    assert_range(
      "1..-2",
      SimpleRange {
        start: Some(int(1)),
        end: Some(unary(UnaryOp::Neg, int(2))),
        kind: RangeExprKind::To,
      },
    );
  }

  #[test]
  fn addition_in_start_groups_inside_start() {
    assert_range(
      "1 + 2..3",
      SimpleRange {
        start: Some(bin(BinaryOp::Add, int(1), int(2))),
        end: Some(int(3)),
        kind: RangeExprKind::To,
      },
    );
  }

  #[test]
  fn addition_in_end_groups_inside_end() {
    assert_range(
      "1..2 + 3",
      SimpleRange {
        start: Some(int(1)),
        end: Some(bin(BinaryOp::Add, int(2), int(3))),
        kind: RangeExprKind::To,
      },
    );
  }

  #[test]
  fn shift_in_end_groups_inside_end() {
    assert_range(
      "1..2 << 3",
      SimpleRange {
        start: Some(int(1)),
        end: Some(bin(BinaryOp::Shl, int(2), int(3))),
        kind: RangeExprKind::To,
      },
    );
  }

  #[test]
  fn shift_in_start_groups_inside_start() {
    assert_range(
      "1 << 2..3",
      SimpleRange {
        start: Some(bin(BinaryOp::Shl, int(1), int(2))),
        end: Some(int(3)),
        kind: RangeExprKind::To,
      },
    );
  }

  #[test]
  fn parenthesized_bounds() {
    assert_range(
      "(1)..(2)",
      SimpleRange {
        start: Some(group(int(1))),
        end: Some(group(int(2))),
        kind: RangeExprKind::To,
      },
    );
  }

  #[test]
  fn range_inside_parentheses_is_group_expr() {
    let expr = parse("(1..2)").unwrap();
    match &expr {
      ExprKind::Group { expr } => {
        assert_eq!(
          simplify_range(&expr.kind),
          SimpleRange {
            start: Some(int(1)),
            end: Some(int(2)),
            kind: RangeExprKind::To,
          }
        );
      },
      other => panic!("expected group, got: {:?}", other),
    }
  }
}
