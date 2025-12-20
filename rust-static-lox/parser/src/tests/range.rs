#[cfg(test)]
mod range_tests {

  use crate::{
    ast::{
      expr::{ExprKind, RangeExprKind},
      Lit,
    },
    parser_utils::ExprContext,
    tests::support::parse_expression_expr,
  };

  #[derive(Debug, PartialEq)]
  struct SimpleRange {
    start: Option<i128>,
    end: Option<i128>,
    kind: RangeExprKind,
  }

  fn simplify(expr: &ExprKind) -> SimpleRange {
    match expr {
      ExprKind::Range { start, end, kind } => SimpleRange {
        start: start.as_ref().map(|expr| extract_int(&expr.kind)),
        end: end.as_ref().map(|expr| extract_int(&expr.kind)),
        kind: kind.clone(),
      },
      _ => panic!("unexpected expression in range tests: {:?}", expr),
    }
  }

  fn extract_int(expr: &ExprKind) -> i128 {
    match expr {
      ExprKind::Literal(Lit::Integer { value, .. }) => *value,
      _ => panic!("expected integer literal, got: {:?}", expr),
    }
  }

  fn parse(input: &str) -> Result<ExprKind, ()> {
    parse_expression_expr(input, "range_expr_test_temp", ExprContext::Default)
  }

  fn assert_range(input: &str, expected: SimpleRange) {
    let expr = parse(input).unwrap();
    assert_eq!(simplify(&expr), expected);
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
        start: Some(1),
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
        end: Some(5),
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
        end: Some(5),
        kind: RangeExprKind::ToInclusive,
      },
    );
  }

  #[test]
  fn parses_range_with_start_and_end() {
    assert_range(
      "1..2",
      SimpleRange {
        start: Some(1),
        end: Some(2),
        kind: RangeExprKind::To,
      },
    );
  }

  #[test]
  fn parses_inclusive_range_with_start_and_end() {
    assert_range(
      "1..=2",
      SimpleRange {
        start: Some(1),
        end: Some(2),
        kind: RangeExprKind::ToInclusive,
      },
    );
  }

  #[test]
  fn parses_inclusive_from_range() {
    assert_range(
      "1..=",
      SimpleRange {
        start: Some(1),
        end: None,
        kind: RangeExprKind::FromInclusive,
      },
    );
  }

  #[test]
  fn parses_exclusive_range_without_bounds() {
    assert_range(
      "..=",
      SimpleRange {
        start: None,
        end: None,
        kind: RangeExprKind::Exclusive,
      },
    );
  }

  #[test]
  fn rejects_chained_ranges() {
    assert_err("1..2..3");
  }
}
