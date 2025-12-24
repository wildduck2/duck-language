#[cfg(test)]
mod shift_tests {

  use crate::{
    ast::expr::{BinaryOp, ExprKind, RangeExprKind, UnaryOp},
    parser_utils::ParserContext,
    tests::support::{bin, group, int, simplify_expr, unary, SimpleExpr, parse_expression},
  };

  fn parse(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "shift_expr_test_temp", ParserContext::Default)
  }

  fn assert_expr(input: &str, expected: SimpleExpr) {
    let expr = parse(input).unwrap();
    assert_eq!(simplify_expr(&expr), expected);
  }

  fn assert_err(input: &str) {
    assert!(parse(input).is_err(), "expected error for: {input:?}");
  }

  #[test]
  fn parses_left_shift() {
    assert_expr("1 << 2", bin(BinaryOp::Shl, int(1), int(2)));
  }

  #[test]
  fn parses_right_shift() {
    assert_expr("8 >> 2", bin(BinaryOp::Shr, int(8), int(2)));
  }

  #[test]
  fn chains_shifts_left_associatively() {
    assert_expr(
      "1 << 2 >> 3",
      bin(BinaryOp::Shr, bin(BinaryOp::Shl, int(1), int(2)), int(3)),
    );
  }

  #[test]
  fn range_after_shift_parses_as_range() {
    let expr = parse("1 << 2..3").unwrap();
    match expr {
      ExprKind::Range { start, end, kind } => {
        assert_eq!(kind, RangeExprKind::To);
        assert_eq!(
          simplify_expr(&start.unwrap().kind),
          bin(BinaryOp::Shl, int(1), int(2))
        );
        assert_eq!(simplify_expr(&end.unwrap().kind), int(3));
      },
      other => panic!("expected range expression, got: {:?}", other),
    }
  }

  #[test]
  fn inclusive_range_after_shift_parses_as_range() {
    let expr = parse("1 >> 2..=3").unwrap();
    match expr {
      ExprKind::Range { start, end, kind } => {
        assert_eq!(kind, RangeExprKind::ToInclusive);
        assert_eq!(
          simplify_expr(&start.unwrap().kind),
          bin(BinaryOp::Shr, int(1), int(2))
        );
        assert_eq!(simplify_expr(&end.unwrap().kind), int(3));
      },
      other => panic!("expected range expression, got: {:?}", other),
    }
  }

  #[test]
  fn shift_binds_looser_than_addition_left() {
    assert_expr(
      "1 + 2 << 3",
      bin(BinaryOp::Shl, bin(BinaryOp::Add, int(1), int(2)), int(3)),
    );
  }

  #[test]
  fn shift_binds_looser_than_addition_right() {
    assert_expr(
      "1 << 2 + 3",
      bin(BinaryOp::Shl, int(1), bin(BinaryOp::Add, int(2), int(3))),
    );
  }

  #[test]
  fn shift_binds_tighter_than_bitwise_and_left() {
    assert_expr(
      "1 & 2 << 3",
      bin(BinaryOp::BitAnd, int(1), bin(BinaryOp::Shl, int(2), int(3))),
    );
  }

  #[test]
  fn shift_binds_tighter_than_bitwise_and_right() {
    assert_expr(
      "1 << 2 & 3",
      bin(BinaryOp::BitAnd, bin(BinaryOp::Shl, int(1), int(2)), int(3)),
    );
  }

  #[test]
  fn parentheses_override_shift_precedence() {
    assert_expr(
      "1 << (2 + 3)",
      bin(
        BinaryOp::Shl,
        int(1),
        group(bin(BinaryOp::Add, int(2), int(3))),
      ),
    );
  }

  #[test]
  fn nested_parentheses_override_shift() {
    assert_expr(
      "(1 << 2) + 3",
      bin(
        BinaryOp::Add,
        group(bin(BinaryOp::Shl, int(1), int(2))),
        int(3),
      ),
    );
  }

  #[test]
  fn shift_binds_tighter_than_equality() {
    assert_expr(
      "1 << 2 == 4",
      bin(BinaryOp::Eq, bin(BinaryOp::Shl, int(1), int(2)), int(4)),
    );
  }

  #[test]
  fn errors_on_double_left_shift_operator() {
    assert_err("1 << << 2");
  }

  #[test]
  fn errors_on_double_right_shift_operator() {
    assert_err("1 >> >> 2");
  }

  #[test]
  fn errors_on_invalid_token_after_shift() {
    assert_err("1 << +");
  }

  #[test]
  fn errors_on_missing_rhs_after_shift() {
    assert_err("1 <<");
  }

  #[test]
  fn unary_binds_tighter_than_shift_left() {
    assert_expr(
      "-1 << 2",
      bin(BinaryOp::Shl, unary(UnaryOp::Neg, int(1)), int(2)),
    );
  }

  #[test]
  fn unary_binds_tighter_than_shift_right() {
    assert_expr(
      "1 << -2",
      bin(BinaryOp::Shl, int(1), unary(UnaryOp::Neg, int(2))),
    );
  }

  #[test]
  fn shift_binds_tighter_than_less_than() {
    assert_expr(
      "1 << 2 < 4",
      bin(BinaryOp::Less, bin(BinaryOp::Shl, int(1), int(2)), int(4)),
    );
  }

  #[test]
  fn less_than_binds_looser_than_shift() {
    assert_expr(
      "1 < 2 << 3",
      bin(BinaryOp::Less, int(1), bin(BinaryOp::Shl, int(2), int(3))),
    );
  }

  #[test]
  fn shift_binds_tighter_than_logical_and() {
    assert_expr(
      "1 << 2 && 3",
      bin(BinaryOp::And, bin(BinaryOp::Shl, int(1), int(2)), int(3)),
    );
  }

  #[test]
  fn logical_or_binds_looser_than_shift() {
    assert_expr(
      "1 || 2 << 3",
      bin(BinaryOp::Or, int(1), bin(BinaryOp::Shl, int(2), int(3))),
    );
  }

  #[test]
  fn shift_rhs_group_only() {
    assert_expr("1 << (2)", bin(BinaryOp::Shl, int(1), group(int(2))));
  }

  #[test]
  fn nested_shift_on_rhs() {
    assert_expr(
      "1 << (2 << 3)",
      bin(
        BinaryOp::Shl,
        int(1),
        group(bin(BinaryOp::Shl, int(2), int(3))),
      ),
    );
  }

  #[test]
  fn invalid_lhs_shift_errors() {
    assert_err("<< 1");
  }
}
