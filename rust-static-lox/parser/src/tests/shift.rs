#[cfg(test)]
mod shift_tests {

  use crate::{
    ast::{
      expr::{BinaryOp, ExprKind},
      Lit,
    },
    parser_utils::ExprContext,
    tests::support::parse_expression_expr,
  };

  #[derive(Debug, PartialEq)]
  enum SimpleExpr {
    Int(i128),
    Binary {
      op: BinaryOp,
      left: Box<SimpleExpr>,
      right: Box<SimpleExpr>,
    },
  }

  fn int(value: i128) -> SimpleExpr {
    SimpleExpr::Int(value)
  }

  fn bin(op: BinaryOp, left: SimpleExpr, right: SimpleExpr) -> SimpleExpr {
    SimpleExpr::Binary {
      op,
      left: Box::new(left),
      right: Box::new(right),
    }
  }

  fn simplify(expr: &ExprKind) -> SimpleExpr {
    match expr {
      ExprKind::Literal(Lit::Integer { value, .. }) => int(*value),
      ExprKind::Binary { left, op, right } => {
        bin(*op, simplify(&left.kind), simplify(&right.kind))
      },
      _ => panic!("unexpected expression in shift tests: {:?}", expr),
    }
  }

  fn parse(input: &str) -> Result<ExprKind, ()> {
    parse_expression_expr(input, "shift_expr_test_temp", ExprContext::Default)
  }

  fn assert_expr(input: &str, expected: SimpleExpr) {
    let expr = parse(input).unwrap();
    assert_eq!(simplify(&expr), expected);
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
  fn errors_on_range_after_shift() {
    assert_err("1 << 2..3");
  }

  #[test]
  fn errors_on_inclusive_range_after_shift() {
    assert_err("1 >> 2..=3");
  }
}
