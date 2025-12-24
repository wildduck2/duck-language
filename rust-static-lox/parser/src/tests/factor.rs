#[cfg(test)]
mod factor_tests {

  use crate::{
    ast::{
      expr::{BinaryOp, ExprKind},
      Lit,
    },
    parser_utils::ParserContext,
    tests::support::parse_expression,
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

  fn int(v: i128) -> SimpleExpr {
    SimpleExpr::Int(v)
  }

  fn bin(op: BinaryOp, l: SimpleExpr, r: SimpleExpr) -> SimpleExpr {
    SimpleExpr::Binary {
      op,
      left: Box::new(l),
      right: Box::new(r),
    }
  }

  fn simplify(expr: &ExprKind) -> SimpleExpr {
    match expr {
      ExprKind::Literal(Lit::Integer { value, .. }) => int(*value),

      ExprKind::Binary { left, op, right } => bin(*op, simplify(&left.kind), simplify(&right.kind)),

      ExprKind::Group { expr } => simplify(&expr.kind),

      other => panic!("unexpected expr in factor tests: {:?}", other),
    }
  }

  fn parse(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "factor_expr_test_temp", ParserContext::Default)
  }

  fn assert_expr(input: &str, expected: SimpleExpr) {
    let expr = parse(input).unwrap();
    assert_eq!(simplify(&expr), expected);
  }

  fn assert_err(input: &str) {
    assert!(parse(input).is_err(), "expected error for {input:?}");
  }

  // Basic factor operators

  #[test]
  fn parses_multiplication() {
    assert_expr("2 * 3", bin(BinaryOp::Mul, int(2), int(3)));
  }

  #[test]
  fn parses_division() {
    assert_expr("6 / 3", bin(BinaryOp::Div, int(6), int(3)));
  }

  #[test]
  fn parses_remainder() {
    assert_expr("7 % 4", bin(BinaryOp::Mod, int(7), int(4)));
  }

  // Left associativity

  #[test]
  fn multiplication_is_left_associative() {
    assert_expr(
      "2 * 3 * 4",
      bin(BinaryOp::Mul, bin(BinaryOp::Mul, int(2), int(3)), int(4)),
    );
  }

  #[test]
  fn mixed_factor_ops_are_left_associative() {
    assert_expr(
      "8 / 4 % 3",
      bin(BinaryOp::Mod, bin(BinaryOp::Div, int(8), int(4)), int(3)),
    );
  }

  // Factor vs cast precedence

  #[test]
  fn cast_binds_tighter_than_multiplication() {
    assert_expr("2 * 3", bin(BinaryOp::Mul, int(2), int(3)));
  }

  // Factor vs addition precedence

  #[test]
  fn factor_binds_tighter_than_addition() {
    assert_expr(
      "1 + 2 * 3",
      bin(BinaryOp::Add, int(1), bin(BinaryOp::Mul, int(2), int(3))),
    );
  }

  // Grouping

  #[test]
  fn parenthesized_factor_changes_binding() {
    assert_expr(
      "(1 + 2) * 3",
      bin(BinaryOp::Mul, bin(BinaryOp::Add, int(1), int(2)), int(3)),
    );
  }

  // Error cases

  #[test]
  fn missing_rhs_errors() {
    assert_err("1 *");
    assert_err("1 /");
    assert_err("1 %");
  }

  #[test]
  fn double_operator_errors() {
    assert_err("1 ** 2");
    assert_err("1 %% 2");
  }
}
