#[cfg(test)]
mod logical_tests {

  use crate::{
    ast::{
      expr::{BinaryOp, ExprKind},
      Lit,
    },
    parser_utils::ExprContext,
    tests::support::parse_expression,
  };

  #[derive(Debug, PartialEq)]
  enum SimpleExpr {
    Bool(bool),
    Int(i128),
    Binary {
      op: BinaryOp,
      left: Box<SimpleExpr>,
      right: Box<SimpleExpr>,
    },
  }

  fn boolean(value: bool) -> SimpleExpr {
    SimpleExpr::Bool(value)
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
      ExprKind::Literal(Lit::Bool(value)) => boolean(*value),
      ExprKind::Literal(Lit::Integer { value, .. }) => SimpleExpr::Int(*value),
      ExprKind::Group { expr } => simplify(&expr.kind),
      ExprKind::Binary { left, op, right } => bin(*op, simplify(&left.kind), simplify(&right.kind)),
      _ => panic!("unexpected expression in logical tests: {:?}", expr),
    }
  }

  fn parse(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "logical_expr_test_temp", ExprContext::Default)
  }

  fn assert_expr(input: &str, expected: SimpleExpr) {
    let expr = parse(input).unwrap();
    assert_eq!(simplify(&expr), expected);
  }

  fn assert_err(input: &str) {
    assert!(parse(input).is_err(), "expected error for: {input:?}");
  }

  #[test]
  fn parses_logical_or() {
    assert_expr(
      "1 || 0",
      bin(BinaryOp::Or, SimpleExpr::Int(1), SimpleExpr::Int(0)),
    );
  }

  #[test]
  fn parses_logical_and() {
    assert_expr(
      "1 && 0",
      bin(BinaryOp::And, SimpleExpr::Int(1), SimpleExpr::Int(0)),
    );
  }

  #[test]
  fn logical_and_has_higher_precedence_than_or() {
    assert_expr(
      "1 || 0 && 2",
      bin(
        BinaryOp::Or,
        SimpleExpr::Int(1),
        bin(BinaryOp::And, SimpleExpr::Int(0), SimpleExpr::Int(2)),
      ),
    );
  }

  #[test]
  fn chains_logical_and_left_associatively() {
    assert_expr(
      "1 && 2 && 3",
      bin(
        BinaryOp::And,
        bin(BinaryOp::And, SimpleExpr::Int(1), SimpleExpr::Int(2)),
        SimpleExpr::Int(3),
      ),
    );
  }

  #[test]
  fn errors_when_or_missing_rhs() {
    assert_err("true ||");
  }

  #[test]
  fn errors_when_and_missing_rhs() {
    assert_err("true &&");
  }

  #[test]
  fn parentheses_override_precedence() {
    assert_expr(
      "(1 || 0) && 2",
      bin(
        BinaryOp::And,
        bin(BinaryOp::Or, SimpleExpr::Int(1), SimpleExpr::Int(0)),
        SimpleExpr::Int(2),
      ),
    );
  }

  #[test]
  fn single_and_is_not_logical_and() {
    assert_err("true & false");
  }

  #[test]
  fn single_or_is_not_logical_or() {
    assert_err("true | false");
  }
}
