#[cfg(test)]
mod bitwise_tests {

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
      _ => panic!("unexpected expression in bitwise tests: {:?}", expr),
    }
  }

  fn parse(input: &str) -> Result<ExprKind, ()> {
    parse_expression_expr(input, "bitwise_expr_test_temp", ExprContext::Default)
  }

  fn assert_expr(input: &str, expected: SimpleExpr) {
    let expr = parse(input).unwrap();
    assert_eq!(simplify(&expr), expected);
  }

  fn assert_err(input: &str) {
    assert!(parse(input).is_err(), "expected error for: {input:?}");
  }

  #[test]
  fn parses_bitwise_or_chain() {
    assert_expr(
      "1 | 2 | 3",
      bin(BinaryOp::BitOr, bin(BinaryOp::BitOr, int(1), int(2)), int(3)),
    );
  }

  #[test]
  fn parses_bitwise_xor_chain() {
    assert_expr(
      "5 ^ 6 ^ 7",
      bin(BinaryOp::BitXor, bin(BinaryOp::BitXor, int(5), int(6)), int(7)),
    );
  }

  #[test]
  fn parses_bitwise_and_chain() {
    assert_expr(
      "8 & 4 & 2",
      bin(BinaryOp::BitAnd, bin(BinaryOp::BitAnd, int(8), int(4)), int(2)),
    );
  }

  #[test]
  fn respects_bitwise_precedence() {
    assert_expr(
      "1 | 2 & 3 ^ 4",
      bin(
        BinaryOp::BitOr,
        int(1),
        bin(BinaryOp::BitXor, bin(BinaryOp::BitAnd, int(2), int(3)), int(4)),
      ),
    );
  }

  #[test]
  fn errors_when_or_missing_rhs() {
    assert_err("1 |");
  }

  #[test]
  fn errors_when_xor_missing_rhs() {
    assert_err("1 ^");
  }

  #[test]
  fn errors_when_and_missing_rhs() {
    assert_err("1 &");
  }

  #[test]
  fn match_context_short_circuits_bitwise_or() {
    let expr =
      parse_expression_expr("1", "bitwise_match_context", ExprContext::Match).unwrap();
    assert_eq!(simplify(&expr), int(1));
  }
}
