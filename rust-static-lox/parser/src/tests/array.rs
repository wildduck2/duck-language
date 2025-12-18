#[cfg(test)]
mod array_tests {

  use crate::{
    ast::{expr::ExprKind, Lit},
    parser_utils::ExprContext,
    tests::support::parse_primary_expr,
  };

  fn parse_single(input: &str) -> Result<ExprKind, ()> {
    parse_primary_expr(input, "array_test_temp", ExprContext::Default)
  }

  fn assert_err(input: &str) {
    assert!(
      parse_single(input).is_err(),
      "expected error for: {:?}",
      input
    );
  }

  #[derive(Debug, PartialEq)]
  enum SimpleExpr {
    Int(i128),
    Array {
      elements: Vec<SimpleExpr>,
      repeat: Option<Box<SimpleExpr>>,
    },
  }

  fn int(value: i128) -> SimpleExpr {
    SimpleExpr::Int(value)
  }

  fn array(elements: Vec<SimpleExpr>) -> SimpleExpr {
    SimpleExpr::Array {
      elements,
      repeat: None,
    }
  }

  fn repeat_array(value: SimpleExpr, repeat: SimpleExpr) -> SimpleExpr {
    SimpleExpr::Array {
      elements: vec![value],
      repeat: Some(Box::new(repeat)),
    }
  }

  fn simplify(expr: &ExprKind) -> SimpleExpr {
    match expr {
      ExprKind::Literal(Lit::Integer { value, .. }) => int(*value),
      ExprKind::Array { elements, repeat } => SimpleExpr::Array {
        elements: elements.iter().map(|e| simplify(&e.kind)).collect(),
        repeat: repeat.as_ref().map(|expr| Box::new(simplify(&expr.kind))),
      },
      _ => panic!("unsupported expression in array tests: {:?}", expr),
    }
  }

  fn assert_expr(input: &str, expected: SimpleExpr) {
    let expr = parse_single(input).unwrap();
    assert_eq!(simplify(&expr), expected);
  }

  #[test]
  fn empty_array() {
    assert_expr("[]", array(vec![]));
  }

  #[test]
  fn single_element_array() {
    assert_expr("[1]", array(vec![int(1)]));
  }

  #[test]
  fn multiple_elements_array() {
    assert_expr("[1, 2, 3]", array(vec![int(1), int(2), int(3)]));
  }

  #[test]
  fn trailing_comma_array() {
    assert_expr("[1, 2, 3,]", array(vec![int(1), int(2), int(3)]));
  }

  #[test]
  fn repeated_commas_error() {
    assert_err("[1,,2]");
  }

  #[test]
  fn nested_arrays() {
    assert_expr(
      "[[1], [2, 3]]",
      array(vec![array(vec![int(1)]), array(vec![int(2), int(3)])]),
    );
  }

  #[test]
  fn repeat_form_array() {
    assert_expr("[1; 4]", repeat_array(int(1), int(4)));
  }

  #[test]
  fn repeat_form_requires_literal_count() {
    assert_err("[1; foo]");
  }

  #[test]
  fn repeat_form_missing_count() {
    assert_err("[1;]");
  }

  #[test]
  fn missing_closing_bracket_errors() {
    assert_err("[1, 2");
  }

  #[test]
  fn missing_separator_is_allowed() {
    assert_expr("[1 2]", array(vec![int(1), int(2)]));
  }
}
