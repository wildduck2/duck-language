#[cfg(test)]
mod cast_tests {

  use crate::{
    ast::{
      expr::{BinaryOp, ExprKind, UnaryOp},
      path::PathSegmentKind,
      ty::Type,
      Lit, Path,
    },
    parser_utils::ParserContext,
    tests::support::parse_expression,
  };

  #[derive(Debug, PartialEq)]
  enum SimpleExpr {
    Int(i128),
    Path(&'static str),
    Unary {
      op: UnaryOp,
      expr: Box<SimpleExpr>,
    },
    Cast {
      expr: Box<SimpleExpr>,
      ty: &'static str,
    },
    Binary {
      op: BinaryOp,
      left: Box<SimpleExpr>,
      right: Box<SimpleExpr>,
    },
  }

  fn int(v: i128) -> SimpleExpr {
    SimpleExpr::Int(v)
  }

  fn path(name: &'static str) -> SimpleExpr {
    SimpleExpr::Path(name)
  }

  fn unary(op: UnaryOp, expr: SimpleExpr) -> SimpleExpr {
    SimpleExpr::Unary {
      op,
      expr: Box::new(expr),
    }
  }

  fn cast(expr: SimpleExpr, ty: &'static str) -> SimpleExpr {
    SimpleExpr::Cast {
      expr: Box::new(expr),
      ty,
    }
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

      ExprKind::Path { path, .. } => {
        let seg = &path.segments[0];
        match &seg.kind {
          PathSegmentKind::Ident(name) => {
            SimpleExpr::Path(Box::leak(name.clone().into_boxed_str()))
          },
          _ => panic!("unexpected path segment"),
        }
      },

      ExprKind::Unary { op, expr } => unary(op.clone(), simplify(&expr.kind)),

      ExprKind::Cast { expr, ty } => {
        let name: &'static str = match ty {
          // Builtin primitive types
          Type::I8 => "i8",
          Type::I16 => "i16",
          Type::I32 => "i32",
          Type::I64 => "i64",
          Type::I128 => "i128",
          Type::Isize => "isize",
          Type::U8 => "u8",
          Type::U16 => "u16",
          Type::U32 => "u32",
          Type::U64 => "u64",
          Type::U128 => "u128",
          Type::Usize => "usize",
          Type::F32 => "f32",
          Type::F64 => "f64",
          Type::Bool => "bool",
          Type::Char => "char",
          Type::Str => "str",

          // Path types: Foo, std::vec::Vec, etc
          Type::Path(Path { segments, .. }) => {
            let seg = &segments[0];
            match &seg.kind {
              PathSegmentKind::Ident(name) => Box::leak(name.clone().into_boxed_str()),
              _ => panic!("unexpected type segment"),
            }
          },

          other => panic!("unexpected cast type in tests: {:?}", other),
        };

        cast(simplify(&expr.kind), name)
      },

      ExprKind::Binary { left, op, right } => bin(*op, simplify(&left.kind), simplify(&right.kind)),

      ExprKind::Group { expr } => simplify(&expr.kind),

      other => panic!("unexpected expr in cast tests: {:?}", other),
    }
  }

  fn parse(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "cast_expr_test_temp", ParserContext::Default)
  }

  fn assert_expr(input: &str, expected: SimpleExpr) {
    let expr = parse(input).unwrap();
    assert_eq!(simplify(&expr), expected);
  }

  fn assert_err(input: &str) {
    assert!(parse(input).is_err(), "expected error for {input:?}");
  }

  // ------------------------------------------------------------
  // Basic casts
  // ------------------------------------------------------------

  #[test]
  fn parses_simple_cast() {
    assert_expr("1 as i32", cast(int(1), "i32"));
  }

  #[test]
  fn parses_cast_of_identifier() {
    assert_expr("x as u64", cast(path("x"), "u64"));
  }

  // ------------------------------------------------------------
  // Cast chaining
  // ------------------------------------------------------------

  #[test]
  fn parses_chained_casts() {
    assert_expr("1 as i32 as i64", cast(cast(int(1), "i32"), "i64"));
  }

  // ------------------------------------------------------------
  // Cast precedence vs arithmetic
  // ------------------------------------------------------------

  #[test]
  fn cast_binds_tighter_than_addition() {
    assert_expr(
      "1 + 2 as i32",
      bin(BinaryOp::Add, int(1), cast(int(2), "i32")),
    );
  }

  #[test]
  fn cast_binds_tighter_than_multiplication() {
    assert_expr(
      "1 * 2 as i32",
      bin(BinaryOp::Mul, int(1), cast(int(2), "i32")),
    );
  }

  #[test]
  fn parenthesized_cast_changes_binding() {
    assert_expr(
      "(1 + 2) as i32",
      cast(bin(BinaryOp::Add, int(1), int(2)), "i32"),
    );
  }

  // ------------------------------------------------------------
  // Cast vs unary
  // ------------------------------------------------------------

  #[test]
  fn unary_applies_before_cast() {
    assert_expr("-1 as i32", cast(unary(UnaryOp::Neg, int(1)), "i32"));
  }

  // ------------------------------------------------------------
  // Errors
  // ------------------------------------------------------------

  #[test]
  fn cast_missing_type_errors() {
    assert_err("1 as");
  }

  #[test]
  fn cast_without_expression_errors() {
    assert_err("as i32");
  }

  #[test]
  fn double_as_without_type_errors() {
    assert_err("1 as as i32");
  }

  #[test]
  fn cast_inside_expression_missing_rhs_errors() {
    assert_err("1 + as i32");
  }

  #[test]
  fn cast_binds_tighter_than_comparison() {
    assert_expr(
      "1 < 2 as i32",
      bin(BinaryOp::Less, int(1), cast(int(2), "i32")),
    );
  }

  #[test]
  fn parenthesized_cast_inside_expression() {
    assert_expr(
      "1 + (2 as i32)",
      bin(BinaryOp::Add, int(1), cast(int(2), "i32")),
    );
  }

  #[test]
  fn cast_inside_multiplication_chain() {
    assert_expr(
      "1 * 2 as i32 * 3",
      bin(
        BinaryOp::Mul,
        bin(BinaryOp::Mul, int(1), cast(int(2), "i32")),
        int(3),
      ),
    );
  }

  #[test]
  fn cast_of_grouped_unary_expression() {
    assert_expr("(-1) as i32", cast(unary(UnaryOp::Neg, int(1)), "i32"));
  }

  #[test]
  fn cast_of_parenthesized_identifier() {
    assert_expr("(x) as i32", cast(path("x"), "i32"));
  }

  #[test]
  fn comparison_against_cast() {
    assert_expr(
      "1 == 2 as i32",
      bin(BinaryOp::Eq, int(1), cast(int(2), "i32")),
    );
  }

  #[test]
  fn cast_then_addition_is_valid() {
    assert_expr(
      "1 as i32 + 2",
      bin(BinaryOp::Add, cast(int(1), "i32"), int(2)),
    );
  }

  #[test]
  fn cast_of_binary_without_parentheses_errors() {
    assert_err("1 + 2 as i32 as");
  }
}
