#[cfg(test)]
mod variable_tests {
  use crate::{
    ast::{
      expr::ExprKind,
      pattern::{BindingMode, Pattern},
      Lit, Mutability, Stmt, Type,
    },
    parser_utils::ParserContext,
    tests::support::run_parser,
  };

  fn parse_stmt(input: &str) -> Result<Stmt, ()> {
    run_parser(input, "variable_stmt_test_temp", |parser| {
      parser.parse_stmt(ParserContext::Default)
    })
  }

  fn assert_ident_pattern(pattern: &Pattern, expected: &str, mutability: Mutability) {
    match pattern {
      Pattern::Ident {
        binding,
        name,
        subpattern,
        ..
      } => {
        assert_eq!(name, expected);
        assert_eq!(binding, &BindingMode::ByValue(mutability));
        assert!(subpattern.is_none());
      },
      other => panic!("expected ident pattern, got: {:?}", other),
    }
  }

  fn assert_tuple_pattern(pattern: &Pattern, expected: &[&str]) {
    match pattern {
      Pattern::Tuple { patterns, .. } => {
        assert_eq!(patterns.len(), expected.len());
        for (pattern, name) in patterns.iter().zip(expected.iter()) {
          assert_ident_pattern(pattern, name, Mutability::Immutable);
        }
      },
      other => panic!("expected tuple pattern, got: {:?}", other),
    }
  }

  #[test]
  fn parses_simple_let_binding() {
    let stmt = parse_stmt("let x = 1").unwrap();
    match stmt {
      Stmt::Let(let_stmt) => {
        assert_ident_pattern(&let_stmt.pattern, "x", Mutability::Immutable);
        assert!(let_stmt.ty.is_none());
        match let_stmt.init.as_deref() {
          Some(expr) => match &expr.kind {
            ExprKind::Literal(Lit::Integer { value, .. }) => assert_eq!(*value, 1),
            other => panic!("expected integer literal init, got: {:?}", other),
          },
          None => panic!("expected init expression"),
        }
        assert!(let_stmt.else_block.is_none());
      },
      other => panic!("expected let statement, got: {:?}", other),
    }
  }

  #[test]
  fn parses_typed_let_binding() {
    let stmt = parse_stmt("let x: i32").unwrap();
    match stmt {
      Stmt::Let(let_stmt) => {
        assert_ident_pattern(&let_stmt.pattern, "x", Mutability::Immutable);
        assert_eq!(let_stmt.ty, Some(Type::I32));
        assert!(let_stmt.init.is_none());
        assert!(let_stmt.else_block.is_none());
      },
      other => panic!("expected let statement, got: {:?}", other),
    }
  }

  #[test]
  fn parses_mut_let_binding_with_init() {
    let stmt = parse_stmt("let mut x: i32 = 1").unwrap();
    match stmt {
      Stmt::Let(let_stmt) => {
        assert_ident_pattern(&let_stmt.pattern, "x", Mutability::Mutable);
        assert_eq!(let_stmt.ty, Some(Type::I32));
        match let_stmt.init.as_deref() {
          Some(expr) => match &expr.kind {
            ExprKind::Literal(Lit::Integer { value, .. }) => assert_eq!(*value, 1),
            other => panic!("expected integer literal init, got: {:?}", other),
          },
          None => panic!("expected init expression"),
        }
        assert!(let_stmt.else_block.is_none());
      },
      other => panic!("expected let statement, got: {:?}", other),
    }
  }

  #[test]
  fn parses_tuple_pattern_let_binding() {
    let stmt = parse_stmt("let (x, y) = foo").unwrap();
    match stmt {
      Stmt::Let(let_stmt) => {
        assert_tuple_pattern(&let_stmt.pattern, &["x", "y"]);
        assert!(let_stmt.ty.is_none());
        assert!(let_stmt.init.is_some());
      },
      other => panic!("expected let statement, got: {:?}", other),
    }
  }

  #[test]
  fn parses_reference_pattern_let_binding() {
    let stmt = parse_stmt("let &x = y").unwrap();
    match stmt {
      Stmt::Let(let_stmt) => match &let_stmt.pattern {
        Pattern::Reference {
          depth,
          mutability,
          pattern,
          ..
        } => {
          assert_eq!(*depth, 1);
          assert_eq!(*mutability, Mutability::Immutable);
          assert_ident_pattern(pattern.as_ref(), "x", Mutability::Immutable);
        },
        other => panic!("expected reference pattern, got: {:?}", other),
      },
      other => panic!("expected let statement, got: {:?}", other),
    }
  }

  #[test]
  fn parses_let_else_statement() {
    assert!(parse_stmt("let x = y else { }").is_ok());
  }
}
#[cfg(test)]
mod variables_tests {
  use crate::{
    ast::{expr::ExprKind, BinaryOp},
    parser_utils::ParserContext,
    tests::support::parse_expression,
  };

  fn parse(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "variables_expr_test_temp", ParserContext::Default)
  }

  fn assert_assign(input: &str) {
    let expr = parse(input).unwrap();
    match expr {
      ExprKind::Assign { .. } => {},
      other => panic!("expected assignment expression, got: {:?}", other),
    }
  }

  fn assert_err(input: &str) {
    assert!(parse(input).is_err(), "expected error for {input:?}");
  }

  #[test]
  fn parses_simple_assignment() {
    assert_assign("x = 1");
  }

  #[test]
  fn parses_compound_assignments() {
    for op in ["+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>="] {
      let expr = parse(&format!("x {op} 1")).unwrap();
      match expr {
        ExprKind::Assign { .. } => {},
        other => panic!("expected assignment expression, got: {:?}", other),
      }
    }
  }

  #[test]
  fn assignment_has_lower_precedence_than_binary() {
    let expr = parse("x = 1 + 2").unwrap();
    match expr {
      ExprKind::Assign { value, .. } => match value.kind {
        ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Add),
        other => panic!("expected binary value, got: {:?}", other),
      },
      other => panic!("expected assignment expression, got: {:?}", other),
    }
  }

  #[test]
  fn errors_on_missing_rhs() {
    assert_err("x =");
  }

  #[test]
  fn rejects_chained_assignments() {
    assert_err("x = y = 1");
  }
}
