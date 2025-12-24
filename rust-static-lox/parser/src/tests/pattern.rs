#[cfg(test)]
mod pattern_tests {
  use crate::{
    ast::{
      expr::ExprKind,
      path::{Path, PathSegmentKind},
      pattern::{BindingMode, Pattern, RangeKind},
      Expr, Lit, Mutability, Type,
    },
    parser_utils::ParserContext,
    tests::support::run_parser,
  };

  fn parse_pattern(input: &str) -> Result<Pattern, ()> {
    run_parser(input, "pattern_test_temp", |parser| {
      parser.parse_pattern(ParserContext::Default)
    })
  }

  fn parse_pattern_with_or(input: &str) -> Result<Pattern, ()> {
    run_parser(input, "pattern_or_test_temp", |parser| {
      parser.parse_pattern_with_or(ParserContext::Default)
    })
  }

  fn assert_ident_path(path: &Path, expected: &str) {
    assert!(!path.leading_colon);
    assert_eq!(path.segments.len(), 1);
    match &path.segments[0].kind {
      PathSegmentKind::Ident(name) => assert_eq!(name, expected),
      other => panic!("expected ident path, got: {:?}", other),
    }
  }

  fn assert_path_segments(path: &Path, expected: &[&str]) {
    assert!(!path.leading_colon);
    assert_eq!(path.segments.len(), expected.len());
    for (segment, expected_name) in path.segments.iter().zip(expected.iter()) {
      match &segment.kind {
        PathSegmentKind::Ident(name) => assert_eq!(name, expected_name),
        other => panic!("expected ident path segment, got: {:?}", other),
      }
    }
  }

  fn assert_ident_pattern(pattern: &Pattern, expected: &str) {
    match pattern {
      Pattern::Ident {
        binding,
        name,
        subpattern,
        ..
      } => {
        assert_eq!(name, expected);
        assert_eq!(binding, &BindingMode::ByValue(Mutability::Immutable));
        assert!(subpattern.is_none());
      },
      other => panic!("expected ident pattern, got: {:?}", other),
    }
  }

  fn assert_int_literal(expr: &Expr, expected: i128) {
    match &expr.kind {
      ExprKind::Literal(Lit::Integer { value, .. }) => assert_eq!(*value, expected),
      other => panic!("expected integer literal {expected}, got: {:?}", other),
    }
  }

  #[test]
  fn parses_wildcard_pattern() {
    match parse_pattern("_").unwrap() {
      Pattern::Wildcard { .. } => {},
      other => panic!("expected wildcard pattern, got: {:?}", other),
    }
  }

  #[test]
  fn parses_ident_binding() {
    match parse_pattern("x").unwrap() {
      Pattern::Ident {
        binding,
        name,
        subpattern,
        ..
      } => {
        assert_eq!(name, "x");
        assert_eq!(binding, BindingMode::ByValue(Mutability::Immutable));
        assert!(subpattern.is_none());
      },
      other => panic!("expected ident binding, got: {:?}", other),
    }
  }

  #[test]
  fn parses_mut_binding() {
    match parse_pattern("mut x").unwrap() {
      Pattern::Ident {
        binding,
        name,
        subpattern,
        ..
      } => {
        assert_eq!(name, "x");
        assert_eq!(binding, BindingMode::ByValue(Mutability::Mutable));
        assert!(subpattern.is_none());
      },
      other => panic!("expected mut binding, got: {:?}", other),
    }
  }

  #[test]
  fn parses_ref_binding() {
    match parse_pattern("ref x").unwrap() {
      Pattern::Ident {
        binding,
        name,
        subpattern,
        ..
      } => {
        assert_eq!(name, "x");
        assert_eq!(binding, BindingMode::ByRef(Mutability::Immutable));
        assert!(subpattern.is_none());
      },
      other => panic!("expected ref binding, got: {:?}", other),
    }
  }

  #[test]
  fn parses_ref_mut_binding() {
    match parse_pattern("ref mut x").unwrap() {
      Pattern::Ident {
        binding,
        name,
        subpattern,
        ..
      } => {
        assert_eq!(name, "x");
        assert_eq!(binding, BindingMode::ByRef(Mutability::Mutable));
        assert!(subpattern.is_none());
      },
      other => panic!("expected ref mut binding, got: {:?}", other),
    }
  }

  #[test]
  fn parses_at_binding() {
    match parse_pattern("x @ y").unwrap() {
      Pattern::Ident {
        binding,
        name,
        subpattern,
        ..
      } => {
        assert_eq!(name, "x");
        assert_eq!(binding, BindingMode::ByValue(Mutability::Immutable));
        match subpattern.as_deref() {
          Some(pattern) => assert_ident_pattern(pattern, "y"),
          None => panic!("expected subpattern"),
        }
      },
      other => panic!("expected at binding, got: {:?}", other),
    }
  }

  #[test]
  fn parses_reference_pattern() {
    match parse_pattern("&mut x").unwrap() {
      Pattern::Reference {
        depth,
        mutability,
        pattern,
        ..
      } => {
        assert_eq!(depth, 1);
        assert_eq!(mutability, Mutability::Mutable);
        assert_ident_pattern(pattern.as_ref(), "x");
      },
      other => panic!("expected reference pattern, got: {:?}", other),
    }
  }

  #[test]
  fn parses_reference_depth() {
    match parse_pattern("&&x").unwrap() {
      Pattern::Reference {
        depth,
        mutability,
        pattern,
        ..
      } => {
        assert_eq!(depth, 2);
        assert_eq!(mutability, Mutability::Immutable);
        assert_ident_pattern(pattern.as_ref(), "x");
      },
      other => panic!("expected reference pattern, got: {:?}", other),
    }
  }

  #[test]
  fn parses_group_pattern() {
    match parse_pattern("(x)").unwrap() {
      Pattern::Group { pattern, .. } => assert_ident_pattern(pattern.as_ref(), "x"),
      other => panic!("expected group pattern, got: {:?}", other),
    }
  }

  #[test]
  fn parses_tuple_pattern() {
    match parse_pattern("(x, y)").unwrap() {
      Pattern::Tuple { patterns, .. } => {
        assert_eq!(patterns.len(), 2);
        assert_ident_pattern(&patterns[0], "x");
        assert_ident_pattern(&patterns[1], "y");
      },
      other => panic!("expected tuple pattern, got: {:?}", other),
    }
  }

  #[test]
  fn parses_slice_pattern_with_rest() {
    match parse_pattern("[a, .., b]").unwrap() {
      Pattern::Slice {
        before,
        has_rest,
        after,
        ..
      } => {
        assert_eq!(before.len(), 1);
        assert_eq!(after.len(), 1);
        assert!(has_rest);
        assert_ident_pattern(&before[0], "a");
        assert_ident_pattern(&after[0], "b");
      },
      other => panic!("expected slice pattern, got: {:?}", other),
    }
  }

  #[test]
  fn parses_or_pattern() {
    match parse_pattern_with_or("a | b | c").unwrap() {
      Pattern::Or { patterns, .. } => {
        assert_eq!(patterns.len(), 3);
        assert_ident_pattern(&patterns[0], "a");
        assert_ident_pattern(&patterns[1], "b");
        assert_ident_pattern(&patterns[2], "c");
      },
      other => panic!("expected or pattern, got: {:?}", other),
    }
  }

  #[test]
  fn parses_qualified_path_pattern() {
    match parse_pattern("<T as Trait>::Item").unwrap() {
      Pattern::Path { qself, path, .. } => {
        match qself.as_deref() {
          Some(Type::Path(self_path)) => assert_ident_path(self_path, "T"),
          other => panic!("expected qself type path, got: {:?}", other),
        }
        assert_path_segments(&path, &["Trait", "Item"]);
      },
      other => panic!("expected path pattern, got: {:?}", other),
    }
  }

  #[test]
  fn parses_literal_pattern() {
    match parse_pattern("true").unwrap() {
      Pattern::Literal { expr, .. } => match &expr.kind {
        ExprKind::Literal(Lit::Bool(value)) => assert!(*value),
        other => panic!("expected bool literal, got: {:?}", other),
      },
      other => panic!("expected literal pattern, got: {:?}", other),
    }
  }

  #[test]
  fn parses_range_pattern() {
    match parse_pattern("1..=3").unwrap() {
      Pattern::Range {
        start, end, kind, ..
      } => {
        assert_eq!(kind, RangeKind::ToInclusive);
        assert_int_literal(start.as_ref().expect("expected start"), 1);
        assert_int_literal(end.as_ref().expect("expected end"), 3);
      },
      other => panic!("expected range pattern, got: {:?}", other),
    }
  }

  #[test]
  fn parses_rest_pattern() {
    match parse_pattern("..").unwrap() {
      Pattern::Rest { .. } => {},
      other => panic!("expected rest pattern, got: {:?}", other),
    }
  }
}
