#[cfg(test)]
mod where_tests {

  use crate::{
    ast::{
      generic::{TraitBoundModifier, TypeBound, WhereClause, WherePredicate},
      path::Path,
      Type,
    },
    tests::support::run_parser,
  };

  fn parse_where_clause(input: &str) -> Result<Option<WhereClause>, ()> {
    run_parser(input, "where_clause_test_temp", |parser| {
      parser.parse_where_clause()
    })
  }

  fn ty(name: &str) -> Type {
    Type::Path(Path::from_ident(name.to_string()))
  }

  fn trait_bound(name: &str) -> TypeBound {
    TypeBound::Trait {
      modifier: TraitBoundModifier::None,
      for_lifetimes: None,
      path: Path::from_ident(name.to_string()),
    }
  }

  fn lifetime_bound(name: &str) -> TypeBound {
    TypeBound::Lifetime {
      name: name.to_string(),
    }
  }

  #[test]
  fn returns_none_when_where_missing() {
    assert_eq!(parse_where_clause("").unwrap(), None);
  }

  #[test]
  fn parses_type_predicate_with_trait_bound() {
    let clause = parse_where_clause("where T: Clone").unwrap().unwrap();
    assert_eq!(
      clause,
      WhereClause {
        predicates: vec![WherePredicate::Type {
          for_lifetimes: None,
          ty: ty("T"),
          bounds: vec![trait_bound("Clone")],
        }],
      }
    );
  }

  #[test]
  fn parses_lifetime_predicate_with_bounds() {
    let clause = parse_where_clause("where 'a: 'b + 'c").unwrap().unwrap();
    assert_eq!(
      clause,
      WhereClause {
        predicates: vec![WherePredicate::Lifetime {
          lifetime: "'a".to_string(),
          bounds: vec!["'b".to_string(), "'c".to_string()],
        }],
      }
    );
  }

  #[test]
  fn parses_for_lifetimes_on_type_predicate() {
    let clause = parse_where_clause("where for<'a, 'b> T: 'a + Trait")
      .unwrap()
      .unwrap();
    assert_eq!(
      clause,
      WhereClause {
        predicates: vec![WherePredicate::Type {
          for_lifetimes: Some(vec!["'a".to_string(), "'b".to_string()]),
          ty: ty("T"),
          bounds: vec![lifetime_bound("'a"), trait_bound("Trait")],
        }],
      }
    );
  }

  #[test]
  fn parses_equality_predicate() {
    let clause = parse_where_clause("where T = U").unwrap().unwrap();
    assert_eq!(
      clause,
      WhereClause {
        predicates: vec![WherePredicate::Equality {
          ty: ty("T"),
          equals: ty("U"),
        }],
      }
    );
  }

  #[test]
  fn parses_multiple_predicates() {
    let clause = parse_where_clause("where T: Clone, 'a").unwrap().unwrap();
    assert_eq!(
      clause,
      WhereClause {
        predicates: vec![
          WherePredicate::Type {
            for_lifetimes: None,
            ty: ty("T"),
            bounds: vec![trait_bound("Clone")],
          },
          WherePredicate::Lifetime {
            lifetime: "'a".to_string(),
            bounds: vec![],
          }
        ],
      }
    );
  }

  #[test]
  fn errors_on_invalid_predicate() {
    assert!(parse_where_clause("where T unknown").is_err());
  }

  #[test]
  fn errors_on_empty_where_clause() {
    assert!(parse_where_clause("where").is_err());
  }

  #[test]
  fn parses_trailing_comma() {
    let clause = parse_where_clause("where T: Clone,").unwrap().unwrap();
    assert_eq!(
      clause,
      WhereClause {
        predicates: vec![WherePredicate::Type {
          for_lifetimes: None,
          ty: ty("T"),
          bounds: vec![trait_bound("Clone")],
        }],
      }
    );
  }

  #[test]
  fn parses_multiple_type_bounds() {
    let clause = parse_where_clause("where T: Clone + Copy + 'a")
      .unwrap()
      .unwrap();
    assert_eq!(
      clause,
      WhereClause {
        predicates: vec![WherePredicate::Type {
          for_lifetimes: None,
          ty: ty("T"),
          bounds: vec![
            trait_bound("Clone"),
            trait_bound("Copy"),
            lifetime_bound("'a"),
          ],
        }],
      }
    );
  }

  #[test]
  fn parses_lifetime_predicate_without_bounds() {
    let clause = parse_where_clause("where 'a").unwrap().unwrap();
    assert_eq!(
      clause,
      WhereClause {
        predicates: vec![WherePredicate::Lifetime {
          lifetime: "'a".to_string(),
          bounds: vec![],
        }],
      }
    );
  }

  #[test]
  fn errors_on_empty_for_lifetimes() {
    assert!(parse_where_clause("where for<> T: Clone").is_err());
  }

  #[test]
  fn errors_on_missing_colon_after_type() {
    assert!(parse_where_clause("where T Clone").is_err());
  }

  #[test]
  fn errors_on_missing_bounds_after_colon() {
    assert!(parse_where_clause("where T:").is_err());
  }

  #[test]
  fn errors_on_double_plus_in_bounds() {
    assert!(parse_where_clause("where T: Clone + + Copy").is_err());
  }

  #[test]
  fn errors_on_trailing_plus_in_bounds() {
    assert!(parse_where_clause("where T: Clone +").is_err());
  }

  #[test]
  fn errors_on_trailing_plus_before_comma() {
    assert!(parse_where_clause("where T: Clone +, U: Copy").is_err());
  }

  #[test]
  fn errors_on_trailing_plus_before_gt() {
    assert!(parse_where_clause("where T: Clone +>").is_err());
  }

  #[test]
  fn errors_on_invalid_lifetime_bound_syntax() {
    assert!(parse_where_clause("where 'a: + 'b").is_err());
  }

  #[test]
  fn errors_on_equality_missing_rhs() {
    assert!(parse_where_clause("where T =").is_err());
  }

  #[test]
  fn errors_on_multiple_equals() {
    assert!(parse_where_clause("where T = U = V").is_err());
  }

  #[test]
  fn allows_trailing_comma_in_where_clause() {
    assert!(parse_where_clause("where T: Clone,").is_ok());
  }

  #[test]
  fn parses_trailing_comma_without_extra_predicate() {
    let clause = parse_where_clause("where T: Clone,").unwrap().unwrap();
    assert_eq!(clause.predicates.len(), 1);
  }

  #[test]
  fn parses_for_lifetimes_on_equality_predicate() {
    let clause = parse_where_clause("where for<'a> T = U").unwrap().unwrap();
    assert_eq!(
      clause,
      WhereClause {
        predicates: vec![WherePredicate::Equality {
          ty: ty("T"),
          equals: ty("U"),
        }],
      }
    );
  }

  #[test]
  fn parses_multiple_for_lifetime_predicates() {
    let clause = parse_where_clause("where for<'a> T: Clone, for<'b> U: Copy")
      .unwrap()
      .unwrap();
    assert_eq!(
      clause,
      WhereClause {
        predicates: vec![
          WherePredicate::Type {
            for_lifetimes: Some(vec!["'a".to_string()]),
            ty: ty("T"),
            bounds: vec![trait_bound("Clone")],
          },
          WherePredicate::Type {
            for_lifetimes: Some(vec!["'b".to_string()]),
            ty: ty("U"),
            bounds: vec![trait_bound("Copy")],
          },
        ],
      }
    );
  }

  #[test]
  fn errors_on_trait_bound_in_lifetime_predicate() {
    assert!(parse_where_clause("where 'a: Trait").is_err());
  }

  #[test]
  fn errors_on_lifetime_bound_in_equality_predicate() {
    assert!(parse_where_clause("where 'a = 'b").is_err());
  }

  #[test]
  fn errors_on_for_lifetime_before_lifetime_predicate() {
    assert!(parse_where_clause("where for<'a> 'b: 'a").is_err());
  }

  #[test]
  fn errors_on_comma_without_predicate() {
    assert!(parse_where_clause("where ,").is_err());
  }

  #[test]
  fn errors_on_double_comma() {
    assert!(parse_where_clause("where T: Clone,, U: Copy").is_err());
  }

  #[test]
  fn errors_on_trailing_comma_without_predicate() {
    assert!(parse_where_clause("where T: Clone, ,").is_err());
  }

  #[test]
  fn errors_on_unclosed_for_lifetimes() {
    assert!(parse_where_clause("where for<'a T: Clone").is_err());
  }

  #[test]
  fn errors_on_unexpected_gt_in_bounds() {
    assert!(parse_where_clause("where T: Clone > Copy").is_err());
  }

  #[test]
  fn errors_on_bounds_without_type() {
    assert!(parse_where_clause("where : Clone").is_err());
  }

  #[test]
  fn errors_on_equality_with_bounds() {
    assert!(parse_where_clause("where T = U: Clone").is_err());
  }
}
