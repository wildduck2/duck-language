#[cfg(test)]
mod path_tests {

  use crate::{
    ast::{
      expr::ExprKind, GenericArg, GenericArgs, Path, PathSegment, PathSegmentKind, QSelf, Type,
    },
    parser_utils::ExprContext,
    tests::support::parse_primary_expr,
  };

  // Helper function to parse a single expression from a string
  fn parse_single(input: &str) -> Result<ExprKind, ()> {
    parse_primary_expr(input, "path_test_temp", ExprContext::Default)
  }

  fn assert_path(input: &str, expected: ExprKind) {
    let result = parse_single(input).unwrap();
    assert_eq!(result, expected);
  }

  // Helper function to check if parsing produces an error
  fn assert_err(input: &str) {
    assert!(
      parse_single(input).is_err(),
      "expected error for `{}`",
      input
    );
  }

  // ============================================================================
  // Simple Path Tests
  // ============================================================================

  #[test]
  fn test_simple_ident_path() {
    assert_path(
      "foo",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::Ident("foo".to_string()),
            args: None,
          }],
        },
      },
    );
  }

  #[test]
  fn test_two_segment_path() {
    assert_path(
      "foo::bar",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Ident("foo".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("bar".to_string()),
              args: None,
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_leading_colon_two_segment_path() {
    assert_path(
      "::foo::bar",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: true,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Ident("foo".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("bar".to_string()),
              args: None,
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_three_segment_path() {
    assert_path(
      "foo::bar::baz",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Ident("foo".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("bar".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("baz".to_string()),
              args: None,
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_leading_colon_three_segment_path() {
    assert_path(
      "::foo::bar::baz",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: true,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Ident("foo".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("bar".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("baz".to_string()),
              args: None,
            },
          ],
        },
      },
    );
  }

  // ============================================================================
  // Self/Super/Crate Path Tests
  // ============================================================================

  #[test]
  fn test_self_path() {
    assert_path(
      "self",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::Self_,
            args: None,
          }],
        },
      },
    );
  }

  #[test]
  fn test_self_type_mid_path_segment() {
    assert_path(
      "foo::Self::bar",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Ident("foo".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::SelfType,
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("bar".to_string()),
              args: None,
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_super_path() {
    assert_path(
      "super",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::Super,
            args: None,
          }],
        },
      },
    );
  }

  #[test]
  fn test_crate_path() {
    assert_path(
      "crate",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::Crate,
            args: None,
          }],
        },
      },
    );
  }

  #[test]
  fn test_dollar_crate_path() {
    assert_path(
      "$crate",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::DollarCrate,
            args: None,
          }],
        },
      },
    );
  }

  #[test]
  fn test_self_with_segment() {
    assert_path(
      "self::foo",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Self_,
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("foo".to_string()),
              args: None,
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_super_with_segment() {
    assert_path(
      "super::foo",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Super,
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("foo".to_string()),
              args: None,
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_crate_with_segment() {
    assert_path(
      "crate::foo",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Crate,
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("foo".to_string()),
              args: None,
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_dollar_crate_with_segment() {
    assert_path(
      "$crate::foo",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::DollarCrate,
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("foo".to_string()),
              args: None,
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_leading_colon_dollar_crate() {
    assert_path(
      "::$crate",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: true,
          segments: vec![PathSegment {
            kind: PathSegmentKind::DollarCrate,
            args: None,
          }],
        },
      },
    );
  }

  #[test]
  fn test_self_super_crate_chain() {
    assert_path(
      "self::super::crate",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Self_,
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Super,
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Crate,
              args: None,
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_leading_colon_self_super_crate_chain() {
    assert_path(
      "::self::super::crate",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: true,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Self_,
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Super,
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Crate,
              args: None,
            },
          ],
        },
      },
    );
  }

  // ============================================================================
  // Generic Arguments Tests
  // ============================================================================

  #[test]
  fn test_path_with_single_generic_arg() {
    assert_path(
      "foo::bar::<T>",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Ident("foo".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("bar".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![GenericArg::Type(Type::Path(Path {
                  leading_colon: false,
                  segments: vec![PathSegment {
                    kind: PathSegmentKind::Ident("T".to_string()),
                    args: None,
                  }],
                }))],
              }),
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_path_with_two_generic_args() {
    assert_path(
      "foo::bar::<T, U>",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Ident("foo".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("bar".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![
                  GenericArg::Type(Type::Path(Path {
                    leading_colon: false,
                    segments: vec![PathSegment {
                      kind: PathSegmentKind::Ident("T".to_string()),
                      args: None,
                    }],
                  })),
                  GenericArg::Type(Type::Path(Path {
                    leading_colon: false,
                    segments: vec![PathSegment {
                      kind: PathSegmentKind::Ident("U".to_string()),
                      args: None,
                    }],
                  })),
                ],
              }),
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_path_with_three_generic_args() {
    assert_path(
      "foo::bar::<T, U, V>",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Ident("foo".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("bar".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![
                  GenericArg::Type(Type::Path(Path {
                    leading_colon: false,
                    segments: vec![PathSegment {
                      kind: PathSegmentKind::Ident("T".to_string()),
                      args: None,
                    }],
                  })),
                  GenericArg::Type(Type::Path(Path {
                    leading_colon: false,
                    segments: vec![PathSegment {
                      kind: PathSegmentKind::Ident("U".to_string()),
                      args: None,
                    }],
                  })),
                  GenericArg::Type(Type::Path(Path {
                    leading_colon: false,
                    segments: vec![PathSegment {
                      kind: PathSegmentKind::Ident("V".to_string()),
                      args: None,
                    }],
                  })),
                ],
              }),
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_leading_colon_path_with_generic_arg() {
    assert_path(
      "::foo::bar::<T>",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: true,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Ident("foo".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("bar".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![GenericArg::Type(Type::Path(Path {
                  leading_colon: false,
                  segments: vec![PathSegment {
                    kind: PathSegmentKind::Ident("T".to_string()),
                    args: None,
                  }],
                }))],
              }),
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_leading_colon_path_with_two_generic_args() {
    assert_path(
      "::foo::bar::<T, U>",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: true,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Ident("foo".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("bar".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![
                  GenericArg::Type(Type::Path(Path {
                    leading_colon: false,
                    segments: vec![PathSegment {
                      kind: PathSegmentKind::Ident("T".to_string()),
                      args: None,
                    }],
                  })),
                  GenericArg::Type(Type::Path(Path {
                    leading_colon: false,
                    segments: vec![PathSegment {
                      kind: PathSegmentKind::Ident("U".to_string()),
                      args: None,
                    }],
                  })),
                ],
              }),
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_path_with_generic_on_first_segment() {
    assert_path(
      "foo::<T>",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::Ident("foo".to_string()),
            args: Some(GenericArgs::AngleBracketed {
              args: vec![GenericArg::Type(Type::Path(Path {
                leading_colon: false,
                segments: vec![PathSegment {
                  kind: PathSegmentKind::Ident("T".to_string()),
                  args: None,
                }],
              }))],
            }),
          }],
        },
      },
    );
  }

  #[test]
  fn test_path_with_generic_then_segment() {
    assert_path(
      "foo::<T>::bar",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Ident("foo".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![GenericArg::Type(Type::Path(Path {
                  leading_colon: false,
                  segments: vec![PathSegment {
                    kind: PathSegmentKind::Ident("T".to_string()),
                    args: None,
                  }],
                }))],
              }),
            },
            PathSegment {
              kind: PathSegmentKind::Ident("bar".to_string()),
              args: None,
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_path_with_multiple_generic_segments() {
    assert_path(
      "foo::<T>::bar::<U>",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Ident("foo".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![GenericArg::Type(Type::Path(Path {
                  leading_colon: false,
                  segments: vec![PathSegment {
                    kind: PathSegmentKind::Ident("T".to_string()),
                    args: None,
                  }],
                }))],
              }),
            },
            PathSegment {
              kind: PathSegmentKind::Ident("bar".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![GenericArg::Type(Type::Path(Path {
                  leading_colon: false,
                  segments: vec![PathSegment {
                    kind: PathSegmentKind::Ident("U".to_string()),
                    args: None,
                  }],
                }))],
              }),
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_path_with_generic_in_middle() {
    assert_path(
      "foo::bar::<T>::baz",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Ident("foo".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("bar".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![GenericArg::Type(Type::Path(Path {
                  leading_colon: false,
                  segments: vec![PathSegment {
                    kind: PathSegmentKind::Ident("T".to_string()),
                    args: None,
                  }],
                }))],
              }),
            },
            PathSegment {
              kind: PathSegmentKind::Ident("baz".to_string()),
              args: None,
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_path_with_multiple_generics() {
    assert_path(
      "foo::bar::<T>::baz::<U>",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Ident("foo".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("bar".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![GenericArg::Type(Type::Path(Path {
                  leading_colon: false,
                  segments: vec![PathSegment {
                    kind: PathSegmentKind::Ident("T".to_string()),
                    args: None,
                  }],
                }))],
              }),
            },
            PathSegment {
              kind: PathSegmentKind::Ident("baz".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![GenericArg::Type(Type::Path(Path {
                  leading_colon: false,
                  segments: vec![PathSegment {
                    kind: PathSegmentKind::Ident("U".to_string()),
                    args: None,
                  }],
                }))],
              }),
            },
          ],
        },
      },
    );
  }

  // ============================================================================
  // SelfType Path Tests
  // ============================================================================

  #[test]
  fn test_self_type_with_generic() {
    assert_path(
      "Self::<T>",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::SelfType,
            args: Some(GenericArgs::AngleBracketed {
              args: vec![GenericArg::Type(Type::Path(Path {
                leading_colon: false,
                segments: vec![PathSegment {
                  kind: PathSegmentKind::Ident("T".to_string()),
                  args: None,
                }],
              }))],
            }),
          }],
        },
      },
    );
  }

  #[test]
  fn test_self_type_with_generic_and_assoc() {
    assert_path(
      "Self::<T>::Assoc",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::SelfType,
              args: Some(GenericArgs::AngleBracketed {
                args: vec![GenericArg::Type(Type::Path(Path {
                  leading_colon: false,
                  segments: vec![PathSegment {
                    kind: PathSegmentKind::Ident("T".to_string()),
                    args: None,
                  }],
                }))],
              }),
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Assoc".to_string()),
              args: None,
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_self_type_with_generic_and_nested_assoc() {
    assert_path(
      "Self::<T>::Assoc::<U>",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::SelfType,
              args: Some(GenericArgs::AngleBracketed {
                args: vec![GenericArg::Type(Type::Path(Path {
                  leading_colon: false,
                  segments: vec![PathSegment {
                    kind: PathSegmentKind::Ident("T".to_string()),
                    args: None,
                  }],
                }))],
              }),
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Assoc".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![GenericArg::Type(Type::Path(Path {
                  leading_colon: false,
                  segments: vec![PathSegment {
                    kind: PathSegmentKind::Ident("U".to_string()),
                    args: None,
                  }],
                }))],
              }),
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_crate_type_with_generics() {
    assert_path(
      "crate::Type::<T>::Assoc::<U>",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Crate,
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Type".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![GenericArg::Type(Type::Path(Path {
                  leading_colon: false,
                  segments: vec![PathSegment {
                    kind: PathSegmentKind::Ident("T".to_string()),
                    args: None,
                  }],
                }))],
              }),
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Assoc".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![GenericArg::Type(Type::Path(Path {
                  leading_colon: false,
                  segments: vec![PathSegment {
                    kind: PathSegmentKind::Ident("U".to_string()),
                    args: None,
                  }],
                }))],
              }),
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_self_with_type() {
    assert_path(
      "self::Type",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Self_,
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Type".to_string()),
              args: None,
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_super_with_type() {
    assert_path(
      "super::Type",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Super,
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Type".to_string()),
              args: None,
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_crate_with_type() {
    assert_path(
      "crate::Type",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Crate,
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Type".to_string()),
              args: None,
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_dollar_crate_with_type() {
    assert_path(
      "$crate::Type",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::DollarCrate,
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Type".to_string()),
              args: None,
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_self_type_with_generic_type() {
    assert_path(
      "self::Type::<T>",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Self_,
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Type".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![GenericArg::Type(Type::Path(Path {
                  leading_colon: false,
                  segments: vec![PathSegment {
                    kind: PathSegmentKind::Ident("T".to_string()),
                    args: None,
                  }],
                }))],
              }),
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_super_type_with_generic_and_assoc() {
    assert_path(
      "super::Type::<T>::Assoc",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Super,
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Type".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![GenericArg::Type(Type::Path(Path {
                  leading_colon: false,
                  segments: vec![PathSegment {
                    kind: PathSegmentKind::Ident("T".to_string()),
                    args: None,
                  }],
                }))],
              }),
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Assoc".to_string()),
              args: None,
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_self_type_assoc() {
    assert_path(
      "Self::Assoc",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::SelfType,
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Assoc".to_string()),
              args: None,
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_self_type_assoc_with_generic() {
    assert_path(
      "Self::Assoc::<T>",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::SelfType,
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Assoc".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![GenericArg::Type(Type::Path(Path {
                  leading_colon: false,
                  segments: vec![PathSegment {
                    kind: PathSegmentKind::Ident("T".to_string()),
                    args: None,
                  }],
                }))],
              }),
            },
          ],
        },
      },
    );
  }

  // ============================================================================
  // Qualified Path Tests (QSelf)
  // ============================================================================

  #[test]
  fn test_qualified_path_simple() {
    assert_path(
      "<T>::Item",
      ExprKind::Path {
        qself: Some(QSelf {
          self_ty: Box::new(Type::Path(Path {
            leading_colon: false,
            segments: vec![PathSegment {
              kind: PathSegmentKind::Ident("T".to_string()),
              args: None,
            }],
          })),
          as_trait: None,
        }),
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::Ident("Item".to_string()),
            args: None,
          }],
        },
      },
    );
  }

  #[test]
  fn test_qualified_path_with_generic() {
    assert_path(
      "<T>::Item::<U>",
      ExprKind::Path {
        qself: Some(QSelf {
          self_ty: Box::new(Type::Path(Path {
            leading_colon: false,
            segments: vec![PathSegment {
              kind: PathSegmentKind::Ident("T".to_string()),
              args: None,
            }],
          })),
          as_trait: None,
        }),
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::Ident("Item".to_string()),
            args: Some(GenericArgs::AngleBracketed {
              args: vec![GenericArg::Type(Type::Path(Path {
                leading_colon: false,
                segments: vec![PathSegment {
                  kind: PathSegmentKind::Ident("U".to_string()),
                  args: None,
                }],
              }))],
            }),
          }],
        },
      },
    );
  }

  #[test]
  fn test_qualified_path_with_self_type() {
    assert_path(
      "<Self>::Assoc",
      ExprKind::Path {
        qself: Some(QSelf {
          self_ty: Box::new(Type::SelfType),
          as_trait: None,
        }),
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::Ident("Assoc".to_string()),
            args: None,
          }],
        },
      },
    );
  }

  #[test]
  fn test_qualified_path_with_dollar_crate_type() {
    assert_path(
      "<$crate::Type>::Assoc",
      ExprKind::Path {
        qself: Some(QSelf {
          self_ty: Box::new(Type::Path(Path {
            leading_colon: false,
            segments: vec![
              PathSegment {
                kind: PathSegmentKind::DollarCrate,
                args: None,
              },
              PathSegment {
                kind: PathSegmentKind::Ident("Type".to_string()),
                args: None,
              },
            ],
          })),
          as_trait: None,
        }),
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::Ident("Assoc".to_string()),
            args: None,
          }],
        },
      },
    );
  }

  #[test]
  fn test_qualified_path_with_trait() {
    assert_path(
      "<crate::Type as Trait>::Item",
      ExprKind::Path {
        qself: Some(QSelf {
          self_ty: Box::new(Type::Path(Path {
            leading_colon: false,
            segments: vec![
              PathSegment {
                kind: PathSegmentKind::Crate,
                args: None,
              },
              PathSegment {
                kind: PathSegmentKind::Ident("Type".to_string()),
                args: None,
              },
            ],
          })),
          as_trait: Some(Path {
            leading_colon: false,
            segments: vec![PathSegment {
              kind: PathSegmentKind::Ident("Trait".to_string()),
              args: None,
            }],
          }),
        }),
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::Ident("Item".to_string()),
            args: None,
          }],
        },
      },
    );
  }

  #[test]
  fn test_qualified_path_with_trait_and_generic() {
    assert_path(
      "<crate::Type as Trait>::Item::<U>",
      ExprKind::Path {
        qself: Some(QSelf {
          self_ty: Box::new(Type::Path(Path {
            leading_colon: false,
            segments: vec![
              PathSegment {
                kind: PathSegmentKind::Crate,
                args: None,
              },
              PathSegment {
                kind: PathSegmentKind::Ident("Type".to_string()),
                args: None,
              },
            ],
          })),
          as_trait: Some(Path {
            leading_colon: false,
            segments: vec![PathSegment {
              kind: PathSegmentKind::Ident("Trait".to_string()),
              args: None,
            }],
          }),
        }),
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::Ident("Item".to_string()),
            args: Some(GenericArgs::AngleBracketed {
              args: vec![GenericArg::Type(Type::Path(Path {
                leading_colon: false,
                segments: vec![PathSegment {
                  kind: PathSegmentKind::Ident("U".to_string()),
                  args: None,
                }],
              }))],
            }),
          }],
        },
      },
    );
  }

  #[test]
  fn test_qualified_path_with_trait_and_sub_item() {
    assert_path(
      "<super::Type as Trait>::Item::Sub",
      ExprKind::Path {
        qself: Some(QSelf {
          self_ty: Box::new(Type::Path(Path {
            leading_colon: false,
            segments: vec![
              PathSegment {
                kind: PathSegmentKind::Super,
                args: None,
              },
              PathSegment {
                kind: PathSegmentKind::Ident("Type".to_string()),
                args: None,
              },
            ],
          })),
          as_trait: Some(Path {
            leading_colon: false,
            segments: vec![PathSegment {
              kind: PathSegmentKind::Ident("Trait".to_string()),
              args: None,
            }],
          }),
        }),
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Ident("Item".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Sub".to_string()),
              args: None,
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_qualified_path_with_self_trait() {
    assert_path(
      "<Self as Trait>::Item::Assoc::Deep",
      ExprKind::Path {
        qself: Some(QSelf {
          self_ty: Box::new(Type::SelfType),
          as_trait: Some(Path {
            leading_colon: false,
            segments: vec![PathSegment {
              kind: PathSegmentKind::Ident("Trait".to_string()),
              args: None,
            }],
          }),
        }),
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Ident("Item".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Assoc".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Deep".to_string()),
              args: None,
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_qualified_path_complex() {
    assert_path(
      "<crate::Type as Trait>::Item::Assoc::Deep::<U>",
      ExprKind::Path {
        qself: Some(QSelf {
          self_ty: Box::new(Type::Path(Path {
            leading_colon: false,
            segments: vec![
              PathSegment {
                kind: PathSegmentKind::Crate,
                args: None,
              },
              PathSegment {
                kind: PathSegmentKind::Ident("Type".to_string()),
                args: None,
              },
            ],
          })),
          as_trait: Some(Path {
            leading_colon: false,
            segments: vec![PathSegment {
              kind: PathSegmentKind::Ident("Trait".to_string()),
              args: None,
            }],
          }),
        }),
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Ident("Item".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Assoc".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Deep".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![GenericArg::Type(Type::Path(Path {
                  leading_colon: false,
                  segments: vec![PathSegment {
                    kind: PathSegmentKind::Ident("U".to_string()),
                    args: None,
                  }],
                }))],
              }),
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_qualified_path_with_leading_colon() {
    assert_path(
      "<::foo as Trait>::Item",
      ExprKind::Path {
        qself: Some(QSelf {
          self_ty: Box::new(Type::Path(Path {
            leading_colon: true,
            segments: vec![PathSegment {
              kind: PathSegmentKind::Ident("foo".to_string()),
              args: None,
            }],
          })),
          as_trait: Some(Path {
            leading_colon: false,
            segments: vec![PathSegment {
              kind: PathSegmentKind::Ident("Trait".to_string()),
              args: None,
            }],
          }),
        }),
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::Ident("Item".to_string()),
            args: None,
          }],
        },
      },
    );
  }

  #[test]
  fn test_qualified_path_with_leading_colon_multi_segment() {
    assert_path(
      "<::foo::bar as Trait>::Item",
      ExprKind::Path {
        qself: Some(QSelf {
          self_ty: Box::new(Type::Path(Path {
            leading_colon: true,
            segments: vec![
              PathSegment {
                kind: PathSegmentKind::Ident("foo".to_string()),
                args: None,
              },
              PathSegment {
                kind: PathSegmentKind::Ident("bar".to_string()),
                args: None,
              },
            ],
          })),
          as_trait: Some(Path {
            leading_colon: false,
            segments: vec![PathSegment {
              kind: PathSegmentKind::Ident("Trait".to_string()),
              args: None,
            }],
          }),
        }),
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::Ident("Item".to_string()),
            args: None,
          }],
        },
      },
    );
  }

  #[test]
  fn test_qualified_path_with_leading_colon_crate() {
    assert_path(
      "<::crate::Type as Trait>::Assoc",
      ExprKind::Path {
        qself: Some(QSelf {
          self_ty: Box::new(Type::Path(Path {
            leading_colon: true,
            segments: vec![
              PathSegment {
                kind: PathSegmentKind::Crate,
                args: None,
              },
              PathSegment {
                kind: PathSegmentKind::Ident("Type".to_string()),
                args: None,
              },
            ],
          })),
          as_trait: Some(Path {
            leading_colon: false,
            segments: vec![PathSegment {
              kind: PathSegmentKind::Ident("Trait".to_string()),
              args: None,
            }],
          }),
        }),
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::Ident("Assoc".to_string()),
            args: None,
          }],
        },
      },
    );
  }

  #[test]
  fn test_qualified_path_with_leading_colon_super() {
    assert_path(
      "<::super::Type as Trait>::Assoc::<T>",
      ExprKind::Path {
        qself: Some(QSelf {
          self_ty: Box::new(Type::Path(Path {
            leading_colon: true,
            segments: vec![
              PathSegment {
                kind: PathSegmentKind::Super,
                args: None,
              },
              PathSegment {
                kind: PathSegmentKind::Ident("Type".to_string()),
                args: None,
              },
            ],
          })),
          as_trait: Some(Path {
            leading_colon: false,
            segments: vec![PathSegment {
              kind: PathSegmentKind::Ident("Trait".to_string()),
              args: None,
            }],
          }),
        }),
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::Ident("Assoc".to_string()),
            args: Some(GenericArgs::AngleBracketed {
              args: vec![GenericArg::Type(Type::Path(Path {
                leading_colon: false,
                segments: vec![PathSegment {
                  kind: PathSegmentKind::Ident("T".to_string()),
                  args: None,
                }],
              }))],
            }),
          }],
        },
      },
    );
  }

  #[test]
  fn test_leading_colon_path_with_generics() {
    assert_path(
      "::foo::<T>::bar::<U>::baz",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: true,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Ident("foo".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![GenericArg::Type(Type::Path(Path {
                  leading_colon: false,
                  segments: vec![PathSegment {
                    kind: PathSegmentKind::Ident("T".to_string()),
                    args: None,
                  }],
                }))],
              }),
            },
            PathSegment {
              kind: PathSegmentKind::Ident("bar".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![GenericArg::Type(Type::Path(Path {
                  leading_colon: false,
                  segments: vec![PathSegment {
                    kind: PathSegmentKind::Ident("U".to_string()),
                    args: None,
                  }],
                }))],
              }),
            },
            PathSegment {
              kind: PathSegmentKind::Ident("baz".to_string()),
              args: None,
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_qualified_path_deep_nesting() {
    assert_path(
      "<T>::Item::Assoc::Deep::<U>",
      ExprKind::Path {
        qself: Some(QSelf {
          self_ty: Box::new(Type::Path(Path {
            leading_colon: false,
            segments: vec![PathSegment {
              kind: PathSegmentKind::Ident("T".to_string()),
              args: None,
            }],
          })),
          as_trait: None,
        }),
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Ident("Item".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Assoc".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Deep".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![GenericArg::Type(Type::Path(Path {
                  leading_colon: false,
                  segments: vec![PathSegment {
                    kind: PathSegmentKind::Ident("U".to_string()),
                    args: None,
                  }],
                }))],
              }),
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_qualified_path_self_deep() {
    assert_path(
      "<Self>::Assoc::Deep::Deeper",
      ExprKind::Path {
        qself: Some(QSelf {
          self_ty: Box::new(Type::SelfType),
          as_trait: None,
        }),
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Ident("Assoc".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Deep".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Deeper".to_string()),
              args: None,
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_qualified_path_complex_deep() {
    assert_path(
      "<crate::Type as Trait>::Item::Assoc::Deep::Deeper::<U>",
      ExprKind::Path {
        qself: Some(QSelf {
          self_ty: Box::new(Type::Path(Path {
            leading_colon: false,
            segments: vec![
              PathSegment {
                kind: PathSegmentKind::Crate,
                args: None,
              },
              PathSegment {
                kind: PathSegmentKind::Ident("Type".to_string()),
                args: None,
              },
            ],
          })),
          as_trait: Some(Path {
            leading_colon: false,
            segments: vec![PathSegment {
              kind: PathSegmentKind::Ident("Trait".to_string()),
              args: None,
            }],
          }),
        }),
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Ident("Item".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Assoc".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Deep".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("Deeper".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![GenericArg::Type(Type::Path(Path {
                  leading_colon: false,
                  segments: vec![PathSegment {
                    kind: PathSegmentKind::Ident("U".to_string()),
                    args: None,
                  }],
                }))],
              }),
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_path_with_lifetime_generic() {
    assert_path(
      "foo::bar::<'a>",
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![
            PathSegment {
              kind: PathSegmentKind::Ident("foo".to_string()),
              args: None,
            },
            PathSegment {
              kind: PathSegmentKind::Ident("bar".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![GenericArg::Lifetime("'a".to_string())],
              }),
            },
          ],
        },
      },
    );
  }

  #[test]
  fn test_qualified_path_with_lifetime_generic() {
    assert_path(
      "<T>::Item::<'a>",
      ExprKind::Path {
        qself: Some(QSelf {
          self_ty: Box::new(Type::Path(Path {
            leading_colon: false,
            segments: vec![PathSegment {
              kind: PathSegmentKind::Ident("T".to_string()),
              args: None,
            }],
          })),
          as_trait: None,
        }),
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::Ident("Item".to_string()),
            args: Some(GenericArgs::AngleBracketed {
              args: vec![GenericArg::Lifetime("'a".to_string())],
            }),
          }],
        },
      },
    );
  }

  #[test]
  fn test_qself_as_trait_with_lifetime_generic() {
    assert_path(
      "<crate::Type as Trait<'a>>::Item",
      ExprKind::Path {
        qself: Some(QSelf {
          self_ty: Box::new(Type::Path(Path {
            leading_colon: false,
            segments: vec![
              PathSegment {
                kind: PathSegmentKind::Crate,
                args: None,
              },
              PathSegment {
                kind: PathSegmentKind::Ident("Type".to_string()),
                args: None,
              },
            ],
          })),
          as_trait: Some(Path {
            leading_colon: false,
            segments: vec![PathSegment {
              kind: PathSegmentKind::Ident("Trait".to_string()),
              args: Some(GenericArgs::AngleBracketed {
                args: vec![GenericArg::Lifetime("'a".to_string())],
              }),
            }],
          }),
        }),
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::Ident("Item".to_string()),
            args: None,
          }],
        },
      },
    );
  }

  // ============================================================================
  // Error/Invalid Syntax Tests
  // ============================================================================

  #[test]
  fn test_invalid_lifetime_generics_should_error() {
    let cases = [
      "foo::bar::<'a,>",
      "foo::bar::<,'a>",
      "foo::bar::<'a 'b>",
      "<T>::Item::<'a,>",
    ];

    for src in cases {
      assert_err(src);
    }
  }

  #[test]
  fn test_empty_or_malformed_generic_args_should_error() {
    let cases = [
      "foo::<>",
      "foo::<>::bar",
      "foo::bar::<>",
      "foo::<,>",
      "foo::<T,>",
      "foo::<,T>",
      "foo::<T,,U>",
      "foo::<T U>",
      "foo::<T;>",
      "foo::<T as>",
    ];

    for src in cases {
      assert_err(src);
    }
  }

  #[test]
  fn test_missing_path_segment_after_coloncolon_should_error() {
    let cases = [
      "foo::",
      "::foo::",
      "<T>::",
      "<T>:::",
      "<T as Trait>::",
      "<T as Trait>:::",
    ];

    for src in cases {
      assert_err(src);
    }
  }

  #[test]
  fn test_malformed_qualified_path_header_should_error() {
    let cases = [
      "<>",
      "Item<>",
      "<>::Item",
      "<T as>::Item",
      "<T Trait>::Item",
      "<T as Trait Item>::Assoc",
      "<T as Trait as Other>::Item",
      "<T as>",
      "<T as Trait>",
      "<T as Trait>::",
    ];

    for src in cases {
      assert_err(src);
    }
  }

  #[test]
  fn test_qualified_path_without_trailing_segments_should_error() {
    let cases = [
      "<T>",
      "<T as Trait>",
      "<Self>",
      "<Self as Trait>",
      "<crate::Type>",
      "<crate::Type as Trait>",
    ];

    for src in cases {
      assert_err(src);
    }
  }

  #[test]
  fn test_illegal_qself_placement_should_error() {
    let cases = [
      "foo::<T>::<U>::bar",
      "foo::<T>::<U as Trait>::bar",
      "foo::<T as Trait>::bar",
      "foo::bar::<T as Trait>",
    ];

    for src in cases {
      assert_err(src);
    }
  }

  #[test]
  fn test_invalid_or_empty_path_segments_should_error() {
    let cases = [
      "::",
      ":::",
      "foo::::bar",
      "foo::123",
      "foo::true",
      "foo::false",
      "foo::as",
      "foo::type",
    ];

    for src in cases {
      assert_err(src);
    }
  }

  #[test]
  fn test_invalid_tokens_inside_qself_type_should_error() {
    let cases = [
      "<(foo)>::Item",
      "<[T]>::Item",
      "<{T}>::Item",
      "<(T,)>::Item",
      "<fn()>::Item",
      "<impl Trait>::Item",
    ];

    for src in cases {
      assert_err(src);
    }
  }

  #[test]
  fn test_incomplete_binary_expressions_with_qpath_should_error() {
    let cases = [
      "<T>:: <U>::Item",
      "<T>::Item <",
      "<T>::Item <>",
      "<T>::Item < ::foo",
      "<T>::Item < T",
    ];

    for src in cases {
      assert_err(src);
    }
  }

  #[test]
  fn test_invalid_dollar_crate_usage_should_error() {
    let cases = ["foo::$crate", "foo::bar::$crate", "<T as $crate>::Item"];

    for src in cases {
      assert_err(src);
    }
  }

  #[test]
  fn test_mixed_known_failure_cases_should_error() {
    let cases = [
      "foo::<>::bar",
      "foo::<> <T as>::Item",
      "<>::Item <T>::",
      "<T as>::Item",
      "<T Trait>::Item",
    ];

    for src in cases {
      assert_err(src);
    }
  }
}
