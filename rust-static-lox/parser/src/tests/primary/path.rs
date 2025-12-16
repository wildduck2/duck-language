#[cfg(test)]
mod path_tests {

  use crate::{
    ast::{
      expr::ExprKind, GenericArg, GenericArgs, Path, PathSegment, PathSegmentKind, QSelf, Type,
    },
    parser_utils::ExprContext,
    tests::prepare,
  };

  // Helper function to parse a single expression from a string
  fn parse_single(input: &str) -> Result<ExprKind, ()> {
    use diagnostic::{DiagnosticEngine, SourceFile, SourceMap};
    use lexer::Lexer;
    use std::path::PathBuf;
    
    let mut engine = DiagnosticEngine::new();
    let mut source_map = SourceMap::new();
    
    // Create a source file directly from the input string
    let test_file_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests")
      .join("files")
      .join("path_test_temp.lox");
    let path_str = test_file_path.to_str().unwrap().to_string();
    
    let source_file = SourceFile::new(path_str.clone(), input.to_string());
    source_map.add_file(&path_str, input);
    engine.add_file(&path_str, input);
    
    let mut lexer = Lexer::new(source_file.clone());
    lexer.scan_tokens(&mut engine);
    
    if engine.has_errors() {
      return Err(());
    }
    
    let mut parser = crate::Parser::new(lexer.tokens, source_file);
    parser.parse_primary(ExprContext::Default, &mut engine).map(|expr| expr.kind)
  }

  // Helper function to check if parsing produces an error
  fn should_error(input: &str) -> bool {
    parse_single(input).is_err()
  }

  // ============================================================================
  // Simple Path Tests
  // ============================================================================

  #[test]
  fn test_simple_ident_path() {
    let result = parse_single("foo").unwrap();
    assert_eq!(
      result,
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::Ident("foo".to_string()),
            args: None,
          }],
        },
      }
    );
  }

  #[test]
  fn test_two_segment_path() {
    let result = parse_single("foo::bar").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_leading_colon_two_segment_path() {
    let result = parse_single("::foo::bar").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_three_segment_path() {
    let result = parse_single("foo::bar::baz").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_leading_colon_three_segment_path() {
    let result = parse_single("::foo::bar::baz").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  // ============================================================================
  // Self/Super/Crate Path Tests
  // ============================================================================

  #[test]
  fn test_self_path() {
    let result = parse_single("self").unwrap();
    assert_eq!(
      result,
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::Self_,
            args: None,
          }],
        },
      }
    );
  }

  #[test]
  fn test_super_path() {
    let result = parse_single("super").unwrap();
    assert_eq!(
      result,
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::Super,
            args: None,
          }],
        },
      }
    );
  }

  #[test]
  fn test_crate_path() {
    let result = parse_single("crate").unwrap();
    assert_eq!(
      result,
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::Crate,
            args: None,
          }],
        },
      }
    );
  }

  #[test]
  fn test_dollar_crate_path() {
    let result = parse_single("$crate").unwrap();
    assert_eq!(
      result,
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::DollarCrate,
            args: None,
          }],
        },
      }
    );
  }

  #[test]
  fn test_self_with_segment() {
    let result = parse_single("self::foo").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_super_with_segment() {
    let result = parse_single("super::foo").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_crate_with_segment() {
    let result = parse_single("crate::foo").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_dollar_crate_with_segment() {
    let result = parse_single("$crate::foo").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_leading_colon_dollar_crate() {
    let result = parse_single("::$crate").unwrap();
    assert_eq!(
      result,
      ExprKind::Path {
        qself: None,
        path: Path {
          leading_colon: true,
          segments: vec![PathSegment {
            kind: PathSegmentKind::DollarCrate,
            args: None,
          }],
        },
      }
    );
  }

  #[test]
  fn test_self_super_crate_chain() {
    let result = parse_single("self::super::crate").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_leading_colon_self_super_crate_chain() {
    let result = parse_single("::self::super::crate").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  // ============================================================================
  // Generic Arguments Tests
  // ============================================================================

  #[test]
  fn test_path_with_single_generic_arg() {
    let result = parse_single("foo::bar::<T>").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_path_with_two_generic_args() {
    let result = parse_single("foo::bar::<T, U>").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_path_with_three_generic_args() {
    let result = parse_single("foo::bar::<T, U, V>").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_leading_colon_path_with_generic_arg() {
    let result = parse_single("::foo::bar::<T>").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_leading_colon_path_with_two_generic_args() {
    let result = parse_single("::foo::bar::<T, U>").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_path_with_generic_on_first_segment() {
    let result = parse_single("foo::<T>").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_path_with_generic_then_segment() {
    let result = parse_single("foo::<T>::bar").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_path_with_multiple_generic_segments() {
    let result = parse_single("foo::<T>::bar::<U>").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_path_with_generic_in_middle() {
    let result = parse_single("foo::bar::<T>::baz").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_path_with_multiple_generics() {
    let result = parse_single("foo::bar::<T>::baz::<U>").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  // ============================================================================
  // SelfType Path Tests
  // ============================================================================

  #[test]
  fn test_self_type_with_generic() {
    let result = parse_single("Self::<T>").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_self_type_with_generic_and_assoc() {
    let result = parse_single("Self::<T>::Assoc").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_self_type_with_generic_and_nested_assoc() {
    let result = parse_single("Self::<T>::Assoc::<U>").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_crate_type_with_generics() {
    let result = parse_single("crate::Type::<T>::Assoc::<U>").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_self_with_type() {
    let result = parse_single("self::Type").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_super_with_type() {
    let result = parse_single("super::Type").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_crate_with_type() {
    let result = parse_single("crate::Type").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_dollar_crate_with_type() {
    let result = parse_single("$crate::Type").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_self_type_with_generic_type() {
    let result = parse_single("self::Type::<T>").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_super_type_with_generic_and_assoc() {
    let result = parse_single("super::Type::<T>::Assoc").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_self_type_assoc() {
    let result = parse_single("Self::Assoc").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_self_type_assoc_with_generic() {
    let result = parse_single("Self::Assoc::<T>").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  // ============================================================================
  // Qualified Path Tests (QSelf)
  // ============================================================================

  #[test]
  fn test_qualified_path_simple() {
    let result = parse_single("<T>::Item").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_qualified_path_with_generic() {
    let result = parse_single("<T>::Item::<U>").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_qualified_path_with_self_type() {
    let result = parse_single("<Self>::Assoc").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_qualified_path_with_dollar_crate_type() {
    let result = parse_single("<$crate::Type>::Assoc").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_qualified_path_with_trait() {
    let result = parse_single("<crate::Type as Trait>::Item").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_qualified_path_with_trait_and_generic() {
    let result = parse_single("<crate::Type as Trait>::Item::<U>").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_qualified_path_with_trait_and_sub_item() {
    let result = parse_single("<super::Type as Trait>::Item::Sub").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_qualified_path_with_self_trait() {
    let result = parse_single("<Self as Trait>::Item::Assoc::Deep").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_qualified_path_complex() {
    let result = parse_single("<crate::Type as Trait>::Item::Assoc::Deep::<U>").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_qualified_path_with_leading_colon() {
    let result = parse_single("<::foo as Trait>::Item").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_qualified_path_with_leading_colon_multi_segment() {
    let result = parse_single("<::foo::bar as Trait>::Item").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_qualified_path_with_leading_colon_crate() {
    let result = parse_single("<::crate::Type as Trait>::Assoc").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_qualified_path_with_leading_colon_super() {
    let result = parse_single("<::super::Type as Trait>::Assoc::<T>").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_leading_colon_path_with_generics() {
    let result = parse_single("::foo::<T>::bar::<U>::baz").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_qualified_path_deep_nesting() {
    let result = parse_single("<T>::Item::Assoc::Deep::<U>").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_qualified_path_self_deep() {
    let result = parse_single("<Self>::Assoc::Deep::Deeper").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  #[test]
  fn test_qualified_path_complex_deep() {
    let result = parse_single("<crate::Type as Trait>::Item::Assoc::Deep::Deeper::<U>").unwrap();
    assert_eq!(
      result,
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
      }
    );
  }

  // ============================================================================
  // Error/Invalid Syntax Tests
  // ============================================================================
  // TODO: Add tests for invalid syntax that should produce errors
  // Examples from path.lox lines 57-59:
  // - foo::<>::bar
  // - foo::<> <T as>::Item <T Trait>::Item
  // - <>::Item <T>::;
  //
  // Add your error test cases here:
  //
  // #[test]
  // fn test_empty_generic_args_should_error() {
  //   assert!(should_error("foo::<>::bar"));
  // }
  //
  // #[test]
  // fn test_malformed_qualified_path_should_error() {
  //   assert!(should_error("foo::<> <T as>::Item"));
  // }
  //
  // #[test]
  // fn test_incomplete_path_should_error() {
  //   assert!(should_error("<>::Item <T>::"));
  // }
  //
  // Add more error test cases as needed...
}
