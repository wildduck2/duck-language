#[cfg(test)]
mod path_tests {

  use crate::{
    ast::{
      expr::ExprKind, GenericArg, GenericArgs, Path, PathSegment, PathSegmentKind, QSelf, Type,
    },
    parser_utils::ExprContext,
    tests::prepare,
  };

  #[test]
  fn literal_expressions_cover_all_variants() {
    let (mut engine, mut parser) = prepare("./tests/files/path.lox").unwrap();

    let mut ast = vec![];
    while !parser.is_eof() {
      match parser.parse_primary(ExprContext::Default, &mut engine) {
        Ok(item) => {
          println!("{:#?}", item.kind);
          ast.push(item);
        },
        Err(_) => parser.synchronize(&mut engine),
      }
    }

    let cases = [
      (
        0,
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
      ),
      (
        2,
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
      ),
      (
        3,
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
      ),
      (
        4,
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
      ),
      (
        5,
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
      ),
      (
        6,
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
      ),
      (
        7,
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
      ),
      (
        8,
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
      ),
      (
        9,
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
      ),
      (
        10,
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
      ),
      (
        11,
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
      ),
      (
        12,
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
      ),
      (
        13,
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
      ),
      (
        14,
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
      ),
      (
        15,
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
      ),
      (
        16,
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
      ),
      (
        17,
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
      ),
      (
        18,
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
      ),
      (
        19,
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
      ),
      (
        20,
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
      ),
      (
        21,
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
      ),
      (
        22,
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
      ),
      (
        23,
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
      ),
      (
        24,
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
      ),
      (
        25,
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
      ),
      (
        26,
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
      ),
      (
        27,
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
      ),
      (
        28,
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
      ),
      (
        29,
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
      ),
      (
        30,
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
      ),
      (
        31,
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
      ),
      (
        32,
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
      ),
      (
        33,
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
      ),
      (
        34,
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
      ),
      (
        35,
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
      ),
      (
        36,
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
      ),
      (
        37,
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
      ),
      (
        38,
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
      ),
      (
        39,
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
      ),
      (
        40,
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
      ),
      (
        41,
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
      ),
      (
        42,
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
      ),
      (
        43,
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
      ),
      (
        44,
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
      ),
      (
        45,
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
      ),
      (
        46,
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
      ),
      (
        47,
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
      ),
      (
        48,
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
      ),
      (
        49,
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
      ),
      (
        50,
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
      ),
      (
        51,
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
      ),
      (
        52,
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
      ),
      (
        53,
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
      ),
      (
        54,
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
      ),
      (
        55,
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
      ),
    ];

    for case in cases {
      let (index, kind) = case;
      assert_eq!(ast[index].kind, kind);
    }
  }
}
