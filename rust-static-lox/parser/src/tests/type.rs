#[cfg(test)]
mod type_tests {
  use crate::{
    ast::{
      expr::ExprKind,
      generic::{GenericArg, GenericArgs},
      path::{Path, PathSegmentKind},
      Item, Lit, Mutability, Safety, Type, TypeAliasDecl, VisItemKind,
    },
    parser_utils::ParserContext,
    tests::support::{parse_item, run_parser},
  };

  fn parse_type(input: &str) -> Result<Type, ()> {
    run_parser(input, "type_test_temp", |parser| {
      parser.parse_type(ParserContext::Type)
    })
  }

  fn assert_type_err(input: &str) {
    assert!(parse_type(input).is_err(), "expected error for {input:?}");
  }

  fn assert_ident_path(path: &Path, expected: &str) {
    assert!(!path.leading_colon);
    assert_eq!(path.segments.len(), 1);
    match &path.segments[0].kind {
      PathSegmentKind::Ident(name) => assert_eq!(name, expected),
      other => panic!("expected ident path, got: {:?}", other),
    }
  }

  fn assert_path_segment_name(path: &Path, index: usize, expected: &str) {
    match &path.segments[index].kind {
      PathSegmentKind::Ident(name) => assert_eq!(name, expected),
      other => panic!("expected ident path segment, got: {:?}", other),
    }
  }

  #[test]
  fn parses_primitive_types() {
    assert_eq!(parse_type("i32").unwrap(), Type::I32);
    assert_eq!(parse_type("u8").unwrap(), Type::U8);
    assert_eq!(parse_type("bool").unwrap(), Type::Bool);
    assert_eq!(parse_type("char").unwrap(), Type::Char);
    assert_eq!(parse_type("str").unwrap(), Type::Str);
  }

  #[test]
  fn parses_more_primitive_types() {
    assert_eq!(parse_type("u16").unwrap(), Type::U16);
    assert_eq!(parse_type("u128").unwrap(), Type::U128);
    assert_eq!(parse_type("i8").unwrap(), Type::I8);
    assert_eq!(parse_type("i16").unwrap(), Type::I16);
    assert_eq!(parse_type("i128").unwrap(), Type::I128);
    assert_eq!(parse_type("isize").unwrap(), Type::Isize);
    assert_eq!(parse_type("f32").unwrap(), Type::F32);
    assert_eq!(parse_type("f64").unwrap(), Type::F64);
  }

  #[test]
  fn parses_infer_type() {
    assert_eq!(parse_type("_").unwrap(), Type::Infer);
  }

  #[test]
  fn parses_self_type() {
    assert_eq!(parse_type("Self").unwrap(), Type::SelfType);
  }

  #[test]
  fn parses_path_type() {
    match parse_type("Foo").unwrap() {
      Type::Path(path) => assert_ident_path(&path, "Foo"),
      other => panic!("expected path type, got: {:?}", other),
    }
  }

  #[test]
  fn parses_leading_colon_path_type() {
    match parse_type("::foo::bar").unwrap() {
      Type::Path(path) => {
        assert!(path.leading_colon);
        assert_eq!(path.segments.len(), 2);
        assert_path_segment_name(&path, 0, "foo");
        assert_path_segment_name(&path, 1, "bar");
      },
      other => panic!("expected path type, got: {:?}", other),
    }
  }

  #[test]
  fn parses_tuple_and_unit_types() {
    match parse_type("(i32, u8)").unwrap() {
      Type::Tuple(types) => assert_eq!(types, vec![Type::I32, Type::U8]),
      other => panic!("expected tuple type, got: {:?}", other),
    }

    assert_eq!(parse_type("()").unwrap(), Type::Unit);
  }

  #[test]
  fn parses_single_element_tuple_type() {
    match parse_type("(i32,)").unwrap() {
      Type::Tuple(types) => assert_eq!(types, vec![Type::I32]),
      other => panic!("expected tuple type, got: {:?}", other),
    }
  }

  #[test]
  fn parses_array_and_slice_types() {
    match parse_type("[u8; 3]").unwrap() {
      Type::Array { element, size } => {
        assert_eq!(*element, Type::U8);
        match &size.kind {
          ExprKind::Literal(Lit::Integer { value, .. }) => assert_eq!(*value, 3),
          other => panic!("expected integer size expr, got: {:?}", other),
        }
      },
      other => panic!("expected array type, got: {:?}", other),
    }

    match parse_type("[u8]").unwrap() {
      Type::Slice(inner) => assert_eq!(*inner, Type::U8),
      other => panic!("expected slice type, got: {:?}", other),
    }
  }

  #[test]
  fn parses_reference_types() {
    match parse_type("&T").unwrap() {
      Type::Reference {
        lifetime,
        mutability,
        inner,
      } => {
        assert_eq!(lifetime, None);
        assert_eq!(mutability, Mutability::Immutable);
        match inner.as_ref() {
          Type::Path(path) => assert_ident_path(path, "T"),
          other => panic!("expected inner path type, got: {:?}", other),
        }
      },
      other => panic!("expected reference type, got: {:?}", other),
    }

    match parse_type("&'a mut T").unwrap() {
      Type::Reference {
        lifetime,
        mutability,
        inner,
      } => {
        assert_eq!(lifetime, Some("'a".to_string()));
        assert_eq!(mutability, Mutability::Mutable);
        match inner.as_ref() {
          Type::Path(path) => assert_ident_path(path, "T"),
          other => panic!("expected inner path type, got: {:?}", other),
        }
      },
      other => panic!("expected reference type, got: {:?}", other),
    }
  }

  #[test]
  fn parses_reference_to_raw_pointer_types() {
    match parse_type("&*const T").unwrap() {
      Type::Reference {
        mutability,
        inner,
        ..
      } => {
        assert_eq!(mutability, Mutability::Immutable);
        match inner.as_ref() {
          Type::RawPointer { mutability, inner } => {
            assert_eq!(*mutability, Mutability::Immutable);
            match inner.as_ref() {
              Type::Path(path) => assert_ident_path(path, "T"),
              other => panic!("expected inner path type, got: {:?}", other),
            }
          },
          other => panic!("expected raw pointer inner type, got: {:?}", other),
        }
      },
      other => panic!("expected reference type, got: {:?}", other),
    }

    match parse_type("&*mut T").unwrap() {
      Type::Reference {
        mutability,
        inner,
        ..
      } => {
        assert_eq!(mutability, Mutability::Immutable);
        match inner.as_ref() {
          Type::RawPointer { mutability, inner } => {
            assert_eq!(*mutability, Mutability::Mutable);
            match inner.as_ref() {
              Type::Path(path) => assert_ident_path(path, "T"),
              other => panic!("expected inner path type, got: {:?}", other),
            }
          },
          other => panic!("expected raw pointer inner type, got: {:?}", other),
        }
      },
      other => panic!("expected reference type, got: {:?}", other),
    }
  }

  #[test]
  fn rejects_reference_to_raw_pointer_without_mutability() {
    assert_type_err("&*T");
  }

  #[test]
  fn rejects_const_reference_type() {
    assert_type_err("&const T");
  }

  #[test]
  fn parses_raw_pointer_types() {
    match parse_type("*const T").unwrap() {
      Type::RawPointer { mutability, inner } => {
        assert_eq!(mutability, Mutability::Immutable);
        match inner.as_ref() {
          Type::Path(path) => assert_ident_path(path, "T"),
          other => panic!("expected inner path type, got: {:?}", other),
        }
      },
      other => panic!("expected raw pointer type, got: {:?}", other),
    }

    match parse_type("*mut T").unwrap() {
      Type::RawPointer { mutability, inner } => {
        assert_eq!(mutability, Mutability::Mutable);
        match inner.as_ref() {
          Type::Path(path) => assert_ident_path(path, "T"),
          other => panic!("expected inner path type, got: {:?}", other),
        }
      },
      other => panic!("expected raw pointer type, got: {:?}", other),
    }
  }

  #[test]
  fn parses_qualified_path_type() {
    match parse_type("<T as Trait>::Item").unwrap() {
      Type::QualifiedPath { qself, path } => {
        match qself.self_ty.as_ref() {
          Type::Path(self_path) => assert_ident_path(self_path, "T"),
          other => panic!("expected self type path, got: {:?}", other),
        }
        match &qself.as_trait {
          Some(trait_path) => assert_ident_path(trait_path, "Trait"),
          None => panic!("expected as-trait path"),
        }
        assert_ident_path(&path, "Item");
      },
      other => panic!("expected qualified path type, got: {:?}", other),
    }
  }

  #[test]
  fn parses_qualified_path_type_without_as_trait() {
    match parse_type("<T>::Item").unwrap() {
      Type::QualifiedPath { qself, path } => {
        match qself.self_ty.as_ref() {
          Type::Path(self_path) => assert_ident_path(self_path, "T"),
          other => panic!("expected self type path, got {:?}", other),
        }
        assert!(qself.as_trait.is_none());
        assert_ident_path(&path, "Item");
      },
      other => panic!("expected qualified path type, got {:?}", other),
    }
  }

  #[test]
  fn parses_bare_function_type() {
    match parse_type("fn(i32, u8) -> bool").unwrap() {
      Type::BareFn {
        for_lifetimes,
        safety,
        abi,
        params,
        return_type,
        is_variadic,
      } => {
        assert!(for_lifetimes.is_none());
        assert_eq!(safety, Safety::Safe);
        assert!(abi.is_none());
        assert_eq!(params, vec![Type::I32, Type::U8]);
        assert_eq!(return_type, Some(Box::new(Type::Bool)));
        assert!(!is_variadic);
      },
      other => panic!("expected bare fn type, got: {:?}", other),
    }
  }

  #[test]
  fn parses_bare_function_type_with_for_lifetimes() {
    match parse_type("for<'a> fn(&'a i32)").unwrap() {
      Type::BareFn {
        for_lifetimes,
        params,
        ..
      } => {
        assert_eq!(for_lifetimes, Some(vec!["'a".to_string()]));
        assert_eq!(params.len(), 1);
        match &params[0] {
          Type::Reference { lifetime, .. } => {
            assert_eq!(lifetime, &Some("'a".to_string()));
          },
          other => panic!("expected reference param, got: {:?}", other),
        }
      },
      other => panic!("expected bare fn type, got: {:?}", other),
    }
  }

  #[test]
  fn parses_unsafe_fn_type() {
    match parse_type("unsafe fn()").unwrap() {
      Type::BareFn { safety, abi, .. } => {
        assert_eq!(safety, Safety::Unsafe);
        assert!(abi.is_none());
      },
      other => panic!("expected bare fn type, got: {:?}", other),
    }
  }

  #[test]
  fn rejects_invalid_type_starts() {
    assert_type_err("mut i32");
    assert_type_err("const i32");
    assert_type_err("*i32");
  }

  #[test]
  fn parses_parenthesized_type() {
    match parse_type("(i32)").unwrap() {
      Type::Tuple(types) => assert_eq!(types, vec![Type::I32]),
      other => panic!("expected single-element tuple type, got: {:?}", other),
    }

    match parse_type("((bool))").unwrap() {
      Type::Tuple(types) => {
        assert_eq!(types.len(), 1);
        match &types[0] {
          Type::Tuple(inner) => assert_eq!(inner, &vec![Type::Bool]),
          other => panic!("expected nested tuple type, got: {:?}", other),
        }
      },
      other => panic!("expected tuple type, got: {:?}", other),
    }
  }

  #[test]
  fn parses_never_type() {
    assert_eq!(parse_type("!").unwrap(), Type::Never);
  }

  #[test]
  #[ignore = "dyn trait types are not implemented yet"]
  fn parses_dyn_trait_type() {
    assert!(parse_type("dyn Trait").is_ok());
  }

  #[test]
  #[ignore = "impl trait types are not implemented yet"]
  fn parses_impl_trait_type() {
    assert!(parse_type("impl Trait").is_ok());
  }

  #[test]
  fn parses_unsafe_extern_fn_type() {
    match parse_type("unsafe extern \"C\" fn(i32)").unwrap() {
      Type::BareFn { safety, abi, .. } => {
        assert_eq!(safety, Safety::Unsafe);
        assert!(abi.is_some());
      },
      other => panic!("expected bare fn type, got {:?}", other),
    }
  }

  #[test]
  fn parses_variadic_fn_type() {
    match parse_type("fn(i32, ...)").unwrap() {
      Type::BareFn { is_variadic, .. } => assert!(is_variadic),
      other => panic!("expected variadic fn type, got {:?}", other),
    }
  }

  #[test]
  fn rejects_variadic_params_with_trailing_types() {
    assert_type_err("fn(..., i32)");
  }

  #[test]
  fn parses_generic_type_path() {
    match parse_type("Vec<i32>").unwrap() {
      Type::Path(path) => {
        assert_eq!(path.segments.len(), 1);
        match &path.segments[0].args {
          Some(GenericArgs::AngleBracketed { args }) => {
            assert_eq!(args.len(), 1);
            match &args[0] {
              GenericArg::Type(Type::I32) => {},
              other => panic!("expected i32 type arg, got {:?}", other),
            }
          },
          other => panic!("expected angle bracketed args, got {:?}", other),
        }
      },
      other => panic!("expected generic path type, got {:?}", other),
    }
  }

  #[test]
  fn parses_turbofish_type_path() {
    match parse_type("Vec::<i32>").unwrap() {
      Type::Path(path) => {
        assert_eq!(path.segments.len(), 1);
        match &path.segments[0].args {
          Some(GenericArgs::AngleBracketed { args }) => {
            assert_eq!(args.len(), 1);
            match &args[0] {
              GenericArg::Type(Type::I32) => {},
              other => panic!("expected i32 type arg, got {:?}", other),
            }
          },
          other => panic!("expected angle bracketed args, got {:?}", other),
        }
      },
      other => panic!("expected generic path type, got {:?}", other),
    }
  }

  #[test]
  fn parses_nested_types() {
    match parse_type("&mut Vec<Box<i32>>").unwrap() {
      Type::Reference {
        mutability,
        inner,
        ..
      } => {
        assert_eq!(mutability, Mutability::Mutable);
        match inner.as_ref() {
          Type::Path(vec_path) => match &vec_path.segments[0].args {
            Some(GenericArgs::AngleBracketed { args }) => {
              assert_eq!(args.len(), 1);
              match &args[0] {
                GenericArg::Type(Type::Path(box_path)) => {
                  match &box_path.segments[0].args {
                    Some(GenericArgs::AngleBracketed { args }) => {
                      assert_eq!(args.len(), 1);
                      match &args[0] {
                        GenericArg::Type(Type::I32) => {},
                        other => panic!("expected i32 type arg, got {:?}", other),
                      }
                    },
                    other => panic!("expected Box args, got {:?}", other),
                  }
                },
                other => panic!("expected Box type arg, got {:?}", other),
              }
            },
            other => panic!("expected Vec args, got {:?}", other),
          },
          other => panic!("expected Vec path type, got {:?}", other),
        }
      },
      other => panic!("expected reference type, got {:?}", other),
    }
  }

  #[test]
  fn parses_type_alias_decl() {
    let item = parse_item(
      "type Foo = i32",
      "type_alias_test_temp",
      ParserContext::Default,
    )
    .unwrap();

    match item {
      Item::Vis(vis_item) => match vis_item.kind {
        VisItemKind::TypeAlias(TypeAliasDecl { name, ty, .. }) => {
          assert_eq!(name.as_str(), "Foo");
          assert_eq!(ty, Type::I32);
        },
        other => panic!("expected type alias, got {:?}", other),
      },
      other => panic!("expected vis item, got {:?}", other),
    }
  }

  #[test]
  fn rejects_missing_return_type() {
    assert_type_err("fn() ->");
  }
}
