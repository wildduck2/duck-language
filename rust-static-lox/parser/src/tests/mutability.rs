#[cfg(test)]
mod mutability_tests {
  use crate::{
    ast::{Mutability, Type},
    parser_utils::ParserContext,
    tests::support::run_parser,
  };

  fn parse_mutability(input: &str) -> Result<Mutability, ()> {
    run_parser(input, "mutability_parse_test_temp", |parser| {
      parser.parse_mutability()
    })
  }

  fn parse_type(input: &str) -> Result<Type, ()> {
    run_parser(input, "mutability_type_test_temp", |parser| {
      parser.parse_type(ParserContext::Type)
    })
  }

  fn assert_type_err(input: &str) {
    assert!(parse_type(input).is_err(), "expected error for {input:?}");
  }

  #[test]
  fn parses_mut_keyword() {
    assert_eq!(parse_mutability("mut").unwrap(), Mutability::Mutable);
  }

  #[test]
  fn parses_const_keyword_as_immutable() {
    assert_eq!(parse_mutability("const").unwrap(), Mutability::Immutable);
  }

  #[test]
  fn missing_mutability_is_immutable() {
    assert_eq!(parse_mutability("").unwrap(), Mutability::Immutable);
  }

  #[test]
  fn parses_mutable_reference_type() {
    let ty = parse_type("&mut i32").unwrap();
    match ty {
      Type::Reference {
        lifetime,
        mutability,
        inner,
      } => {
        assert_eq!(lifetime, None);
        assert_eq!(mutability, Mutability::Mutable);
        assert_eq!(*inner, Type::I32);
      },
      other => panic!("expected mutable reference type, got: {:?}", other),
    }
  }

  #[test]
  fn parses_immutable_reference_type() {
    let ty = parse_type("&i32").unwrap();
    match ty {
      Type::Reference {
        lifetime,
        mutability,
        inner,
      } => {
        assert_eq!(lifetime, None);
        assert_eq!(mutability, Mutability::Immutable);
        assert_eq!(*inner, Type::I32);
      },
      other => panic!("expected immutable reference type, got: {:?}", other),
    }
  }

  #[test]
  fn parses_raw_pointer_const() {
    let ty = parse_type("*const i32").unwrap();
    match ty {
      Type::RawPointer { mutability, inner } => {
        assert_eq!(mutability, Mutability::Immutable);
        assert_eq!(*inner, Type::I32);
      },
      other => panic!("expected raw const pointer type, got: {:?}", other),
    }
  }

  #[test]
  fn parses_raw_pointer_mut() {
    let ty = parse_type("*mut i32").unwrap();
    match ty {
      Type::RawPointer { mutability, inner } => {
        assert_eq!(mutability, Mutability::Mutable);
        assert_eq!(*inner, Type::I32);
      },
      other => panic!("expected raw mut pointer type, got: {:?}", other),
    }
  }

  #[test]
  fn rejects_const_reference_type() {
    assert_type_err("&const i32");
  }

  #[test]
  fn rejects_raw_pointer_without_qualifier() {
    assert_type_err("*i32");
  }

  #[test]
  fn parses_mutable_reference_with_lifetime() {
    let ty = parse_type("&'a mut i32").unwrap();
    match ty {
      Type::Reference {
        lifetime,
        mutability,
        inner,
      } => {
        assert_eq!(lifetime, Some("'a".to_string()));
        assert_eq!(mutability, Mutability::Mutable);
        assert_eq!(*inner, Type::I32);
      },
      other => panic!("expected reference type, got {:?}", other),
    }
  }

  #[test]
  fn parses_immutable_reference_with_lifetime() {
    let ty = parse_type("&'a i32").unwrap();
    match ty {
      Type::Reference {
        lifetime,
        mutability,
        inner,
      } => {
        assert_eq!(lifetime, Some("'a".to_string()));
        assert_eq!(mutability, Mutability::Immutable);
        assert_eq!(*inner, Type::I32);
      },
      other => panic!("expected reference type, got {:?}", other),
    }
  }

  #[test]
  fn rejects_raw_pointer_with_lifetime() {
    assert_type_err("*const 'a i32");
    assert_type_err("*mut 'a i32");
  }

  #[test]
  fn rejects_double_mutability() {
    assert_type_err("&mut mut i32");
    assert_type_err("*mut mut i32");
  }

  #[test]
  fn rejects_bare_mut_type() {
    assert_type_err("mut i32");
  }

  #[test]
  fn rejects_const_as_type() {
    assert_type_err("const i32");
  }

  #[test]
  fn parses_nested_pointer_and_reference() {
    assert!(parse_type("&mut *const i32").is_ok());
    assert!(parse_type("*mut &i32").is_ok());
  }
}
