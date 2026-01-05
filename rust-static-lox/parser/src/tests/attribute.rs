#[cfg(test)]
mod attributes_tests {
  use crate::{
    ast::{
      path::{Path, PathSegmentKind},
      AttrArgs, AttrStyle, Delimiter, Expr, ExprKind, Lit, TokenTree,
    },
    parser_utils::ParserContext,
    tests::support::run_parser,
  };

  fn parse_outer_attrs(input: &str) -> Result<Vec<crate::ast::Attribute>, ()> {
    run_parser(input, "outer_attr_test_temp", |parser| {
      parser.parse_outer_attributes(ParserContext::Default)
    })
  }

  fn parse_attribute_direct(input: &str) -> Result<crate::ast::Attribute, ()> {
    run_parser(input, "attr_direct_test_temp", |parser| {
      parser.parse_attribute(ParserContext::Default)
    })
  }

  fn parse_inner_attrs(input: &str) -> Result<Vec<crate::ast::Attribute>, ()> {
    run_parser(input, "inner_attr_test_temp", |parser| {
      if !matches!(parser.current_token().kind, lexer::token::TokenKind::Pound) {
        let _ = parser.parse_expression(vec![], ParserContext::Default)?;
      }
      parser.parse_inner_attributes(ParserContext::Default)
    })
  }

  fn parse_expression_with_attrs(input: &str) -> Result<Expr, ()> {
    run_parser(input, "expr_attr_test_temp", |parser| {
      let attrs = parser.parse_outer_attributes(ParserContext::Default)?;
      parser.parse_expression(attrs, ParserContext::Default)
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

  #[test]
  fn parses_outer_attribute_path_only() {
    let attrs = parse_outer_attrs("#[test]").unwrap();
    assert_eq!(attrs.len(), 1);
    assert_eq!(attrs[0].style, AttrStyle::Outer);
    assert_ident_path(&attrs[0].input.path, "test");
    assert!(attrs[0].input.args.is_none());
  }

  #[test]
  fn parses_attribute_outer_style() {
    let attr = parse_attribute_direct("#[test]").unwrap();
    assert_eq!(attr.style, AttrStyle::Outer);
  }

  #[test]
  #[ignore = "expression attributes are not preserved yet"]
  fn parses_attribute_inner_style() {
    let attr = parse_attribute_direct("#![test]").unwrap();
    assert_eq!(attr.style, AttrStyle::Inner);
  }

  #[test]
  fn attribute_rejects_non_attribute_start() {
    assert!(parse_attribute_direct("test").is_err());
  }

  #[test]
  fn parses_outer_attribute_name_value() {
    let attrs = parse_outer_attrs("#[path = 1]").unwrap();
    assert_eq!(attrs.len(), 1);
    match &attrs[0].input.args {
      Some(AttrArgs::NameValue { value }) => match &value.kind {
        ExprKind::Literal(Lit::Integer { value, .. }) => assert_eq!(*value, 1),
        other => panic!("expected integer literal, got: {:?}", other),
      },
      other => panic!("expected name-value attr args, got: {:?}", other),
    }
  }

  #[test]
  fn parses_outer_attribute_delimited_tokens() {
    let attrs = parse_outer_attrs("#[cfg(any(unix))]").unwrap();
    assert_eq!(attrs.len(), 1);
    match &attrs[0].input.args {
      Some(AttrArgs::Delimited { delimiter, tokens }) => {
        assert_eq!(*delimiter, Delimiter::Paren);
        assert_eq!(tokens.len(), 2);
        match &tokens[0] {
          TokenTree::Token(name) => assert_eq!(name, "any"),
          other => panic!("expected token tree name, got: {:?}", other),
        }
        match &tokens[1] {
          TokenTree::Delimited { delimiter, tokens } => {
            assert_eq!(*delimiter, Delimiter::Paren);
            assert_eq!(tokens, &vec![TokenTree::Token("unix".to_string())]);
          },
          other => panic!("expected nested token tree, got: {:?}", other),
        }
      },
      other => panic!("expected delimited attr args, got: {:?}", other),
    }
  }

  #[test]
  fn parses_outer_attribute_bracket_tokens() {
    let attrs = parse_outer_attrs("#[cfg[unix]]").unwrap();
    assert_eq!(attrs.len(), 1);
    match &attrs[0].input.args {
      Some(AttrArgs::Delimited { delimiter, tokens }) => {
        assert_eq!(*delimiter, Delimiter::Bracket);
        assert_eq!(tokens, &vec![TokenTree::Token("unix".to_string())]);
      },
      other => panic!("expected delimited attr args, got: {:?}", other),
    }
  }

  #[test]
  fn parses_outer_attribute_brace_tokens() {
    let attrs = parse_outer_attrs("#[cfg{unix}]").unwrap();
    assert_eq!(attrs.len(), 1);
    match &attrs[0].input.args {
      Some(AttrArgs::Delimited { delimiter, tokens }) => {
        assert_eq!(*delimiter, Delimiter::Brace);
        assert_eq!(tokens, &vec![TokenTree::Token("unix".to_string())]);
      },
      other => panic!("expected delimited attr args, got: {:?}", other),
    }
  }

  #[test]
  fn delim_token_tree_rejects_non_delimiter_start() {
    let result = run_parser("test", "attr_tree_error_temp", |parser| {
      parser.parse_delim_token_tree()
    });
    assert!(result.is_err());
  }

  #[test]
  fn token_tree_helpers_default_for_token_variant() {
    let tree = TokenTree::Token("x".to_string());
    assert_eq!(tree.delimiter(), Delimiter::Paren);
    assert!(tree.tokens().is_empty());
  }

  #[test]
  fn parses_multiple_outer_attributes() {
    let attrs = parse_outer_attrs("#[a] #[b]").unwrap();
    assert_eq!(attrs.len(), 2);
  }

  #[test]
  fn parses_inner_attribute() {
    let attrs = parse_inner_attrs("{} #![no_std]").unwrap();
    assert_eq!(attrs.len(), 1);
    assert_eq!(attrs[0].style, AttrStyle::Inner);
    assert_ident_path(&attrs[0].input.path, "no_std");
  }

  #[test]
  fn inner_attributes_reject_outer_syntax() {
    assert!(parse_inner_attrs("{} #[attr]").is_err());
  }

  #[test]
  #[ignore = "expression attributes are not preserved yet"]
  fn attributes_on_literal_expression() {
    let expr = parse_expression_with_attrs("#[attr] 1").unwrap();
    assert_eq!(expr.attributes.len(), 1);
  }

  #[test]
  #[ignore = "expression attributes are not preserved yet"]
  fn attributes_on_binary_expression() {
    let expr = parse_expression_with_attrs("#[attr] 1 + 2").unwrap();
    assert_eq!(expr.attributes.len(), 1);
  }

  #[test]
  #[ignore = "expression attributes are not preserved yet"]
  fn attributes_on_tuple_expression() {
    let expr = parse_expression_with_attrs("#[attr] (1, 2)").unwrap();
    assert_eq!(expr.attributes.len(), 1);
  }
}
