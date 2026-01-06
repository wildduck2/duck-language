#[cfg(test)]
mod macros_tests {
  use crate::{
    ast::{Delimiter, ExprKind, Ident, Item, MacroItemKind, MacroRule, RepeatKind, Stmt, TokenTree},
    parser_utils::ParserContext,
    tests::support::{parse_expression, parse_item, run_parser, simple_path, simplify_path},
  };

  fn parse_expr(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "macro_expr_test_temp", ParserContext::Default)
  }

  fn parse_stmt(input: &str) -> Result<Stmt, ()> {
    run_parser(input, "macro_stmt_test_temp", |parser| {
      parser.parse_stmt(ParserContext::Default)
    })
  }

  fn parse_item_decl(input: &str) -> Result<Item, ()> {
    parse_item(input, "macro_item_test_temp", ParserContext::Default)
  }

  fn parse_rules(input: &str) -> Result<Vec<MacroRule>, ()> {
    parse_item_decl(input).and_then(|item| match item {
      Item::Macro(mac) => match mac.kind {
        MacroItemKind::MacroRules(decl) => Ok(decl.rules),
        other => panic!("expected macro_rules item, got: {:?}", other),
      },
      other => panic!("expected macro item, got: {:?}", other),
    })
  }

  #[test]
  fn parses_macro_invocation_paren() {
    let expr = parse_expr("foo!(x, 1, true)").unwrap();
    match expr {
      ExprKind::Macro { mac } => {
        assert_eq!(mac.delimiter, Delimiter::Paren);
        assert_eq!(
          mac.tokens,
          vec![
            TokenTree::Token("x".to_string()),
            TokenTree::Token(",".to_string()),
            TokenTree::Token("1".to_string()),
            TokenTree::Token(",".to_string()),
            TokenTree::Token("true".to_string()),
          ]
        );
      },
      other => panic!("expected macro expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro_invocation_bracket() {
    let expr = parse_expr("foo![x]").unwrap();
    match expr {
      ExprKind::Macro { mac } => {
        assert_eq!(mac.delimiter, Delimiter::Bracket);
        assert_eq!(mac.tokens, vec![TokenTree::Token("x".to_string())]);
      },
      other => panic!("expected macro expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro_invocation_brace() {
    let expr = parse_expr("foo!{x}").unwrap();
    match expr {
      ExprKind::Macro { mac } => {
        assert_eq!(mac.delimiter, Delimiter::Brace);
        assert_eq!(mac.tokens, vec![TokenTree::Token("x".to_string())]);
      },
      other => panic!("expected macro expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_nested_macro_token_trees() {
    let expr = parse_expr("foo!({x}, [y])").unwrap();
    match expr {
      ExprKind::Macro { mac } => {
        assert_eq!(mac.tokens.len(), 2);
        match &mac.tokens[0] {
          TokenTree::Delimited { delimiter, tokens } => {
            assert_eq!(*delimiter, Delimiter::Brace);
            assert_eq!(tokens, &vec![TokenTree::Token("x".to_string())]);
          },
          other => panic!("expected delimited token, got: {:?}", other),
        }
        match &mac.tokens[1] {
          TokenTree::Delimited { delimiter, tokens } => {
            assert_eq!(*delimiter, Delimiter::Bracket);
            assert_eq!(tokens, &vec![TokenTree::Token("y".to_string())]);
          },
          other => panic!("expected delimited token, got: {:?}", other),
        }
      },
      other => panic!("expected macro expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_nested_macro_token_trees_without_comma() {
    let expr = parse_expr("foo!({x}[y])").unwrap();
    match expr {
      ExprKind::Macro { mac } => {
        assert_eq!(mac.tokens.len(), 2);
        match &mac.tokens[0] {
          TokenTree::Delimited { delimiter, tokens } => {
            assert_eq!(*delimiter, Delimiter::Brace);
            assert_eq!(tokens, &vec![TokenTree::Token("x".to_string())]);
          },
          other => panic!("expected delimited token, got: {:?}", other),
        }
        match &mac.tokens[1] {
          TokenTree::Delimited { delimiter, tokens } => {
            assert_eq!(*delimiter, Delimiter::Bracket);
            assert_eq!(tokens, &vec![TokenTree::Token("y".to_string())]);
          },
          other => panic!("expected delimited token, got: {:?}", other),
        }
      },
      other => panic!("expected macro expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro_invocation_statement() {
    let stmt = parse_stmt("foo!(x)").unwrap();
    match stmt {
      Stmt::Macro { mac, .. } => {
        assert_eq!(mac.delimiter, Delimiter::Paren);
        assert_eq!(mac.tokens, vec![TokenTree::Token("x".to_string())]);
      },
      other => panic!("expected macro statement, got: {:?}", other),
    }
  }

  #[test]
  fn rejects_missing_macro_delimiter() {
    assert!(parse_expr("foo!x").is_err());
  }

  #[test]
  fn rejects_unclosed_macro_invocation() {
    assert!(parse_expr("foo!(x").is_err());
  }

  #[test]
  fn rejects_mismatched_macro_invocation_delimiter() {
    assert!(parse_expr("foo!(x]").is_err());
  }

  #[test]
  fn parses_macro_rules_metavar() {
    let rules = parse_rules("macro_rules! m { ($x:ident) => {} }").unwrap();
    assert_eq!(rules.len(), 1);
    match &rules[0].matcher {
      TokenTree::Delimited { delimiter, tokens } => {
        assert_eq!(*delimiter, Delimiter::Paren);
        assert_eq!(
          tokens,
          &vec![TokenTree::MetaVar {
            name: Ident::Name("x".to_string()),
            frag: Ident::Name("ident".to_string()),
          }]
        );
      },
      other => panic!("expected delimited matcher, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro_rules_repetition_ops() {
    let rules = parse_rules(
      "macro_rules! m { ($(x)*) => {}; ($(x)+) => {}; ($(x)?) => {}; ($(x),*) => {}; ($(x),+) => {}; ($(x),?) => {}; }",
    )
    .unwrap();
    assert_eq!(rules.len(), 6);

    match &rules[0].matcher {
      TokenTree::Delimited { tokens, .. } => match &tokens[0] {
        TokenTree::Repeat {
          tokens,
          separator,
          kind,
        } => {
          assert_eq!(*kind, RepeatKind::ZeroOrMore);
          assert_eq!(separator, &None);
          assert_eq!(tokens, &vec![TokenTree::Token("x".to_string())]);
        },
        other => panic!("expected repeat token tree, got: {:?}", other),
      },
      other => panic!("expected delimited matcher, got: {:?}", other),
    }

    match &rules[1].matcher {
      TokenTree::Delimited { tokens, .. } => match &tokens[0] {
        TokenTree::Repeat { kind, .. } => assert_eq!(*kind, RepeatKind::OneOrMore),
        other => panic!("expected repeat token tree, got: {:?}", other),
      },
      other => panic!("expected delimited matcher, got: {:?}", other),
    }

    match &rules[2].matcher {
      TokenTree::Delimited { tokens, .. } => match &tokens[0] {
        TokenTree::Repeat { kind, .. } => assert_eq!(*kind, RepeatKind::ZeroOrOne),
        other => panic!("expected repeat token tree, got: {:?}", other),
      },
      other => panic!("expected delimited matcher, got: {:?}", other),
    }

    match &rules[3].matcher {
      TokenTree::Delimited { tokens, .. } => match &tokens[0] {
        TokenTree::Repeat {
          separator, kind, ..
        } => {
          assert_eq!(*kind, RepeatKind::ZeroOrMore);
          assert_eq!(separator.as_deref(), Some(","));
        },
        other => panic!("expected repeat token tree, got: {:?}", other),
      },
      other => panic!("expected delimited matcher, got: {:?}", other),
    }

    match &rules[4].matcher {
      TokenTree::Delimited { tokens, .. } => match &tokens[0] {
        TokenTree::Repeat {
          separator, kind, ..
        } => {
          assert_eq!(*kind, RepeatKind::OneOrMore);
          assert_eq!(separator.as_deref(), Some(","));
        },
        other => panic!("expected repeat token tree, got: {:?}", other),
      },
      other => panic!("expected delimited matcher, got: {:?}", other),
    }

    match &rules[5].matcher {
      TokenTree::Delimited { tokens, .. } => match &tokens[0] {
        TokenTree::Repeat {
          separator, kind, ..
        } => {
          assert_eq!(*kind, RepeatKind::ZeroOrOne);
          assert_eq!(separator.as_deref(), Some(","));
        },
        other => panic!("expected repeat token tree, got: {:?}", other),
      },
      other => panic!("expected delimited matcher, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro_rules_item() {
    let item = parse_item_decl("macro_rules! m { () => {} }").unwrap();
    match item {
      Item::Macro(mac) => match mac.kind {
        MacroItemKind::MacroRules(_) => {},
        other => panic!("expected macro_rules item, got: {:?}", other),
      },
      other => panic!("expected macro item, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro_rules_item_with_attribute() {
    let item = parse_item_decl("#[doc = \"mac\"] macro_rules! m { () => {} }").unwrap();
    match item {
      Item::Macro(mac) => assert_eq!(mac.attributes.len(), 1),
      other => panic!("expected macro item, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro_rules_body_bracket() {
    let rules = parse_rules("macro_rules! m [ () => {} ]").unwrap();
    assert_eq!(rules.len(), 1);
  }

  #[test]
  fn parses_macro_rules_body_paren_with_semi() {
    let rules = parse_rules("macro_rules! m ( () => {} );").unwrap();
    assert_eq!(rules.len(), 1);
  }

  #[test]
  fn rejects_macro_rules_body_paren_missing_semi() {
    assert!(parse_item_decl("macro_rules! m ( () => {} )").is_err());
  }

  #[test]
  fn rejects_macro_rules_missing_body_delimiter() {
    assert!(parse_item_decl("macro_rules! m x").is_err());
  }

  #[test]
  fn parses_macro_rules_matcher_delimiters() {
    let rules = parse_rules("macro_rules! m { [] => {}; {} => {}; () => {} }").unwrap();
    assert_eq!(rules.len(), 3);

    match &rules[0].matcher {
      TokenTree::Delimited { delimiter, .. } => assert_eq!(*delimiter, Delimiter::Bracket),
      other => panic!("expected bracket matcher, got: {:?}", other),
    }

    match &rules[1].matcher {
      TokenTree::Delimited { delimiter, .. } => assert_eq!(*delimiter, Delimiter::Brace),
      other => panic!("expected brace matcher, got: {:?}", other),
    }

    match &rules[2].matcher {
      TokenTree::Delimited { delimiter, .. } => assert_eq!(*delimiter, Delimiter::Paren),
      other => panic!("expected paren matcher, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro_rules_nested_matcher() {
    let rules = parse_rules("macro_rules! m { ((x)) => {} }").unwrap();
    match &rules[0].matcher {
      TokenTree::Delimited { tokens, .. } => match &tokens[0] {
        TokenTree::Delimited { delimiter, tokens } => {
          assert_eq!(*delimiter, Delimiter::Paren);
          assert_eq!(tokens, &vec![TokenTree::Token("x".to_string())]);
        },
        other => panic!("expected nested matcher, got: {:?}", other),
      },
      other => panic!("expected delimited matcher, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro_rules_transcriber_nested_delims() {
    let rules = parse_rules("macro_rules! m { () => { {x}[y](z) } }").unwrap();
    match &rules[0].transcriber {
      TokenTree::Delimited { delimiter, tokens } => {
        assert_eq!(*delimiter, Delimiter::Brace);
        assert_eq!(tokens.len(), 3);
        match &tokens[0] {
          TokenTree::Delimited { delimiter, tokens } => {
            assert_eq!(*delimiter, Delimiter::Brace);
            assert_eq!(tokens, &vec![TokenTree::Token("x".to_string())]);
          },
          other => panic!("expected nested brace, got: {:?}", other),
        }
        match &tokens[1] {
          TokenTree::Delimited { delimiter, tokens } => {
            assert_eq!(*delimiter, Delimiter::Bracket);
            assert_eq!(tokens, &vec![TokenTree::Token("y".to_string())]);
          },
          other => panic!("expected nested bracket, got: {:?}", other),
        }
        match &tokens[2] {
          TokenTree::Delimited { delimiter, tokens } => {
            assert_eq!(*delimiter, Delimiter::Paren);
            assert_eq!(tokens, &vec![TokenTree::Token("z".to_string())]);
          },
          other => panic!("expected nested paren, got: {:?}", other),
        }
      },
      other => panic!("expected delimited transcriber, got: {:?}", other),
    }
  }

  #[test]
  fn rejects_macro_rules_invalid_matcher_start() {
    assert!(parse_item_decl("macro_rules! m { x => {} }").is_err());
  }

  #[test]
  fn rejects_macro_rules_invalid_match_token() {
    assert!(parse_item_decl("macro_rules! m { [) => {} ] }").is_err());
  }

  #[test]
  fn rejects_macro_rules_invalid_frag_spec() {
    assert!(parse_item_decl("macro_rules! m { ($x:notreal) => {} }").is_err());
  }

  #[test]
  fn rejects_macro_rules_metavar_missing_spec() {
    assert!(parse_item_decl("macro_rules! m { ($) => {} }").is_err());
  }

  #[test]
  fn rejects_macro_rules_repetition_empty() {
    assert!(parse_item_decl("macro_rules! m { ($()*) => {} }").is_err());
  }

  #[test]
  fn rejects_macro_rules_repetition_delim_separator() {
    assert!(parse_item_decl("macro_rules! m { ($(x)()*) => {} }").is_err());
  }

  #[test]
  fn rejects_macro_rules_repetition_missing_op() {
    assert!(parse_item_decl("macro_rules! m { ($(x),) => {} }").is_err());
  }

  #[test]
  fn rejects_macro_rules_transcriber_without_delim() {
    assert!(parse_item_decl("macro_rules! m { () => x }").is_err());
  }

  #[test]
  fn parses_macro_invocation_item() {
    let item = parse_item_decl("foo!(x);").unwrap();
    match item {
      Item::Macro(mac) => match mac.kind {
        MacroItemKind::Invocation(invoc) => {
          assert_eq!(simplify_path(&invoc.path), simple_path(["foo"]));
          assert_eq!(invoc.delimiter, Delimiter::Paren);
          assert_eq!(invoc.tokens, vec![TokenTree::Token("x".to_string())]);
        },
        other => panic!("expected macro invocation item, got: {:?}", other),
      },
      other => panic!("expected macro item, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro_invocation_item_without_semicolon() {
    let item = parse_item_decl("foo! { x }").unwrap();
    match item {
      Item::Macro(mac) => match mac.kind {
        MacroItemKind::Invocation(invoc) => {
          assert_eq!(simplify_path(&invoc.path), simple_path(["foo"]));
          assert_eq!(invoc.delimiter, Delimiter::Brace);
          assert_eq!(invoc.tokens, vec![TokenTree::Token("x".to_string())]);
        },
        other => panic!("expected macro invocation item, got: {:?}", other),
      },
      other => panic!("expected macro item, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro_invocation_item_with_attribute() {
    let item = parse_item_decl("#[doc = \"mac\"] foo!(x);").unwrap();
    match item {
      Item::Macro(mac) => assert_eq!(mac.attributes.len(), 1),
      other => panic!("expected macro item, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro2_item() {
    let item = parse_item_decl("macro m(x, y,) { }").unwrap();
    match item {
      Item::Macro(mac) => match mac.kind {
        MacroItemKind::Macro2(decl) => {
          assert_eq!(decl.name, Ident::Name("m".to_string()));
          assert_eq!(decl.params, vec!["x".to_string(), "y".to_string()]);
          match decl.body {
            TokenTree::Delimited { delimiter, tokens } => {
              assert_eq!(delimiter, Delimiter::Brace);
              assert!(tokens.is_empty());
            },
            other => panic!("expected macro2 body token tree, got: {:?}", other),
          }
        },
        other => panic!("expected macro2 item, got: {:?}", other),
      },
      other => panic!("expected macro item, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro2_item_with_attribute() {
    let item = parse_item_decl("#[doc = \"mac\"] macro m(x,) { }").unwrap();
    match item {
      Item::Macro(mac) => assert_eq!(mac.attributes.len(), 1),
      other => panic!("expected macro item, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro2_item_no_params() {
    let item = parse_item_decl("macro m() { }").unwrap();
    match item {
      Item::Macro(mac) => match mac.kind {
        MacroItemKind::Macro2(decl) => assert!(decl.params.is_empty()),
        other => panic!("expected macro2 item, got: {:?}", other),
      },
      other => panic!("expected macro item, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro2_item_single_param() {
    let item = parse_item_decl("macro m(x) { }").unwrap();
    match item {
      Item::Macro(mac) => match mac.kind {
        MacroItemKind::Macro2(decl) => assert_eq!(decl.params, vec!["x".to_string()]),
        other => panic!("expected macro2 item, got: {:?}", other),
      },
      other => panic!("expected macro item, got: {:?}", other),
    }
  }

  #[test]
  fn rejects_macro2_body_without_delim() {
    assert!(parse_item_decl("macro m() x").is_err());
  }
}
